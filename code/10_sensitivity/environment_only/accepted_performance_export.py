"""Export evaluation values for projections in a frozen paired manifest.

The remote reader uses base R and reads existing files only. Calibration,
validation and external evaluation remain separate; candidate-pool values are
never substituted for an unidentified accepted projection.
"""

import argparse
import csv
import hashlib
import io
import json
from pathlib import Path
import subprocess


REMOTE_R = r'''
options(warn=1)
manifest <- read.csv(text=MANIFEST_TEXT, stringsAsFactors=FALSE, check.names=FALSE)
resolve <- function(x) {
 x <- gsub("<FULL_SDM_ROOT>", FULL_ROOT, x, fixed=TRUE)
 gsub("<ENV_SOURCE_ROOT>", ENV_ROOT, x, fixed=TRUE)
}
scalar_json <- function(x) {
 if(length(x)==0 || is.na(x[1])) return("null")
 if(is.numeric(x)) return(format(x[1],digits=17,scientific=FALSE,trim=TRUE))
 x <- enc2utf8(as.character(x[1]))
 x <- gsub("\\", "\\\\", x, fixed=TRUE)
 x <- gsub('"', '\\"', x, fixed=TRUE)
 x <- gsub("\n", "\\n", x, fixed=TRUE)
 x <- gsub("\r", "\\r", x, fixed=TRUE)
 x <- gsub("\t", "\\t", x, fixed=TRUE)
 paste0('"',x,'"')
}
row_json <- function(x) paste0("{",paste(vapply(names(x),function(n)
 paste0(scalar_json(n),":",scalar_json(x[[n]])),character(1)),collapse=","),"}")
loaded_value <- function(path) {
 if(grepl("[.]rds$",path,ignore.case=TRUE)) return(readRDS(path))
 e <- new.env(parent=emptyenv()); load(path,envir=e)
 if(length(ls(e))!=1) stop("Serialized file has multiple objects")
 e[[ls(e)[1]]]
}
stored_frame <- function(x) {
 if(!isS4(x) && is.data.frame(x)) return(x)
 a <- attributes(x)
 if(is.data.frame(a$val) && isTRUE(a$inMemory)) return(a$val)
 stop("Evaluation data are not an in-memory data frame")
}
result <- list(); sources <- list(); index <- 0
for(i in seq_len(nrow(manifest))) {
 for(group in c("FULL","ENV")) {
  if("export_full" %in% names(manifest) &&
     !isTRUE(manifest[[paste0("export_",tolower(group))]][i])) next
  projection <- resolve(manifest[[paste0(tolower(group),"_projection")]][i])
  model_path <- resolve(manifest[[paste0(tolower(group),"_model")]][i])
  selected <- ""; unit <- ""; tier <- ""; source <- ""; state <- "PARTIAL"
  error <- ""; ev <- NULL; selected_count <- NA_integer_; method <- ""
  tryCatch({
   if(group=="FULL" && !file.exists(file.path(dirname(projection),"writer_contract.rds"))) {
    name <- tools::file_path_sans_ext(basename(projection))
    if(grepl("[.]grd$",projection,ignore.case=TRUE)) {
     header <- readLines(projection,warn=FALSE)
     band <- grep("^nbands=",header,value=TRUE)
     layer <- grep("^layername=",header,value=TRUE)
     if(length(band)==1 && band=="nbands=1" && length(layer)==1)
      name <- sub("^layername=","",layer)
    }
    if(grepl("_EM(mean|median|wmean|cv|ca|ci)",name)) {
     selected <- name; unit <- "accepted_ensemble"
     source <- sub("\\.models\\.out$",".ensemble.models.out",model_path)
     obj <- loaded_value(source)
     ev <- stored_frame(attributes(obj)$models.evaluation)
     method <- "projection_filename_or_single_band_header_exact_ensemble_match"
     if(!selected %in% ev$full.name && grepl("_ensemble_EM(mean|median)",name)) {
      declared <- sub("^.*_ensemble_(EMmean|EMmedian).*$","\\1",name)
      choices <- unique(ev$full.name[ev$algo==declared & ev$metric.eval %in% c("TSS","ROC")])
      if(length(choices)==1) {
       selected <- choices[1]
       method <- "projection_declared_algorithm_unique_serialized_ensemble_unit"
      }
     }
    } else {
     selected <- sub("^proj_fallback_projection_","",name)
     unit <- "accepted_single_model"
     species_root <- dirname(model_path)
     candidates <- list.files(file.path(species_root,".BIOMOD_DATA"),
       pattern="^models\\.evaluation$",recursive=TRUE,full.names=TRUE)
     matches <- list()
     for(p in candidates) {
      tab <- stored_frame(loaded_value(p))
      if("full.name" %in% names(tab) && selected %in% tab$full.name)
       matches[[p]] <- tab
     }
     if(length(matches)!=1) stop("Accepted single model has no unique evaluation source")
     source <- names(matches)[1]; ev <- matches[[1]]
     method <- "projection_filename_exact_single_match"
    }
    selected_count <- if(unit=="accepted_single_model") 1L else NA_integer_
   } else {
    parent <- dirname(projection)
    selection_path <- file.path(parent,"selection.csv")
    selection <- read.csv(selection_path,stringsAsFactors=FALSE)
    selected_count <- nrow(selection)
    writer_path <- file.path(parent,"writer_contract.rds")
    if(!file.exists(writer_path)) stop("Missing writer contract")
    writer <- readRDS(writer_path)
    chosen <- writer$chosen_ensemble
    projected <- writer$projection$selected
    if(is.character(chosen) && length(chosen)==1 && nzchar(chosen)) {
     selected <- chosen; unit <- "accepted_ensemble"
     source <- file.path(parent,"ensemble_evaluation.csv")
     if(!file.exists(source)) stop("Chosen ensemble evaluation CSV missing")
     ev <- read.csv(source,stringsAsFactors=FALSE,check.names=FALSE)
     method <- "writer_contract_chosen_ensemble_exact_match"
    } else if(is.null(chosen) && nrow(selection)==1 &&
              length(projected)==1 && identical(selection$model,projected)) {
     selected <- selection$model[1]; unit <- "accepted_single_model"
     tier <- selection$tier[1]
     source <- file.path(parent,"evaluation.csv")
     ev <- read.csv(source,stringsAsFactors=FALSE,check.names=FALSE)
     method <- "writer_contract_and_selection_exact_single_match"
    } else {
     source <- file.path(parent,"evaluation.csv")
     if(is.null(chosen) && setequal(selection$model,projected)) {
      selected <- paste(selection$model,collapse=";")
      unit <- "selected_composite_without_projection_evaluation"
      method <- "writer_contract_selected_members_exact_match"
     }
     stop("Selected composite has no projection-level evaluation; member metrics not substituted")
    }
   }
   if(!all(c("full.name","metric.eval") %in% names(ev)))
    stop("Evaluation schema lacks exact model and metric identifiers")
   ev <- ev[ev$full.name==selected & ev$metric.eval %in% c("TSS","ROC"),,drop=FALSE]
   if(nrow(ev)!=2 || anyDuplicated(ev$metric.eval))
    stop("Expected exactly one TSS and one ROC row for accepted model")
   state <- "EXACT_SELECTED_UNIT"
  },error=function(e) {error <<- conditionMessage(e)})
  md5 <- if(nzchar(source) && file.exists(source)) unname(tools::md5sum(source)) else ""
  for(metric in c("TSS","ROC")) for(stage in c("calibration","validation","evaluation")) {
   value <- NA_real_
   if(state=="EXACT_SELECTED_UNIT" && stage %in% names(ev))
    value <- as.numeric(ev[ev$metric.eval==metric,stage][1])
   index <- index+1L
   result[[index]] <- list(species=manifest$species[i],group=group,
    comparison_role=if(group=="ENV") "accepted_environment_only_refit" else if(grepl("/CLEAN_FULL/",projection,fixed=TRUE)) "accepted_clean_full_refit" else "original_accepted_full",
    accepted_projection=projection,selected_model=selected,projection_unit=unit,
    selection_tier=tier,selected_candidate_count=selected_count,
    metric=if(metric=="ROC") "ROC_AUC" else metric,original_metric=metric,
    evaluation_stage=stage,value=value,unit_mapping_state=state,
    metric_availability=if(is.na(value)) "MISSING" else "AVAILABLE",
    source_evaluation_file=source,source_md5=md5,mapping_method=method,
    unresolved_reason=error,clean_full_equivalence=if(grepl("/CLEAN_FULL/",projection,fixed=TRUE)) "EXPLICIT_CLEAN_FULL_PROJECTION" else "NOT_ESTABLISHED")
  }
 }
}
cat('[',paste(vapply(result,row_json,character(1)),collapse=','),']',sep='')
'''


def r_literal(value):
    return json.dumps(str(value), ensure_ascii=False)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--manifest", required=True, type=Path)
    parser.add_argument("--full-root", required=True)
    parser.add_argument("--env-root", required=True)
    parser.add_argument("--host", required=True)
    parser.add_argument("--rscript", required=True)
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--provenance", required=True, type=Path)
    parser.add_argument("--previous-export", type=Path,
                        help="Reuse exact selected units and retry only unresolved metadata.")
    args = parser.parse_args()
    if args.output.exists() or args.provenance.exists():
        raise SystemExit("Output already exists; refusing to overwrite.")
    data = args.manifest.read_bytes()
    text = data.decode("utf-8-sig")
    roster = list(csv.DictReader(io.StringIO(text)))
    species = [row["species"] for row in roster]
    if len(roster) != 684 or len(set(species)) != 684:
        raise SystemExit("Expected the fixed 684-species paired manifest.")
    if any(row["full_status"] != "ACCEPTED" or row["env_status"] != "ACCEPTED" for row in roster):
        raise SystemExit("Manifest includes unaccepted projections.")
    retained = []
    if args.previous_export:
        with args.previous_export.open(encoding="utf-8-sig", newline="") as handle:
            previous = list(csv.DictReader(handle))
        if set(row["species"] for row in previous) != set(species):
            raise SystemExit("Previous export has a different species roster.")
        retained = [row for row in previous if row["unit_mapping_state"] == "EXACT_SELECTED_UNIT"]
        exact = {(row["species"], row["group"]) for row in retained}
        for row in roster:
            for group in ["FULL", "ENV"]:
                row["export_" + group.lower()] = "FALSE" if (row["species"], group) in exact else "TRUE"
        stream = io.StringIO()
        writer = csv.DictWriter(stream, fieldnames=list(roster[0]))
        writer.writeheader()
        writer.writerows(roster)
        text = stream.getvalue()
    script = "\n".join([
        "MANIFEST_TEXT <- " + r_literal(text),
        "FULL_ROOT <- " + r_literal(args.full_root),
        "ENV_ROOT <- " + r_literal(args.env_root), REMOTE_R])
    process = subprocess.run(
        ["ssh", "-o", "BatchMode=yes", args.host, args.rscript, "-"],
        input=script.encode("utf-8"), capture_output=True, check=False)
    if process.returncode:
        raise SystemExit(process.stderr.decode("utf-8", errors="replace"))
    rows = retained + json.loads(process.stdout)
    if len(rows) != 684 * 2 * 2 * 3 or set(row["species"] for row in rows) != set(species):
        raise SystemExit("Remote export changed the frozen species roster.")
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.provenance.parent.mkdir(parents=True, exist_ok=True)
    with args.output.open("x", encoding="utf-8-sig", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)
    unit_states = {}
    availability = {}
    for row in rows:
        key = (row["species"], row["group"])
        unit_states[key] = row["unit_mapping_state"]
        label = "|".join([row["group"], row["metric"], row["evaluation_stage"], row["metric_availability"]])
        availability[label] = availability.get(label, 0) + 1
    metadata = {
        "manifest_sha256": hashlib.sha256(data).hexdigest(),
        "species_count": len(species), "output_rows": len(rows),
        "exact_selected_units": sum(state == "EXACT_SELECTED_UNIT" for state in unit_states.values()),
        "partial_selected_units": sum(state != "EXACT_SELECTED_UNIT" for state in unit_states.values()),
        "metric_availability_counts": availability,
        "comparison_scope": "Manifest-defined accepted FULL versus ENV, preserving explicit CLEAN_FULL paths; historical FULL is not substituted for clean FULL",
        "remote_reads_only": True, "model_fitting_performed": False,
        "source_checksums": "MD5 of exact evaluation source files; manifest SHA-256",
        "remote_stderr": process.stderr.decode("utf-8", errors="replace"),
        "output_sha256": hashlib.sha256(args.output.read_bytes()).hexdigest(),
    }
    with args.provenance.open("x", encoding="utf-8") as handle:
        json.dump(metadata, handle, indent=2)
    print(json.dumps({key: metadata[key] for key in ["species_count", "output_rows", "exact_selected_units", "partial_selected_units"]}))


if __name__ == "__main__":
    main()
