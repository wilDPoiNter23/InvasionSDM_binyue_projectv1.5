#!/usr/bin/env Rscript
# Execute configured downstream analyses and record their input/output lineage.
suppressPackageStartupMessages({library(jsonlite); library(digest); library(yaml)})
if(.Platform$OS.type == "windows") Sys.setlocale("LC_CTYPE","English_United States.utf8")
args <- commandArgs(TRUE)
if (!length(args) || !file.exists(args[1])) stop("Supply an existing private JSON run configuration")
record_only <- "--record-only" %in% args[-1]
cfg <- fromJSON(args[1], simplifyVector=FALSE)
repo <- normalizePath(cfg$repository, winslash="/", mustWork=TRUE)
out <- normalizePath(cfg$output_root, winslash="/", mustWork=TRUE)
if (basename(out) != "reviewer_evidence_v1") stop("Unexpected article evidence root")
control <- file.path(out,"00_project_control")
dir.create(control, recursive=TRUE, showWarnings=FALSE)
git <- function(...) {
  result <- system2("git", c("-C", shQuote(repo), ...), stdout=TRUE, stderr=TRUE)
  if (!is.null(attr(result,"status"))) stop("Cannot resolve repository state")
  result
}
commit <- git("rev-parse", "HEAD")
branch <- git("branch", "--show-current")
if (branch != cfg$branch) stop("Unexpected revision branch")
if (!record_only) {
  for (stage in cfg$stages) {
    if(!is.null(stage$recording_role) && stage$recording_role == "dependency_or_preexecuted_export") next
    script <- normalizePath(file.path(repo,stage$script),winslash="/",mustWork=TRUE)
    if (!startsWith(script,paste0(repo,"/code/10_sensitivity/"))) stop("Unexpected script placement")
    if (!is.null(stage$completed_marker) && file.exists(file.path(out,stage$completed_marker)))
      stop("Completed stage already exists; inspect it instead of overwriting: ", stage$name)
    log <- file.path(out,stage$output_directory,"logs","execution.log")
    dir.create(dirname(log),recursive=TRUE,showWarnings=FALSE)
    python_stage <- grepl("[.]py$",script)
    executable <- if(python_stage) cfg$python_executable else file.path(R.home("bin"),"Rscript")
    if(is.null(executable)) stop("Configure the Python executable for Python stages")
    status <- system2(executable,
                      c(if(!python_stage) "--vanilla",shQuote(script),vapply(stage$arguments,shQuote,character(1))),
                      stdout=log,stderr=log)
    if (status != 0L) stop("Stage failed; inspect its execution log: ", stage$name)
  }
}
inputs <- lapply(cfg$inputs,function(x) {
  path <- normalizePath(x$path,winslash="/",mustWork=TRUE)
  data.frame(input_id=x$id, role=x$role, path=path, size_bytes=file.info(path)$size,
             sha256=digest(file=path,algo="sha256"),stringsAsFactors=FALSE)
})
input_table <- do.call(rbind,inputs)
if (anyDuplicated(input_table$input_id)) stop("Duplicate input identifiers")
write.csv(input_table,file.path(control,"input_manifest.csv"),row.names=FALSE)
write.csv(input_table[,c("input_id","path","sha256")],file.path(control,"input_checksums.csv"),row.names=FALSE)
scripts <- unique(vapply(cfg$stages,function(x) x$script,character(1)))
source_hashes <- setNames(lapply(scripts,function(x) digest(file=file.path(repo,x),algo="sha256")),scripts)
seed_rows <- do.call(rbind,lapply(cfg$seeds,function(x)
  data.frame(analysis=x$analysis,seed=x$seed,replicates=x$replicates)))
write.csv(seed_rows,file.path(control,"random_seeds.csv"),row.names=FALSE)
writeLines(capture.output(sessionInfo()),file.path(control,"sessionInfo.txt"))
pkgs <- cfg$packages
versions <- do.call(rbind,lapply(pkgs,function(p) {
  directory <- find.package(p,quiet=TRUE)
  available <- length(directory)==1L && nzchar(directory)
  version <- if(available) read.dcf(file.path(directory,"DESCRIPTION"),fields="Version")[1L,1L] else NA_character_
  data.frame(package=p,version=version,available=available)
}))
write.csv(versions,file.path(control,"package_versions.csv"),row.names=FALSE)
statuses <- do.call(rbind,lapply(cfg$analysis_status,function(x) as.data.frame(x,stringsAsFactors=FALSE)))
if(!all(statuses$validation_status %in% c("PASS","WARNING","FAIL"))) stop("Invalid analysis validation status")
write.csv(statuses,file.path(control,"analysis_status.csv"),row.names=FALSE)
run <- list(repository=repo,branch=branch,commit=commit,requested_base_commit=cfg$requested_base_commit,
            authorised_base_commit=cfg$authorised_base_commit,
            execution_base_commit=cfg$execution_base_commit,execution_code_state=cfg$execution_code_state,
            date=format(Sys.time(),tz="Europe/London",usetz=TRUE),
            scripts_used=scripts,script_sha256=source_hashes,
            source_datasets=input_table,input_paths=input_table$path,output_root=out,
            random_seeds=seed_rows,analysis_status=statuses,
            unresolved_blockers=cfg$blockers,
            preservation="No SDM refits, original file changes, manuscript edits or main merge",
            scope="Downstream reviewer-informed sensitivities; historical and paired responses remain separate")
write_yaml(run,file.path(control,"run_manifest.yml"))
files <- list.files(out,recursive=TRUE,full.names=TRUE,all.files=FALSE)
files <- files[!file.info(files)$isdir]
files <- files[!basename(files) %in% c("output_manifest.csv","analysis_output_index.csv")]
rows <- lapply(files,function(p) {
  relative <- substring(normalizePath(p,winslash="/",mustWork=TRUE),nchar(out)+2L)
  # Rules are explicit and ordered: specific artifact patterns precede modules.
  owners <- Filter(function(s) grepl(s$pattern,relative,perl=TRUE),cfg$artifact_owners)
  owner <- if(length(owners)) owners[[1L]]$script else cfg$report_script
  ids <- if(length(owners)) paste(unlist(owners[[1L]]$input_ids),collapse=";") else paste(input_table$input_id,collapse=";")
  original_writer <- owner
  writer_method <- "repository_script"
  superseded <- !is.null(cfg$superseded_patterns) && any(vapply(cfg$superseded_patterns,
                      function(pattern) grepl(pattern,relative,perl=TRUE),logical(1)))
  if(!is.null(cfg$writer_overrides[[relative]])) {
    override <- cfg$writer_overrides[[relative]]
    owner <- override$generating_script
    original_writer <- override$original_writer
    writer_method <- override$writer_method
  }
  data.frame(output_path=normalizePath(p,winslash="/",mustWork=TRUE),relative_path=relative,
             generating_script=owner,original_writer=original_writer,writer_method=writer_method,
             artifact_status=if(superseded) "SUPERSEDED_RETAINED" else "CURRENT_OR_DIAGNOSTIC",
             inventory_script="code/10_sensitivity/run_revision_sensitivity_analyses.R",
             input_ids=ids,commit=commit,
             size_bytes=file.info(p)$size,sha256=digest(file=p,algo="sha256"),stringsAsFactors=FALSE)
})
outputs <- do.call(rbind,rows)
write.csv(outputs,file.path(control,"output_manifest.csv"),row.names=FALSE)
write.csv(outputs,file.path(out,"07_integrated_evidence","analysis_output_index.csv"),row.names=FALSE)
cat("Recorded",nrow(input_table),"inputs and",nrow(outputs),"outputs at",commit,"\n")
