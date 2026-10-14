# Current-map calibration thresholds from retained presences and stored backgrounds.
# Input: CSV species, group, projection_path, formatted_input_path, scale_divisor.
# Output: threshold table and point-score tables; no fitting or projection.
suppressPackageStartupMessages(library(biomod2))
suppressPackageStartupMessages(library(terra))

binary_load_saved <- function(path) {
  if (grepl("\\.rds$", path, ignore.case=TRUE)) return(readRDS(path))
  e <- new.env(parent=globalenv()); nm <- load(path, e)
  if (length(nm) != 1L) stop("Expected one saved input object")
  get(nm[[1]], e)
}

binary_maximum_tss <- function(scores, labels) {
  ok <- is.finite(scores); scores <- scores[ok]; labels <- labels[ok]
  if (any(!labels %in% c(0L,1L))) stop("Explicit binary response required")
  np <- sum(labels==1L); nn <- sum(labels==0L)
  if (!np || !nn) stop("Both finite presence and background scores required")
  ord <- order(scores); scores <- scores[ord]; labels <- labels[ord]
  rr <- rle(scores); ends <- cumsum(rr$lengths)
  removed_p <- c(0,cumsum(labels)[ends]); removed_n <- c(0,ends-cumsum(labels)[ends])
  se <- (np-head(removed_p,-1))/np; sp <- head(removed_n,-1)/nn
  tss <- se+sp-1; win <- which(tss >= max(tss)-1e-12)[1]
  list(cutoff=rr$values[win],tss=tss[win],sensitivity=se[win],specificity=sp[win])
}

binary_extract_one <- function(row) {
  f <- binary_load_saved(row$formatted_input_path)
  if (!is(f,"BIOMOD.formated.data")) stop("Unrecognised formatted input")
  response <- as.numeric(f@data.species)
  presence <- !is.na(response) & response==1
  background <- !is.na(response) & response==0
  if ("PA.table" %in% slotNames(f)) {
    pa <- f@PA.table
    if (nrow(pa)!=length(response)) stop("Background table does not align")
    background <- background | (!presence & rowSums(pa,na.rm=TRUE)>0)
  }
  keep <- presence | background
  xy <- as.data.frame(f@coord)
  if (nrow(xy)!=length(response) || ncol(xy)!=2) stop("Coordinates do not align")
  r <- rast(row$projection_path)
  if (nlyr(r)!=1L) stop("One continuous product required")
  if (grepl("TSSbin|TSSfilt",row$projection_path)) stop("Binary input forbidden")
  divisor <- as.numeric(row$scale_divisor)
  if (!is.finite(divisor) || divisor<=0) stop("Invalid declared score divisor")
  value <- extract(r,as.matrix(xy[keep,,drop=FALSE]))[[1]]
  if (any(is.infinite(value)) || any(value[is.finite(value)]<0 | value[is.finite(value)]>divisor)) stop("Score range disagrees with declared units")
  label <- as.integer(presence[keep]); points <- which(keep)
  p <- value[label==1 & is.finite(value)]
  if (!length(p)) stop("No finite retained presence scores")
  t10 <- as.numeric(quantile(p,.1,type=7,names=FALSE))
  mt <- binary_maximum_tss(value,label)
  scores <- data.frame(points=points,x=xy[keep,1],y=xy[keep,2],label=label,score_raw=value)
  summary <- data.frame(species=row$species,group=row$group,
    scientific_status=if("scientific_status" %in% names(row))row$scientific_status else "SOURCE_STATUS_NOT_SUPPLIED",
    projection_path=row$projection_path,formatted_input_path=row$formatted_input_path,
    scale_divisor=divisor,score_source="CURRENT_CONTINUOUS_MAP_AT_STORED_COORDINATES",
    validation_scope="CURRENT_MAP_CALIBRATION_POOLED_PA",
    background_policy="EXPLICIT_ZERO_OR_ANY_STORED_PA_COLUMN",
    quantile_type=7L,boundary="score >= cutoff",tss_tie="LOWEST_CUTOFF",
    t10_raw=t10,t10_01=t10/divisor,max_tss_raw=mt$cutoff,max_tss_01=mt$cutoff/divisor,
    max_tss=mt$tss,sensitivity=mt$sensitivity,specificity=mt$specificity,
    n_presence=sum(label==1),n_background=sum(label==0),
    n_finite_presence=sum(label==1 & is.finite(value)),
    n_finite_background=sum(label==0 & is.finite(value)),
    n_missing=sum(!is.finite(value)),status="CALIBRATION_THRESHOLD_COMPUTED")
  list(summary=summary,scores=scores)
}

if (sys.nframe()==0L) {
  args <- commandArgs(TRUE)
  if (!length(args) %in% c(2L,4L)) stop("Usage: binary_calibration_extract.R manifest.csv output_directory [first_row count]")
  d <- read.csv(args[1],stringsAsFactors=FALSE,check.names=FALSE)
  required <- c("species","group","projection_path","formatted_input_path","scale_divisor")
  if (!all(required %in% names(d))) stop("Missing manifest fields")
  if (anyDuplicated(paste(d$species,d$group,sep="|"))) stop("Duplicate species/group")
  if (length(args)==4L) {
    first <- as.integer(args[3]); count <- as.integer(args[4])
    if (is.na(first) || is.na(count) || first<1L || count<1L || first>nrow(d)) stop("Invalid row range")
    d <- d[seq.int(first,min(nrow(d),first+count-1L)),,drop=FALSE]
  }
  if (dir.exists(args[2])) stop("Output directory already exists")
  dir.create(args[2],recursive=TRUE); dir.create(file.path(args[2],"point_scores"))
  result <- list(); failures <- list()
  for (i in seq_len(nrow(d))) {
    z <- tryCatch(binary_extract_one(d[i,,drop=FALSE]),error=function(e)e)
    if (inherits(z,"error")) {
      failures[[length(failures)+1L]] <- data.frame(species=d$species[i],group=d$group[i],reason=conditionMessage(z))
    } else {
      z$summary$point_score_table <- file.path("point_scores",sprintf("%05d.csv",i))
      result[[length(result)+1L]] <- z$summary
      write.csv(z$scores,file.path(args[2],"point_scores",sprintf("%05d.csv",i)),row.names=FALSE)
    }
  }
  if(length(result)) write.csv(do.call(rbind,result),file.path(args[2],"thresholds.csv"),row.names=FALSE)
  if(length(failures)) write.csv(do.call(rbind,failures),file.path(args[2],"excluded.csv"),row.names=FALSE)
  writeLines(c(paste("requested",nrow(d)),paste("computed",length(result)),paste("excluded",length(failures))),file.path(args[2],"availability.txt"))
}
