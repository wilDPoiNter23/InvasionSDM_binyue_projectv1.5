#!/usr/bin/env Rscript
# Inspect recorded period codes without interpreting them as verified ages.
suppressPackageStartupMessages({library(jsonlite);library(digest)})
a <- commandArgs(TRUE)
if(length(a)!=1L || !file.exists(a[1])) stop("Supply an existing private run configuration")
cfg <- fromJSON(a[1],simplifyVector=FALSE)
out <- file.path(cfg$output_root,"06_optional_exploratory")
records <- lapply(cfg$optional_age_candidates,function(path) {
  dbf <- sub("[.]shp$",".dbf",path,ignore.case=TRUE)
  if(!file.exists(dbf)) stop("Declared vector attribute component is missing")
  d <- foreign::read.dbf(dbf)
  period_columns <- grep("year|date|age|period",names(d),ignore.case=TRUE,value=TRUE)
  codes <- if(length(period_columns)) paste(vapply(period_columns,function(x)
    paste(x,paste(sort(unique(d[[x]])),collapse=";"),sep="="),character(1)),collapse=" | ") else ""
  data.frame(vector_path=path,attributes_path=dbf,attributes_sha256=digest(file=dbf,algo="sha256"),
             n_features=nrow(d),attribute_columns=paste(names(d),collapse=";"),
             temporal_fields=paste(period_columns,collapse=";"),recorded_codes=codes,
             verified_calendar_year_dictionary=FALSE,verified_global_age_scope=FALSE,
             conclusion="Period codes exist; calendar-year mapping, temporal source and final spatial scope remain unverified")
})
target <- file.path(out,"tables","reclamation_age_attribute_inspection.csv")
if(file.exists(target)) stop("Existing inspection preserved; inspect it before further execution")
write.csv(do.call(rbind,records),target,row.names=FALSE)
writeLines(capture.output(sessionInfo()),file.path(out,"logs","age_attribute_sessionInfo.txt"))
cat("Existing period attributes inspected; no age model fitted.\n")
