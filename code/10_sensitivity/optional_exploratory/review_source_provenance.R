#!/usr/bin/env Rscript
# Inspect existing optional inputs without replacing an unresolved model.
suppressPackageStartupMessages({library(jsonlite);library(digest)})
args <- commandArgs(TRUE)
if(length(args)!=1L || !file.exists(args[1])) stop("Supply the private run configuration")
cfg <- fromJSON(args[1],simplifyVector=FALSE)
out <- file.path(cfg$output_root,"06_optional_exploratory")
for(part in c("tables","figures","model_objects","qc","logs"))
  dir.create(file.path(out,part),recursive=TRUE,showWarnings=FALSE)
seed <- 20261014L
set.seed(seed)
summaries <- lapply(cfg$optional_sem_inputs,function(path) {
  if(!file.exists(path)) stop("Missing declared composite candidate: ",path)
  d <- read.csv(path,check.names=FALSE)
  required <- c("risk","reclamation","socioeconomic","Climate","Soil","Water")
  complete <- all(required %in% names(d))
  risk <- if("risk" %in% names(d)) d$risk else numeric()
  data.frame(input_path=path,sha256=digest(file=path,algo="sha256"),
             n=nrow(d),columns=ncol(d),has_required_composites=complete,
             complete_sem_rows=if(complete) sum(complete.cases(d[,required])) else NA_integer_,
             risk_min=if(length(risk)) min(risk,na.rm=TRUE) else NA_real_,
             risk_max=if(length(risk)) max(risk,na.rm=TRUE) else NA_real_,
             original_fit_input_verified=FALSE,
             conclusion="Archived candidate exists; exact interactive selection and preprocessing are unresolved")
})
write.csv(do.call(rbind,summaries),file.path(out,"tables","sem_input_provenance.csv"),row.names=FALSE)
age <- lapply(cfg$optional_age_candidates,function(path) {
  if(!file.exists(path)) stop("Missing declared age candidate: ",path)
  data.frame(input_path=path,sha256=digest(file=path,algo="sha256"),
             file_exists=TRUE,verified_reclamation_age=FALSE,
             conclusion="Period-like filenames do not establish per-polygon age or temporal provenance")
})
write.csv(do.call(rbind,age),file.path(out,"tables","reclamation_age_availability.csv"),row.names=FALSE)
status <- data.frame(analysis=c("SEM coefficient reproduction","SEM environment response sensitivity","Reclamation age sensitivity"),
                     validation_status="WARNING",execution_status="BLOCKED",
                     reason=c("Exact original fit input and preprocessing unresolved",
                              "Original SEM input not uniquely verified; no substitute SEM fitted",
                              "No verified per-polygon year/age provenance identified among declared inputs"))
write.csv(status,file.path(out,"qc","optional_analysis_status.csv"),row.names=FALSE)
write.csv(data.frame(analysis="optional_source_review",seed=seed,stochastic_analysis=FALSE),
          file.path(out,"qc","random_seeds.csv"),row.names=FALSE)
writeLines(capture.output(sessionInfo()),file.path(out,"logs","sessionInfo.txt"))
writeLines(c("Declared existing candidate files read and checksum-recorded.",
             "No SEM or age model fitted; unresolved analyses remain blocked."),
           file.path(out,"logs","source_provenance.log"))
cat("Optional source review complete; SEM and age sensitivities remain blocked.\n")
