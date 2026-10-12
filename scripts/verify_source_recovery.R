# Parse and structural checks only: never source or evaluate analysis code.
args <- commandArgs(TRUE)
root <- normalizePath(if(length(args)) args[1] else ".", mustWork=TRUE)
m <- read.csv(file.path(root,"metadata/source_recovery_manifest.csv"), stringsAsFactors=FALSE)
normalize <- function(x) {
  if (is.call(x) && identical(x[[1]],as.name("publication_path"))) {
    args <- as.list(x)[-1]
    if (length(args) && all(vapply(args,is.character,logical(1))))
      return(paste0("<",args[[1]],">",if(length(args)>1) paste0("/",paste(unlist(args[-1]),collapse="/")) else ""))
  }
  if (is.call(x)) return(as.call(lapply(as.list(x),normalize)))
  if (is.expression(x)) return(as.expression(lapply(as.list(x),normalize)))
  if (is.pairlist(x)) return(as.pairlist(lapply(x,normalize)))
  if (is.list(x)) return(lapply(x,normalize))
  x
}
out <- vector("list",nrow(m));fail <- 0
for(i in seq_len(nrow(m))) {
  row <- m[i,]
  status <- tryCatch({
    if (!grepl("[.]r$",row$publication_file,ignore.case=TRUE)) "PRESERVED_INSPECTION_ONLY" else {
      pub <- parse(file.path(root,row$publication_file),encoding="UTF-8",keep.source=FALSE)
      snap <- parse(file.path(root,row$snapshot_file),encoding="UTF-8",keep.source=FALSE)
      if (startsWith(row$publication_file,"code/")) pub <- pub[-1] # portability guard only
      if (!identical(normalize(pub),normalize(snap))) stop("structural mismatch")
      "PASS_PARSE_AND_SCIENTIFIC_TREE"
    }
  },error=function(e) paste("FAIL",conditionMessage(e)))
  if(startsWith(status,"FAIL")) {fail<-fail+1;cat(row$publication_file,":",status,"\n")}
  out[[i]] <- data.frame(publication_file=row$publication_file,status=status)
}
results <- do.call(rbind,out)
write.csv(results,file.path(root,"metadata/source_recovery_validation.csv"),row.names=FALSE)
cat("Sources:",nrow(m),"verified:",sum(results$status=="PASS_PARSE_AND_SCIENTIFIC_TREE"),
    "inspection-only:",sum(results$status=="PRESERVED_INSPECTION_ONLY"),"failures:",fail,"\n")
quit(status=as.integer(fail>0))
