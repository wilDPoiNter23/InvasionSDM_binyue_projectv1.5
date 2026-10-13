#!/usr/bin/env Rscript
# Check whether legacy zones plus the documented city patches cover final cities.
suppressPackageStartupMessages({library(sf);library(digest)})
a=commandArgs(TRUE)
if(length(a)!=5L) stop('Usage: inspect_city_zone_lineage.R FINAL_CITIES LEGACY_RECLAIM LEGACY_NONRECLAIM PATCH_ZONES OUTPUT_ROOT')
sf_use_s2(FALSE)
z=lapply(a[1:4],function(p)st_read(p,quiet=TRUE));names(z)=c('final','reclaim','nonreclaim','patch')
key=function(x)trimws(tolower(x))
out=a[5];dir.create(file.path(out,'qc'),recursive=TRUE,showWarnings=FALSE)
receipts=data.frame(role=names(z),path=a[1:4],sha256=vapply(a[1:4],function(p)digest(file=p,algo='sha256'),character(1)),features=vapply(z,nrow,integer(1)))
write.csv(receipts,file.path(out,'qc','zone_lineage_source_checksums.csv'),row.names=FALSE)
rows=list()
for(i in seq_len(nrow(z$final))) {
  city=z$final$shapeName[i];k=key(city);final=st_make_valid(z$final[i,]);patched=k %in% key(z$patch$shapeName)
  if(patched){q=z$patch[key(z$patch$shapeName)==k,]} else {
    rec=z$reclaim[key(z$reclaim$shapeName)==k,];non=z$nonreclaim[key(z$nonreclaim$shapeName)==k,]
    q=st_sf(geometry=c(st_geometry(rec),st_geometry(non)))
  }
  if(!nrow(q)){rows[[i]]=data.frame(city=city,source='MISSING',relative_symmetric_difference=NA_real_,within_city=FALSE);next}
  q=st_make_valid(st_transform(q,st_crs(final))); q=st_union(st_geometry(q))
  fm=st_transform(st_geometry(final),6933);qm=st_transform(q,6933)
  error='';relative=tryCatch({area=sum(as.numeric(st_area(fm)));sym=suppressWarnings(st_sym_difference(fm,qm));sum(as.numeric(st_area(sym)))/area},error=function(e){error <<- conditionMessage(e);NA_real_})
  rows[[i]]=data.frame(city=city,source=if(patched)'FIVE_CITY_PATCH' else 'LEGACY_GIS_ZONES',relative_symmetric_difference=relative,within_city=is.finite(relative)&&relative<=1e-8,error=error)
}
result=do.call(rbind,rows);write.csv(result,file.path(out,'qc','final_city_zone_union_equivalence.csv'),row.names=FALSE)
cat('Geometric union matches:',sum(result$within_city),'of',nrow(result),'; failures do not authorize geometry substitution.\n')
quit(save='no',status=0)
