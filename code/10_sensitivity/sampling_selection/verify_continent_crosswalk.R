# Verify city identity and derive continent groups from independent country metadata.
verify_continent_crosswalk <- function(d, metadata_path, input_path) {
  meta <- read.csv(metadata_path, check.names=FALSE,fileEncoding="UTF-8-BOM")
  key <- tolower(trimws(meta$City)); target <- tolower(trimws(d$city))
  if(anyDuplicated(key)||anyDuplicated(target)) stop("City identity is not unique.")
  ix <- match(target,key); reason <- rep("Exact city-name match",nrow(d))
  j <- which(target=="zhanzhou" & is.na(ix)); alt <- which(key=="danzhou")
  if(length(j)==1L && length(alt)==1L && abs(d$Latitude[j]-meta$Latitude[alt])<.001 && abs(d$Longitude[j]-meta$Longitude[alt])<.001 && abs(d$reclamation_area[j]-meta[["Reclamation area (ha)"]][alt])<.001) {
    ix[j]<-alt;reason[j]<-"Explicit zhanzhou/Danzhou alias: matching coordinates and reclamation area"
  }
  if(anyNA(ix)) stop("Unresolved city metadata identities.")
  meta <- meta[ix,]
  country_groups <- list(
    Africa=c("Ivory Coast","Ghana","Egypt","Algeria","Guinea","Senegal","Tanzania","Sierra Leone","South Africa","Nigeria","Togo","Angola","Mozambique","Kenya","Morocco","Mauritania","Republic of the Congo","Libya","Tunisia"),
    Asia=c("China","Indonesia","India","Japan","UAE","Saudi Arabia","South Korea","Lebanon","Bangladesh","Vietnam","Israel","Pakistan","Kuwait","Philippines","Oman","Singapore","Turkey"),
    Europe=c("Netherlands","Greece","Spain","Denmark","Finland","Italy","Portugal","Russia"),
    Oceania=c("Australia","New Zealand"),
    `South America`=c("Brazil","Argentina","Colombia","Peru","Uruguay"),
    `North America`=c("USA","Panama","Haiti","Puerto Rico","Dominican Republic"))
  continent <- rep(NA_character_,nrow(d))
  for(g in names(country_groups)) continent[meta$Country%in%country_groups[[g]]]<-g
  if(anyNA(continent)) stop("Unclassified countries in city metadata.")
  # These transcontinental countries retain the verified city-region assignment.
  special <- meta$Country%in%c("Russia","Turkey")
  if(any(meta$Country=="Russia" & !(target=="st petersburg" & d$Regions=="Eastern Europe"))) stop("Unresolved Russian city-region grouping.")
  if(any(meta$Country=="Turkey" & !(target=="istanbul" & d$Regions=="Western Asia"))) stop("Unresolved Turkish city-region grouping.")
  reason[special]<-paste(reason[special],"transcontinental country: supplied city-region assignment retained",sep="; ")
  original <- ifelse(grepl("Europe",d$Regions,fixed=TRUE),"Europe",ifelse(grepl("Africa",d$Regions,fixed=TRUE),"Africa",ifelse(grepl("Asia",d$Regions,fixed=TRUE),"Asia",ifelse(d$Regions=="Oceania","Oceania",ifelse(d$Regions=="South America","South America","North America")))))
  delta_lat <- abs(d$Latitude-meta$Latitude);delta_lon<-abs(d$Longitude-meta$Longitude)
  swapped <- abs(d$Latitude-meta$Longitude)<.001 & abs(d$Longitude-meta$Latitude)<.001
  agreement <- ifelse(delta_lat<.001 & delta_lon<.001,"Coordinates match within 0.001 degree",ifelse(swapped,"Independent metadata coordinates swapped; canonical coordinates preserved",ifelse(delta_lat<1&delta_lon<1,"City identity matches; coordinates differ by less than 1 degree","Coordinates unresolved")))
  if(any(agreement=="Coordinates unresolved"))stop("Unresolved geographic coordinates.")
  fields<-list(city=d$city,metadata_city=meta$City,country=meta$Country,source_region=d$Regions,metadata_region=meta$Regions,
    source_latitude=d$Latitude,source_longitude=d$Longitude,metadata_latitude=meta$Latitude,metadata_longitude=meta$Longitude,
    coordinate_check=agreement,identity_evidence=reason,original_continent=original,verified_continent=continent,
    continent_changed=original!=continent,region_conflict=d$Regions!=meta$Regions,
    source_sha256=digest::digest(file=input_path,algo="sha256"),metadata_sha256=digest::digest(file=metadata_path,algo="sha256"),
    geographic_rule="Country-based geographic groups; Caribbean countries in North America; city-region assignments for Turkey/Russia")
  if(any(lengths(fields)==0))stop("Empty crosswalk fields: ",paste(names(fields)[lengths(fields)==0],collapse=", "))
  as.data.frame(fields,stringsAsFactors=FALSE)
}
