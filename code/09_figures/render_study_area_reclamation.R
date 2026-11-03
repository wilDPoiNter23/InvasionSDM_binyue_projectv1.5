# Map the study cities, reclaimed-land uses and mapped reclamation-year classes.
# Usage: Rscript render_study_area_reclamation.R private_config.json
suppressPackageStartupMessages({
  library(sf); library(ggplot2); library(patchwork); library(jsonlite)
  library(svglite); library(ragg); library(digest)
})
args <- commandArgs(TRUE)
stopifnot(length(args) == 1L)
cfg <- read_json(args[1], simplifyVector=TRUE)
out <- cfg$output_root
stopifnot(dir.exists(out), dir.exists(dirname(cfg$gis_output)))
sf_use_s2(FALSE)
cowplot::set_null_device('agg')
key <- function(x) tolower(trimws(x))
logmsg <- function(...) {cat(format(Sys.time(), '%H:%M:%S'), ..., '\n'); flush.console()}
font <- 'Arial'
theme_set(theme_void(base_size=7, base_family=font) + theme(
  plot.title=element_text(size=8, face='bold', colour='#253640'),
  plot.subtitle=element_text(size=6.3, colour='#55646C'),
  plot.tag=element_text(size=9, face='bold'),
  plot.margin=margin(3,4,3,4), legend.title=element_text(size=7),
  legend.text=element_text(size=6.1), legend.key.size=grid::unit(2.8,'mm'),
  panel.background=element_rect(fill='#EDF4F6', colour=NA),
  panel.border=element_rect(fill=NA, colour='#CBD5D9', linewidth=.25)))
type_names <- c(bare='Bare land', comm='Commercial', crop='Cropland',
  forest='Forest', grass='Grassland', indus='Industrial', public='Public use',
  resid='Residential', transp='Transport', water='Water')
type_cols <- c(bare='#C4B99B', comm='#A36EA6', crop='#D0B646', forest='#39785C',
  grass='#93B57A', indus='#C36B4E', public='#5686A5', resid='#D89CA7',
  transp='#665E85', water='#65ADBE')
year_cols <- setNames(c('#F5DCB4','#E8B581','#D58B63','#B95C59',
  '#88516D','#5B527E','#344E70'), as.character(seq(1990,2020,5)))
logmsg('Read the 108 study boundaries and point locations')
b <- suppressWarnings(st_read(cfg$cities, quiet=TRUE))
b <- b[,c('shapeName','shapeGroup')]
names(b)[1:2] <- c('city','iso3')
b$city_key <- key(b$city)
b <- b[order(b$city_key),]
b$city_id <- seq_len(nrow(b))
stopifnot(nrow(b)==108L, !anyDuplicated(b$city_key))
p <- st_read(cfg$points, quiet=TRUE)
stopifnot(nrow(p)==108L, setequal(key(p$city),b$city_key))
p$city_key <- key(p$city)
p$city_id <- b$city_id[match(p$city_key,b$city_key)]
bm <- st_transform(b,3395)
boundary_repairs <- sum(!st_is_valid(bm))
bm <- st_make_valid(bm)
logmsg('Read and simplify a display-only world basemap')
world <- st_read(cfg$world,quiet=TRUE)
world <- st_transform(world,3395)
world <- suppressWarnings(st_simplify(st_make_valid(world), dTolerance=1500,
  preserveTopology=TRUE))
cache <- file.path(out,'study_area_map_cache.rds')
sig <- lapply(cfg[c('cities','points','landuse','changeyear','world')],
  function(f) digest(file=f,algo='sha256'))
if(file.exists(cache)) {
  cc <- readRDS(cache)
  stopifnot(identical(cc$signature,sig))
  lu <- cc$landuse; yr <- cc$year; processing <- cc$processing
  logmsg('Reused matching clipped geometry cache')
} else {
  clip_source <- function(path,field,allowed,label) {
    logmsg('Read',label)
    x <- st_read(path,quiet=TRUE)
    stopifnot(st_crs(x)$epsg==3395L, setequal(as.character(x[[field]]),allowed))
    x <- x[,field,drop=FALSE]; names(x)[1] <- 'class_code'
    x$class_code <- as.character(x$class_code)
    logmsg('Separate spatial components for',label)
    x <- suppressWarnings(st_cast(x,'POLYGON'))
    total <- nrow(x)
    logmsg(label,':',total,'components; select those intersecting study boundaries')
    # GEOS indexes individual components rather than global multipart rows.
    hit <- lengths(st_intersects(x,bm)) > 0L
    x <- x[hit,]
    invalid <- sum(!st_is_valid(x))
    x <- st_make_valid(x)
    logmsg(label,':',nrow(x),'selected; clip to the 108 study boundaries')
    z <- suppressWarnings(st_intersection(x,bm[,c('city_id','city','city_key')]))
    z <- suppressWarnings(st_collection_extract(z,'POLYGON'))
    z <- z[!st_is_empty(z),]
    z$area_km2 <- as.numeric(st_area(st_transform(z,6933)))/1e6
    stopifnot(nrow(z)>0, all(st_is_valid(z)), all(is.finite(z$area_km2)),
      all(z$area_km2>=0))
    list(data=z, info=list(components=total, selected=sum(hit),
      invalid_selected_repaired=invalid, clipped_features=nrow(z)))
  }
  ll <- clip_source(cfg$landuse,'class',names(type_names),'land-use classes')
  yy <- clip_source(cfg$changeyear,'reclass1',names(year_cols),'mapped year classes')
  lu <- ll$data; yr <- yy$data
  processing <- list(landuse=ll$info,year=yy$info,boundary_repairs=boundary_repairs)
  saveRDS(list(landuse=lu,year=yr,processing=processing,signature=sig),cache)
}
lu$category <- factor(lu$class_code, levels=names(type_names))
yr$category <- factor(yr$class_code, levels=names(year_cols))
logmsg('Write city coverage and class-area source tables')
summarize <- function(x) {
  a <- aggregate(x$area_km2,st_drop_geometry(x)[c('city_id','city','city_key','class_code')],sum)
  names(a)[5] <- 'area_km2'; a
}
lt <- summarize(lu); yt <- summarize(yr)
write.csv(lt,file.path(out,'city_landuse_area_km2.csv'),row.names=FALSE)
write.csv(yt,file.path(out,'city_year_class_area_km2.csv'),row.names=FALSE)
coverage <- st_drop_geometry(b)[,c('city_id','city','city_key','iso3')]
coverage$landuse_km2 <- vapply(coverage$city_id,function(i)sum(lu$area_km2[lu$city_id==i]),numeric(1))
coverage$year_class_km2 <- vapply(coverage$city_id,function(i)sum(yr$area_km2[yr$city_id==i]),numeric(1))
coverage$boundary_area_km2 <- as.numeric(st_area(st_transform(bm,6933)))/1e6
coverage$boundary_review <- ifelse(coverage$boundary_area_km2>200000,
  'Oversized supplied boundary; review before interpretation','')
write.csv(coverage,file.path(out,'city_108_map_index.csv'),row.names=FALSE)
if(!file.exists(cfg$gis_output)) {
  st_write(b,cfg$gis_output,layer='study_cities_108',quiet=TRUE)
  st_write(p,cfg$gis_output,layer='city_locations_108',quiet=TRUE)
  st_write(lu,cfg$gis_output,layer='reclamation_landuse',quiet=TRUE)
  st_write(yr,cfg$gis_output,layer='reclamation_year_class',quiet=TRUE)
}

# Display simplification is independent of the full-geometry area tables.
ld <- st_simplify(lu,dTolerance=12,preserveTopology=TRUE)
yd <- st_simplify(yr,dTolerance=12,preserveTopology=TRUE)
bd <- st_simplify(bm,dTolerance=20,preserveTopology=TRUE)
world_ll <- st_transform(world,4326)
world_ll <- suppressWarnings(st_crop(world_ll,c(xmin=-180,ymin=-56,xmax=180,ymax=76)))
robin <- '+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs'
labels <- p[p$city_key %in% key(cfg$focus_cities),]
stopifnot(nrow(labels) == length(unique(key(cfg$focus_cities))))
present <- coverage$landuse_km2[match(key(p$city),coverage$city_key)]>0
p$coverage <- factor(ifelse(present,'Mapped polygons (85 cities)','No mapped polygons (23 cities)'),
  levels=c('Mapped polygons (85 cities)','No mapped polygons (23 cities)'))
overview <- ggplot() +
  geom_sf(data=world_ll,fill='#E0E5E5',colour='white',linewidth=.08) +
  geom_sf(data=p,aes(fill=coverage),shape=21,size=1.55,stroke=.28,colour='#795A47') +
  geom_sf(data=labels,shape=21,size=2.3,stroke=.45,fill='#243F50',colour='white') +
  scale_fill_manual(values=c('#C6773F','white'),name=NULL) +
  coord_sf(crs=robin,expand=FALSE,datum=NA) +
  labs(title='108 coastal cities',subtitle='City locations | dark markers identify the three coastal details below') +
  theme(legend.position='bottom',legend.text=element_text(size=6),
    legend.margin=margin(0,0,0,0),legend.key.height=grid::unit(2.5,'mm'))

make_window <- function(i,detail=FALSE) {
  # Use the full study boundary so detailed pages always show its complete extent.
  bb <- st_bbox(bm[i,])
  if(detail) {
    a <- ld[ld$city_id==b$city_id[i],]
    z <- yd[yd$city_id==b$city_id[i],]
    if(nrow(a)+nrow(z)>0) bb <- st_bbox(c(st_geometry(a),st_geometry(z)))
  }
  cx <- mean(bb[c(1,3)]); cy <- mean(bb[c(2,4)])
  span <- max(bb[3]-bb[1],bb[4]-bb[2],8000)*1.12
  c(xmin=cx-span/2,ymin=cy-span/2,xmax=cx+span/2,ymax=cy+span/2)
}
map_city <- function(i,kind,legend=FALSE,detail=FALSE) {
  box <- make_window(i,detail); city_id <- b$city_id[i]
  xx <- if(kind=='type')ld[ld$city_id==city_id,] else yd[yd$city_id==city_id,]
  pal <- if(kind=='type')type_cols else year_cols
  nam <- if(kind=='type')type_names else setNames(names(year_cols),names(year_cols))
  # Crop the low-detail background before drawing to keep atlas PDF sizes bounded.
  wb <- suppressWarnings(st_crop(world,st_bbox(box,crs=st_crs(3395))))
  pp <- ggplot() + geom_sf(data=wb,fill='#E5E7E3',colour='#BDC9CC',linewidth=.12) +
    geom_sf(data=bd[i,],fill='white',colour=NA) +
    geom_sf(data=xx,aes(fill=category),colour=NA) +
    geom_sf(data=bd[i,],fill=NA,colour='#4A5C65',linewidth=.24) +
    scale_fill_manual(values=pal,breaks=names(pal),labels=nam,drop=FALSE,
      name=if(kind=='type')'Reclaimed-land use' else 'Mapped reclamation year') +
    coord_sf(crs=3395,xlim=box[c(1,3)],ylim=box[c(2,4)],expand=FALSE,datum=NA) +
    labs(title=paste0(sprintf('%03d',city_id),'  ',b$city[i]),
      subtitle=if(kind=='type')'Reclaimed-land use' else 'Mapped reclamation-year class') +
    theme(legend.position=if(legend)'bottom'else'none')
  # World Mercator scale is corrected locally for ground distance.
  lat <- st_coordinates(st_transform(suppressWarnings(st_centroid(bm[i,])),4326))[1,2]
  width_ground <- (box[3]-box[1])*cos(lat*pi/180)
  choices <- c(1,2,5,10,20,50,100,200,500)*1000
  scale_m <- max(choices[choices < width_ground*.28],1000)
  dx <- scale_m/cos(lat*pi/180)
  x0 <- box[1]+.06*(box[3]-box[1]); y0 <- box[2]+.07*(box[4]-box[2])
  pp <- pp + annotate('segment',x=x0,xend=x0+dx,y=y0,yend=y0,linewidth=.65,colour='#253640') +
    annotate('text',x=x0+dx/2,y=y0+.045*(box[4]-box[2]),label=paste(scale_m/1000,'km'),size=2.1,family=font)
  if(!nrow(xx)) pp <- pp + annotate('text',x=mean(box[c(1,3)]),y=mean(box[c(2,4)]),
    label='No mapped polygons\nin this boundary',size=2.5,colour='#6C7478',family=font)
  if(coverage$boundary_area_km2[i]>200000) pp <- pp + labs(
    subtitle=paste(if(kind=='type')'Land use'else'Year class', '| OVERSIZED SOURCE BOUNDARY'))
  pp
}
legend_plot <- function(kind) {
  pal <- if(kind=='type')type_cols else year_cols
  nam <- if(kind=='type')type_names else setNames(names(year_cols),names(year_cols))
  ggplot(data.frame(x=seq_along(pal),y=0,k=factor(names(pal),levels=names(pal))),aes(x,y,fill=k)) +
    geom_point(shape=22,size=3) + scale_fill_manual(values=pal,labels=nam,drop=FALSE,
      name=if(kind=='type')'Reclaimed-land use' else 'Mapped reclamation year') +
    guides(fill=guide_legend(nrow=if(kind=='type')2 else 1,byrow=TRUE,title.position='top')) +
    theme_void(base_size=6.5,base_family=font) + theme(legend.position='bottom',
      legend.title=element_text(size=7,face='bold'),legend.text=element_text(size=6),
      legend.key.size=grid::unit(2.8,'mm'),legend.spacing.x=grid::unit(1,'mm'))
}
legend_strip <- function(kind) wrap_elements(full=cowplot::get_legend(legend_plot(kind)))
save_plot <- function(plot,name,width=183,height=228,tiff=TRUE) {
  f <- file.path(out,name); w <- width/25.4; h <- height/25.4
  logmsg('Export',name)
  svglite(paste0(f,'.svg'),width=w,height=h); print(plot); dev.off()
  cairo_pdf(paste0(f,'.pdf'),width=w,height=h,family=font); print(plot); dev.off()
  agg_png(paste0(f,'.png'),width=w,height=h,units='in',res=220); print(plot); dev.off()
  if(tiff) {agg_tiff(paste0(f,'.tiff'),width=w,height=h,units='in',res=600,compression='lzw');print(plot);dev.off()}
}
logmsg('Compose the global overview and paired example maps')
ii <- match(key(cfg$focus_cities),b$city_key)
stopifnot(!anyNA(ii))
type_row <- wrap_plots(lapply(seq_along(ii),function(j)
  map_city(ii[j],'type',detail=TRUE)+labs(tag=letters[j+1])),nrow=1)
year_row <- wrap_plots(lapply(seq_along(ii),function(j)
  map_city(ii[j],'year',detail=TRUE)+labs(tag=letters[j+4])),nrow=1)
row_heading <- function(label) wrap_elements(full=grid::textGrob(label,x=0,y=.2,
  just=c('left','bottom'),gp=grid::gpar(fontfamily=font,fontface='bold',
    fontsize=10,colour='#253640')))
main <- wrap_plots(list(row_heading('Reclaimed-land use categories'),type_row,
  legend_strip('type'),row_heading('Reclamation-year classes'),year_row,
  legend_strip('year')),ncol=1,heights=c(.105,1,.27,.105,1,.20))
save_plot(main,'study_area_108cities_reclamation',183,168)
if(isTRUE(cfg$render_overview)) save_plot(overview,'study_area_108cities_overview',183,92,FALSE)

if(isTRUE(cfg$render_atlas)) {
logmsg('Export all 108 cities as 27 four-city atlas pages')
atlas <- file.path(out,'study_area_108cities_atlas.pdf')
cairo_pdf(atlas,width=210/25.4,height=270/25.4,family=font,onefile=TRUE)
for(pg in seq_len(27)) {
  ix <- ((pg-1)*4+1):(pg*4)
  rows <- lapply(ix,function(i)map_city(i,'type')+map_city(i,'year'))
  page <- wrap_plots(c(rows,list(legend_strip('type'),legend_strip('year'))),
    ncol=1,heights=c(rep(1,4),.34,.23)) +
    plot_annotation(title=paste0('Study-city atlas  |  ',pg,' / 27'),
      caption='Left: land use. Right: mapped reclamation-year class. Same map extent within each pair.\nOutlines mark the full study boundary; uncoloured areas have no mapped class. Scale bars show approximate local ground distance.',
      theme=theme(plot.title=element_text(size=11,face='bold',family=font),
        plot.caption=element_text(size=6,hjust=0,family=font)))
  print(page)
  if(pg==1) first_page <- page
  logmsg('Atlas page',pg,'/ 27')
}
dev.off()
agg_png(file.path(out,'study_area_108cities_atlas_preview.png'),width=210,height=270,units='mm',res=180)
print(first_page);dev.off()
}
write_json(list(status='EXPORTED_PENDING_VISUAL_REVIEW',cities=108,
  atlas_pages=if(isTRUE(cfg$render_atlas))27L else 0L,source_sha256=sig,
  processing=processing,landuse_classes=type_names,year_classes=names(year_cols),
  display_simplification_m=list(reclamation=12,boundary=20,world=1500),
  zero_landuse_cities=coverage$city[coverage$landuse_km2==0],
  zero_year_cities=coverage$city[coverage$year_class_km2==0]),
  file.path(out,'study_area_validation.json'),auto_unbox=TRUE,pretty=TRUE)
writeLines(c('Study regions and mapped reclamation.',
  'The overview shows the 108 study-city boundaries and their supplied city locations.',
  'Detailed maps show land-use categories and mapped year classes clipped to each study boundary.',
  'The paired panels use identical extents. The atlas includes every study city in alphabetical order.',
  'Year values 1990, 1995, 2000, 2005, 2010, 2015 and 2020 are the original reclass1 codes.',
  'No within-bin construction date, temporal interval or reclamation age is inferred.',
  'Land-use names expand the source codes; water is retained as an original mapped category.',
  'White interiors indicate areas without a mapped category, not independently verified unreclaimed land.',
  'Full geometry is used for EPSG:6933 equal-area class totals. Display geometry alone is simplified.',
  'Zero coverage is reported explicitly; it is not evidence that a city has no reclamation.',
  'Overlapping city boundaries can assign the same source polygon to more than one city; city totals must not be summed as a deduplicated global area.',
  'Scale bars correct World Mercator distances locally by latitude and are approximate.',
  'The world basemap provides geographic context and does not determine study boundaries.'),
  file.path(out,'study_area_caption.txt'))
logmsg('Finished; geometry, index and export checks written')
