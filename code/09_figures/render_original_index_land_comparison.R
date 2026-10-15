# Present the historical manuscript response with one shared city sample and scale.
suppressPackageStartupMessages({library(ggplot2);library(patchwork);library(jsonlite);library(svglite);library(ragg);library(digest)})
a<-commandArgs(TRUE);stopifnot(length(a)==1L);cfg<-fromJSON(a[1]);out<-cfg$output_root
stopifnot(!dir.exists(out),fromJSON(file.path(cfg$input_root,'verification.json'))$status=='PASS_FULL_SURFACE_ZONE_ARITHMETIC')
dir.create(out,recursive=TRUE)
src<-file.path(cfg$input_root,'matched_city_means.csv');d<-read.csv(src,stringsAsFactors=FALSE)
d<-d[d$group=='ORIGINAL_SII'&d$minimum_cell_equivalents==0,];ids<-sort(unique(d$city_id));n<-length(ids)
zones<-c('OUTSIDE_RING','CITY_TOTAL','NON_RECLAIMED_CITY','RECLAIMED_CITY');stopifnot(n>1L,all(table(d$city_id)==4L))
y<-sapply(zones,function(z){q<-d[d$zone==z,];q$mean_suitability[match(ids,q$city_id)]})
set.seed(cfg$seed);draw<-replicate(1999,sample.int(n,n,replace=TRUE))
summaries<-do.call(rbind,lapply(1:4,function(j){b<-colMeans(matrix(y[,j][draw],nrow=n));ci<-quantile(b,c(.025,.975),names=FALSE)
 data.frame(zone=zones[j],mean=mean(y[,j]),lower=ci[1],upper=ci[2])}))
stopifnot(max(abs(summaries$mean-colMeans(y)))<1e-12)
write.csv(d,file.path(out,'city_source_data.csv'),row.names=FALSE);write.csv(summaries,file.path(out,'figure_source_data.csv'),row.names=FALSE)
font<-if(capabilities('cairo'))'Liberation Sans'else'Helvetica'
theme_set(theme_classic(base_size=7,base_family=font)+theme(axis.text=element_text(size=6.5,colour='black'),
 axis.line=element_line(linewidth=.3),axis.ticks=element_line(linewidth=.3),plot.title=element_text(size=7.5,face='bold'),
 plot.tag=element_text(size=8,face='bold'),legend.position='none',plot.margin=margin(6,8,5,5)))
limits<-c(0,max(summaries$upper)*1.23);ring<-summaries$mean[1]
panel<-function(index,labels,title){q<-summaries[index,];q$x<-factor(seq_len(nrow(q)),labels=labels)
 ggplot(q,aes(x,mean))+geom_hline(yintercept=ring,colour='grey65',linetype=2,linewidth=.35)+
 geom_col(aes(fill=x),width=.48)+geom_errorbar(aes(ymin=lower,ymax=upper),width=.12,linewidth=.4)+
 geom_text(aes(y=upper,label=sprintf('%.3f',mean)),vjust=-.7,size=2.3,family=font)+
 scale_fill_manual(values=c('#A6B1B8','#35758A'))+scale_y_continuous(limits=limits,expand=expansion(mult=c(0,.01)))+
 labs(x=NULL,y='Mean invasion pressure index',title=title)}
p1<-panel(1:2,c('Outside cities','Whole cities'),paste0('Cities exceed their surroundings (n = ',n,')'))
percent<-100*(1-summaries$mean[4]/summaries$mean[3])
p2<-panel(3:4,c('Other urban land','Reclaimed land'),'Reclaimed land has a lower index')+
 annotate('text',x=1.5,y=limits[2]*.96,label=sprintf('%.1f%% lower',percent),size=2.8,fontface='bold',family=font)+labs(y=NULL)
p<-(p1|p2)+plot_annotation(tag_levels='a');w<-183/25.4;h<-85/25.4;name<-file.path(out,'original_index_land_comparison')
svglite(paste0(name,'.svg'),width=w,height=h);print(p);dev.off()
if(capabilities('cairo')){cairo_pdf(paste0(name,'.pdf'),width=w,height=h,family=font);print(p);dev.off()}else{
 pdf(paste0(name,'.pdf'),width=w,height=h,family='Helvetica',useDingbats=FALSE);print(p);dev.off()
 embedded<-paste0(name,'_embedded.pdf');embedFonts(paste0(name,'.pdf'),outfile=embedded);stopifnot(file.rename(embedded,paste0(name,'.pdf')))}
agg_tiff(paste0(name,'.tiff'),width=w,height=h,units='in',res=600,compression='lzw');print(p);dev.off()
agg_png(paste0(name,'.png'),width=w,height=h,units='in',res=200);print(p);dev.off()
writeLines(paste0('Original manuscript index across ',n,' coastal cities. Panel a compares the area-weighted whole-city index with its outside-city ring; panel b compares non-reclaimed and reclaimed urban land in the same cities. Bars are equal-city means; intervals use 1,999 city bootstrap draws. Both panels share an absolute index scale; the grey line marks the common outside-city mean. The percentage is the ratio of the two panel-b means, not a percentage-point difference or a city-averaged ratio. Reclaimed land is defined by the new mapped five-year cohorts, rather than a verified reproduction of historical manuscript polygons. The stored original index is used unchanged, without additional 261-species or FULL/ENV mask intersections. Missing prediction areas are excluded, not set to zero. This model-derived index describes suitability rather than observed invasion events. A separately reconstructed paired FULL product has a different spatial contrast and is retained separately.'),file.path(out,'figure_caption.txt'))
write_json(list(status='R_ORIGINAL_INDEX_FIGURE_RENDERED',n_cities=n,percent_lower=percent,width_mm=183,height_mm=85,
 input_sha256=digest(file=src,algo='sha256'),configuration_sha256=digest(file=a[1],algo='sha256'),visual_review_required=TRUE),file.path(out,'figure_receipt.json'),auto_unbox=TRUE,pretty=TRUE)
