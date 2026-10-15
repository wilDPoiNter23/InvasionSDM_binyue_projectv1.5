# Put four original-index land summaries on one outside-city reference.
suppressPackageStartupMessages({library(ggplot2);library(jsonlite);library(svglite);library(ragg);library(digest)})
a<-commandArgs(TRUE);stopifnot(length(a)==1L);cfg<-fromJSON(a[1]);out<-cfg$output_root
stopifnot(!dir.exists(out),fromJSON(file.path(cfg$input_root,'verification.json'))$status=='PASS_FULL_SURFACE_ZONE_ARITHMETIC')
dir.create(out,recursive=TRUE);src<-file.path(cfg$input_root,'common_reference_summary.csv');d<-read.csv(src,stringsAsFactors=FALSE)
d<-d[d$group=='ORIGINAL_SII'&d$minimum_cell_equivalents==0,]
zones<-c('OUTSIDE_RING','CITY_TOTAL','NON_RECLAIMED_CITY','RECLAIMED_CITY');d<-d[match(zones,d$zone),]
stopifnot(nrow(d)==4L,!anyNA(d$zone),length(unique(d$n_cities))==1L,d$outside_reference_100[1]==100,
 max(abs(d$outside_reference_100-100*d$mean_index/d$outside_mean))<1e-8)
percent<-100*(d$mean_index[4]/d$mean_index[3]-1);stopifnot(abs(percent-100*(d$outside_reference_100[4]/d$outside_reference_100[3]-1))<1e-10)
d$label<-factor(1:4,labels=c('Outside cities','Whole cities','Other urban land','Reclaimed land'))
write.csv(d,file.path(out,'figure_source_data.csv'),row.names=FALSE)
font<-if(capabilities('cairo'))'Liberation Sans'else'Helvetica'
p<-ggplot(d,aes(label,outside_reference_100,fill=label))+geom_hline(yintercept=100,colour='grey65',linetype=2,linewidth=.35)+
 geom_col(width=.52)+geom_errorbar(aes(ymin=lower,ymax=upper),width=.12,linewidth=.4)+
 geom_text(aes(y=upper,label=sprintf('%.0f',outside_reference_100)),vjust=-.7,size=2.6,family=font)+
 annotate('text',x=3.5,y=max(d$upper)*1.13,label=sprintf('%.1f%% lower than other urban land',abs(percent)),size=2.7,fontface='bold',family=font)+
 scale_fill_manual(values=c('#ACB5BB','#356C91','#356C91','#29877C'))+
 scale_y_continuous(limits=c(0,max(d$upper)*1.22),breaks=c(0,100,200),expand=expansion(mult=c(0,.01)))+
 labs(x=NULL,y='Mean index (outside-city reference = 100)',title='A smaller urban suitability advantage on reclaimed land',subtitle=paste0('Same ',unique(d$n_cities),' cities; paired city-bootstrap intervals'))+
 theme_classic(base_size=7,base_family=font)+theme(legend.position='none',axis.text=element_text(size=6.5,colour='black'),
 axis.line=element_line(linewidth=.3),plot.title=element_text(size=8,face='bold'),plot.subtitle=element_text(size=7),plot.margin=margin(7,8,6,6))
w<-183/25.4;h<-90/25.4;name<-file.path(out,'common_outside_reference')
svglite(paste0(name,'.svg'),width=w,height=h);print(p);dev.off()
if(capabilities('cairo')){cairo_pdf(paste0(name,'.pdf'),width=w,height=h,family=font);print(p);dev.off()}else{
 pdf(paste0(name,'.pdf'),width=w,height=h,family='Helvetica',useDingbats=FALSE);print(p);dev.off();temp<-paste0(name,'_embedded.pdf');embedFonts(paste0(name,'.pdf'),outfile=temp);stopifnot(file.rename(temp,paste0(name,'.pdf')))}
agg_tiff(paste0(name,'.tiff'),width=w,height=h,units='in',res=600,compression='lzw');print(p);dev.off()
agg_png(paste0(name,'.png'),width=w,height=h,units='in',res=200);print(p);dev.off()
writeLines('Common outside-city reference. All four original-index means use the same cities and native-area weighting within each zone. Equal-city means are divided by the same outside-city mean and multiplied by100; this is a ratio of means, not a mean of within-city ratios. The outside category is fixed at100 by definition. Intervals resample whole cities1999 times. Whole-city land includes reclaimed and other urban land. A lower reclaimed-versus-other-urban index can coexist with a reclaimed index above the outside-city reference. The shared denominator itself does not independently validate the within-city contrast. New mapped reclamation cohorts and the stated city exclusion define this sensitivity sample; the result is not an exact numerical reproduction of the original122percent comparison or a causal reclamation effect. Paired FULL/ENV products use a different response construction and remain separate.',file.path(out,'figure_caption.txt'))
write_json(list(status='R_COMMON_REFERENCE_FIGURE_RENDERED',n_cities=unique(d$n_cities),reclaimed_vs_other_percent=percent,
 width_mm=183,height_mm=90,input_sha256=digest(file=src,algo='sha256'),visual_review_required=TRUE),file.path(out,'figure_receipt.json'),auto_unbox=TRUE,pretty=TRUE)
