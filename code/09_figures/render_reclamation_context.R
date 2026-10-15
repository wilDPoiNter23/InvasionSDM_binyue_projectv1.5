# Render matched land-zone and prespecified human-context associations in R.
suppressPackageStartupMessages({library(ggplot2);library(patchwork);library(jsonlite);library(svglite);library(ragg);library(digest)})
a<-commandArgs(TRUE);stopifnot(length(a)==1L);cfg<-read_json(a[1],simplifyVector=TRUE)
out<-cfg$output_root;dir.create(out,recursive=TRUE,showWarnings=FALSE)
dir.create(file.path(out,'source_data'),showWarnings=FALSE)
font<-if(!capabilities('cairo'))'Helvetica'else if('Arial'%in%systemfonts::system_fonts()$family)'Arial'else'Liberation Sans'
stopifnot(font=='Helvetica'||font%in%systemfonts::system_fonts()$family)
if(!capabilities('cairo'))stopifnot(nzchar(Sys.which('gs')))
theme_set(theme_classic(base_size=6.5,base_family=font)+theme(axis.text=element_text(size=6,colour='black'),
 axis.line=element_line(linewidth=.3),axis.ticks=element_line(linewidth=.3),strip.text=element_text(size=6.5),
 plot.title=element_text(size=7,face='bold'),plot.tag=element_text(size=8,face='bold'),
 legend.title=element_blank(),legend.text=element_text(size=6),legend.position='bottom',plot.margin=margin(5,6,5,5)))
cols<-c(FULL='#356C91',ENV='#29877C');provenance<-list();exports<-list();captions<-character()
read_source<-function(root,name,label){f<-file.path(root,name);d<-read.csv(f,stringsAsFactors=FALSE)
 write.csv(d,file.path(out,'source_data',paste0(label,'.csv')),row.names=FALSE)
 provenance[[label]]<<-list(file=name,sha256=digest(file=f,algo='sha256'),rows=nrow(d));d}
save_figure<-function(p,name,height){w<-183/25.4;h<-height/25.4
 svglite(file.path(out,paste0(name,'.svg')),width=w,height=h);print(p);dev.off()
 pdf_file<-file.path(out,paste0(name,'.pdf'))
 if(capabilities('cairo')) {cairo_pdf(pdf_file,width=w,height=h,family=font);print(p);dev.off()}else{
  pdf(pdf_file,width=w,height=h,family='Helvetica',useDingbats=FALSE);print(p);dev.off()
  embedded<-file.path(out,paste0(name,'_embedded.pdf'));embedFonts(pdf_file,outfile=embedded)
  stopifnot(file.exists(embedded),file.info(embedded)$size>1000,file.rename(embedded,pdf_file))}
 agg_tiff(file.path(out,paste0(name,'.tiff')),width=w,height=h,units='in',res=600,compression='lzw');print(p);dev.off()
 agg_png(file.path(out,paste0(name,'.png')),width=w,height=h,units='in',res=200);print(p);dev.off()
 exports[[name]]<<-list(width_mm=183,height_mm=height)}

if(!is.null(cfg$baseline_root)) {
 d<-read_source(cfg$baseline_root,'common_baseline_city_source.csv','land_zone_city_source')
 ef<-read_source(cfg$baseline_root,'zone_effects.csv','land_zone_effects')
 verified<-read_json(file.path(cfg$baseline_root,'verification.json'),simplifyVector=TRUE)
 stopifnot(grepl('PASS',verified$status))
 eligibility<-subset(d,minimum_cell_equivalents==1&zone=='OUTSIDE_RING'&group=='FULL')
 ns<-table(eligibility$support_policy)
 priorities<-c('FIXED_261','STRICT_684','PAIRED_AVAILABLE')
 eligible<-priorities[priorities%in%names(ns)]
 eligible<-eligible[vapply(eligible,function(x)ns[[x]]>=20,logical(1))]
 if(!length(eligible))stop('No prespecified community support has at least 20 matched cities')
 policy<-eligible[1]
 q<-subset(d,support_policy==policy&minimum_cell_equivalents==1)
 stopifnot(nrow(q)>0,length(unique(q$city_id))>=20)
 stopifnot(all(table(q$city_id)==6L))
 n<-length(unique(q$city_id));zlabels<-c(OUTSIDE_RING='Outside-city\nring',NON_RECLAIMED_CITY='Other urban\nland',RECLAIMED_CITY='Reclaimed\nurban land')
 q$zone<-factor(q$zone,levels=names(zlabels),labels=zlabels)
 mu<-aggregate(mean_suitability~group+zone,q,mean)
 pmeans<-ggplot(q,aes(zone,mean_suitability,group=city_id))+geom_line(colour='grey75',linewidth=.25,alpha=.55)+
 geom_point(aes(colour=group),size=.65,alpha=.6)+geom_line(data=mu,aes(group=group),colour='black',linewidth=.8)+
 geom_point(data=mu,aes(group=group),colour='black',size=1.6)+facet_wrap(~group,nrow=1)+scale_colour_manual(values=cols)+
 labs(x=NULL,y='Current predicted suitability',title=paste0('Same ',n,' cities and three disjoint land zones'))+theme(legend.position='none')
 e<-subset(ef,support_policy==policy&minimum_cell_equivalents==1)
 labels<-c(CITY_MINUS_RING='Whole city - ring',NON_RECLAIMED_MINUS_RING='Other urban land - ring',RECLAIMED_MINUS_RING='Reclaimed land - ring',RECLAIMED_MINUS_NON_RECLAIMED='Reclaimed - other urban land')
 labels<-labels[names(labels)%in%e$contrast]
 e$label<-factor(e$contrast,levels=rev(names(labels)),labels=rev(labels))
 # Explicit offsets prevent overlap without changing the numerical axis.
 e$yy<-as.numeric(e$label)+ifelse(e$group=='FULL',.1,-.1)
 pdelta<-ggplot(e,aes(mean_difference,yy,colour=group))+geom_vline(xintercept=0,linetype=2,linewidth=.3,colour='grey60')+
 geom_segment(aes(x=CI_low,xend=CI_high,yend=yy),linewidth=.65)+geom_point(size=1.5)+
 scale_y_continuous(breaks=seq_along(levels(e$label)),labels=levels(e$label))+
 scale_colour_manual(values=cols)+labs(x='Within-city suitability difference',y=NULL,title='Common outside-city reference; 95% city-bootstrap intervals')
 save_figure((pmeans/pdelta)+plot_layout(heights=c(1.25,1))+plot_annotation(tag_levels='a'),'common_land_baseline',128)
 writeLines(paste('Primary policy selected by coverage only:',policy,'; cities:',n),file.path(out,'land_zone_primary_sample.txt'))
 captions<-c(captions,paste0('Common land baseline. Three disjoint zones are compared in the same ',n,' cities under ',policy,' support and at least one native-cell equivalent per zone. Selection depends only on community coverage: fixed 261-species support is preferred when supplied and at least 20 cities qualify, followed by strict 684-species and then paired-available support. FIXED_261 means every retained cell has finite paired predictions for all members of the pre-existing 261-species coverage roster. Grey lines are city means and black lines are equal-city means; lower panels show paired differences and 95% intervals from 1,999 city bootstrap draws. Within zones, native geodesic cell areas times exact polygon fractions weight the means; FULL and ENV use identical eligible cells and weights. Reclaimed land uses the mapped five-year reclamation cohorts, not the historical manuscript zone definition. The outside reference is the EPSG:6933 projected 50-km buffer minus all 108 city polygons, restricted to finite modeled support; no independent shoreline mask was available. Paired-available support can contain different contributing species across cells and its counts are provided. Ring=100 city-specific indices and all computed support policies are retained in source tables; the plotted absolute differences avoid unstable division by very small ring values. These are spatial predictions, not causal effects.'))
}

if(!is.null(cfg$human_root)) {
 d<-read_source(cfg$human_root,'associations.csv','human_associations')
 read_source(cfg$human_root,'analysis_samples.csv','human_analysis_samples')
 verify<-read_json(file.path(cfg$human_root,'verification.json'),simplifyVector=TRUE)
 stopifnot(grepl('PASS',verify$status))
 labels<-c(BUILT_S_E2025_2.5min='Built-up surface',GDP2020_2.5min='GDP',Pop2020_2.5min='Population',VIIRS_2023_2.5min='Night-time light')
 d$label<-factor(labels[d$indicator],levels=rev(unname(labels)))
 titles<-c(SPEARMAN_AMOUNT='Unadjusted land amount',AREA_CONTROLLED_AMOUNT='Land amount, area controlled',AREA_CONTROLLED_FRACTION='Land fraction, area controlled')
 plots<-lapply(names(titles),function(fam){z<-d[d$family==fam,];stopifnot(nrow(z)==4,all(z$status=='FITTED'))
  ggplot(z,aes(estimate,label))+geom_vline(xintercept=0,linetype=2,colour='grey60',linewidth=.3)+
  geom_segment(aes(x=lower,xend=upper,yend=label),colour='#356C91',linewidth=.65)+geom_point(size=1.6)+
  geom_text(aes(label=paste0('q=',formatC(p_bh,format='g',digits=2),'; n=',n)),x=Inf,hjust=1,vjust=-.7,size=1.85,family=font)+
  scale_x_continuous(expand=expansion(mult=c(.15,.3)))+
  labs(x=if(fam=='SPEARMAN_AMOUNT')'Spearman correlation'else'Standardized log-scale slope',y=NULL,title=titles[[fam]])})
 save_figure(wrap_plots(plots,nrow=1)+plot_annotation(tag_levels='a'),'reclamation_human_context',86)
 captions<-c(captions,'Human-activity context. All four prespecified raw human indicators are shown. Unadjusted associations are Spearman correlations with 95% city-bootstrap intervals (1,999 draws). Area-controlled models regress standardized log(1 + indicator) on standardized log(1 + reclamation amount or fraction) and log(1 + city area); intervals use HC3 heteroskedasticity-robust covariance and residual-df t quantiles. Reclamation fraction converts hectares to square kilometres before division by city area. q values apply BH adjustment within each four-indicator analysis family; n is the indicator-specific city count. Indicator reference years differ (built-up 2025, GDP/population 2020, night light 2023). These associations describe covariation and do not establish a causal sequence, habitat fragmentation or mediation. Source tables include all planned tests.')
}

if(!is.null(cfg$binary_root)) {
 d<-read_source(cfg$binary_root,'reclamation_contrasts.csv','binary_fraction_contrasts')
 check<-read_json(file.path(cfg$binary_root,'association_check.json'),simplifyVector=TRUE)
 stopifnot(grepl('PASS',check$status))
 q<-subset(d,region=='CITY'&response=='mean_species_suitable_fraction')
 stopifnot(nrow(q)==4L,all(q$threshold%in%c('T10','MAX_TSS')),all(q$group%in%c('FULL','ENV')))
 q$label<-factor(paste(q$group,q$threshold),levels=rev(c('FULL T10','FULL MAX_TSS','ENV T10','ENV MAX_TSS')))
 p<-ggplot(q,aes(100*mean_q75_minus_q25,label,colour=group))+
 geom_vline(xintercept=0,linetype=2,colour='grey60',linewidth=.3)+
 geom_segment(aes(x=100*lower,xend=100*upper,yend=label),linewidth=.7)+geom_point(size=1.8)+
 scale_colour_manual(values=cols)+theme(legend.position='none')+
 labs(x='Suitable-area fraction contrast (percentage points)',y=NULL,
 title='Greater reclamation is associated with a lower suitable-area fraction')
 save_figure(p,'binary_suitable_fraction',82)
 captions<-c(captions,'Suitable-area fraction. Four prespecified contrasts compare reclamation amount at its 75th versus 25th percentile in the existing city GAMs (84 complete cities, 261 coverage-core species). Each species fraction divides its threshold-exceeding area by its own jointly finite FULL/ENV city support; the response is the equal-species mean of these fractions, not the area suitable for at least one species. T10 is the species-specific ten-percent omission threshold; MAX_TSS is the species-specific maximum-TSS threshold. Bars are existing model-based 95% Wald intervals, expressed in percentage points. Both FULL and ENV models use their corresponding stored species products; ENV excludes the specified human predictors including reclamation. This is a conditional model contrast rather than an intervention effect. The full source table and support definitions are retained.')
}

if(!is.null(cfg$configuration_root)) {
 d<-read_source(cfg$configuration_root,'configuration.csv','mapped_configuration')
 r<-read_source(cfg$configuration_root,'associations.csv','configuration_associations')
 verify<-read_json(file.path(cfg$configuration_root,'verification.json'),simplifyVector=TRUE)
 stopifnot(grepl('PASS',verify$status))
 z<-subset(d,cutoff_ha==10&retained_area_ha>0)
 pshape<-ggplot(z,aes(retained_area_ha,effective_patch_number))+geom_point(size=1.2,alpha=.65,colour='#356C91')+
 scale_x_log10()+scale_y_log10()+labs(x='Retained mapped reclamation area (ha)',y='Effective mapped component number',
 title=paste0('Mapped configuration at 10-ha cutoff (',nrow(z),' cities)'))
 fitted<-subset(r,status=='FITTED')
 metriclabels<-c(n_retained='Component count',effective_patch_number='Effective component number',largest_patch_fraction='Largest component share')
 fitted$metriclabel<-factor(metriclabels[fitted$metric],levels=rev(unname(metriclabels)))
 fitted$yy<-as.numeric(fitted$metriclabel)+c('1'=-.17,'10'=0,'100'=.17)[as.character(fitted$cutoff_ha)]
 fitted$case<-ifelse(fitted$model=='M0','Area-adjusted richness','Record-intensity sensitivity')
 pconfig<-ggplot(fitted,aes(estimate,yy,colour=factor(cutoff_ha)))+
 geom_vline(xintercept=0,linetype=2,colour='grey60',linewidth=.3)+geom_segment(aes(x=lower,xend=upper,yend=yy),linewidth=.55)+
 geom_point(size=1.3)+facet_wrap(~case,nrow=1)+scale_y_continuous(breaks=1:3,labels=levels(fitted$metriclabel))+
 scale_colour_manual(values=c('1'='#8C8C8C','10'='#356C91','100'='#29877C'),name='Analyst size cutoff (ha)')+
 labs(x='Standardized configuration association (95% interval)',y=NULL,title='Recorded-richness associations at all three cutoffs')
 save_figure((pshape/pconfig)+plot_layout(heights=c(1,1.15))+plot_annotation(tag_levels='a'),'mapped_reclamation_configuration',128)
 captions<-c(captions,'Mapped reclamation configuration. All mapped periods were dissolved before connected polygon components were counted within cities. The upper panel shows retained area and the effective component number (squared retained area divided by the sum of squared component areas) at the 10-ha analyst cutoff. Lower panels show all fitted associations for three configuration metrics and all 1/10/100-ha cutoffs, controlling log city area and log canonical reclamation amount. M0 uses area-adjusted raw recorded richness; M1 additionally adjusts the richness baseline for target-record intensity. Intervals are HC3 residual-df t intervals conditional on the estimated richness baseline; all planned tests, sample sizes and BH values (nine tests per response family) are supplied in configuration_associations.csv. No component-above-cutoff units are excluded from ratio associations rather than treated as uninvaded. These are mapped reclamation components, not verified habitat patches; city clipping, unknown source minimum mapping unit and differing source periods limit interpretation. The analysis does not establish that reclamation caused habitat fragmentation or that configuration caused invasion.')
}
writeLines(captions,file.path(out,'figure_captions.txt'))
write_json(list(status='R_CONTEXT_FIGURES_RENDERED',exports=exports,sources=provenance,
 configuration_sha256=digest(file=a[1],algo='sha256'),visual_review_required=TRUE),file.path(out,'figure_receipt.json'),auto_unbox=TRUE,pretty=TRUE)
writeLines(capture.output(sessionInfo()),file.path(out,'sessionInfo.txt'))
