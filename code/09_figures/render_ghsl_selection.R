# Display source-frame comparisons using original-value axes and explicit denominators.
# Usage: Rscript render_ghsl_selection.R comparison_directory output_directory [pdf,svg,png]
suppressPackageStartupMessages({library(ggplot2);library(patchwork)})
args<-commandArgs(TRUE);src<-args[1];out<-args[2]
formats<-if(length(args)>=3)strsplit(args[3],',',fixed=TRUE)[[1]]else c('pdf','svg','png')
stopifnot(all(formats %in% c('pdf','svg','png')))
dir.create(out,recursive=TRUE,showWarnings=FALSE)
u<-read.csv(file.path(src,'unique_centre_analysis_data.csv'))
d<-read.csv(file.path(src,'covariate_group_comparisons.csv'))
d<-d[d$frame=='unique_centres'&d$family=='primary',]
c<-read.csv(file.path(src,'continent_comparisons.csv'))
c<-c[c$frame=='unique_centres',];names(c)[names(c)=='Var1']<-'continent';names(c)[names(c)=='Var2']<-'group'
c$percent<-100*c$Freq/ave(c$Freq,c$group,FUN=sum)
c$continent<-factor(c$continent,levels=rev(c('Africa','Asia','Europe','North America','Oceania','South America')))
c$group<-factor(c$group,levels=c('Linked','Unlinked'));u$group<-factor(u$group,levels=c('Linked','Unlinked'))
stopifnot(nrow(u)==131,all(table(u$group)==c(102,29)))
font<-'Arial';regular<-Sys.getenv('GHSL_FONT_REGULAR');bold<-Sys.getenv('GHSL_FONT_BOLD')
if(nzchar(regular))systemfonts::register_font(font,plain=regular,bold=if(nzchar(bold))bold else regular)
colours<-c(Linked='#397F98',Unlinked='#A5AFB5')
probability_label<-function(x){
 if(x<.001){
  exponent<-floor(log10(x));mantissa<-format(signif(x/10^exponent,3),trim=TRUE)
  return(paste0(mantissa,' %*% 10^{',exponent,'}'))
 }
 format(signif(x,3),trim=TRUE,scientific=FALSE)
}
comparison_label<-function(variable){
 row<-d[d$variable==variable,]
 stopifnot(nrow(row)==1,is.finite(row$p),is.finite(row$q),
           row$n_linked==102,row$n_unlinked==29,row$p>0,row$q>0)
 mark<-if(row$q<.001)'***' else if(row$q<.01)'**' else if(row$q<.05)'*' else 'ns'
 parse(text=paste0('paste(italic(P) == ',probability_label(row$p),
                   ', "; ", italic(q) == ',probability_label(row$q),', " ',mark,'")'))
}
short_labels<-function(x)vapply(x,function(v){
 if(!is.finite(v))return(NA_character_)
 a<-abs(v);unit<-if(a>=1e9*(1-1e-8))1e9 else if(a>=1e6*(1-1e-8))1e6 else if(a>=1e3*(1-1e-8))1e3 else 1
 paste0(format(signif(v/unit,3),trim=TRUE,scientific=FALSE),switch(as.character(unit),'1e+09'='B','1e+06'='M','1000'='K',''))
},character(1))
theme_set(theme_classic(base_size=7,base_family=font)+theme(
 axis.line=element_line(linewidth=.3),axis.ticks=element_line(linewidth=.3),
 axis.title=element_text(size=7),axis.text=element_text(size=6),
 plot.title=element_text(size=7,face='bold'),plot.subtitle=element_text(size=6),
 plot.tag=element_text(size=8,face='bold'),legend.title=element_blank(),
 legend.text=element_text(size=6.5),legend.position='bottom',plot.margin=margin(5,7,5,5)))
settings<-list(
 c('latitude','Latitude','Degrees',1,'linear'),
 c('area_km2','Urban-centre area','Area (km2)',1,'log'),
 c('mapped_reclamation_ha','Mapped reclaimed area','Area (ha)',1,'log'),
 c('mapped_reclamation_fraction','City area reclaimed','Reclaimed / city area (%)',100,'log'),
 c('ghsl_gdp_average_ppp_2020','GDP grid indicator','Mean grid value (PPP)',1,'log'),
 c('ghsl_population_2020','Population','People',1,'log'),
 c('ghsl_built_fraction_2020','City area built up','Built-up / city area (%)',100,'linear'),
 c('night_light_mean_2023','Night-light intensity','Mean grid value',1,'log'),
 c('gbif_all_available','All-plant records','Number of records',1,'log'))
panels<-lapply(seq_along(settings),function(i){
 s<-settings[[i]];v<-u;v$value<-v[[s[1]]]*as.numeric(s[4])
 p<-ggplot(v,aes(group,value,colour=group,fill=group))+
  geom_boxplot(width=.5,outlier.shape=NA,linewidth=.35,alpha=.18)+
  geom_point(position=position_jitter(width=.14,height=0,seed=40+i),alpha=.45,size=.65)+
  scale_colour_manual(values=colours)+scale_fill_manual(values=colours)+
  scale_x_discrete(labels=c(Linked='Selected cities',Unlinked='Excluded cities'))+
  labs(x=NULL,y=s[3],title=s[2],subtitle=comparison_label(s[1]))+theme(legend.position='none')
 if(s[5]=='log'){
  ticks<-10^seq(-2,12);ticks<-ticks[ticks>=min(v$value[v$value>0])&ticks<=max(v$value)]
  if(length(ticks)>5)ticks<-ticks[seq(1,length(ticks),by=ceiling(length(ticks)/5))]
  if(any(v$value==0))ticks<-c(0,ticks)
  if(s[1]=='mapped_reclamation_ha')ticks<-c(0,10,100,1000,10000)
  sigma<-if(s[1]=='mapped_reclamation_fraction').01 else 1
  if(s[1]=='mapped_reclamation_fraction')ticks<-c(0,.01,.1,1,10)
  p<-p+scale_y_continuous(trans=scales::pseudo_log_trans(base=10,sigma=sigma),breaks=ticks,
   labels=short_labels)
 }
 p
})
p<-wrap_plots(panels,ncol=3)+plot_annotation(tag_levels='a',
 title='Comparison of selected and excluded cities',
 subtitle='102 selected and 29 excluded cities | One point per GHSL urban centre',
 caption='P: two-sided Wilcoxon test; q: BH-adjusted P across 10 comparisons.\n* q < 0.05; ** q < 0.01; *** q < 0.001; ns: not significant (q >= 0.05).',
 theme=theme(plot.title=element_text(size=7,face='bold'),plot.subtitle=element_text(size=6.5),
             plot.caption=element_text(size=6,hjust=0,lineheight=1.1)))
pd<-position_dodge(width=.68)
geography<-ggplot(c,aes(percent,continent,fill=group))+geom_col(position=pd,width=.6)+
 geom_text(aes(label=Freq),position=pd,hjust=-.3,size=2.2,family=font)+
 scale_fill_manual(values=colours,labels=c('Selected cities (n = 102)','Excluded cities (n = 29)'))+
 scale_x_continuous(expand=expansion(mult=c(0,.18)))+
 labs(x='Urban units within each group (%)',y=NULL,title='Geographic distribution of selected and excluded cities',
 subtitle='Labels show the number of urban units')
save_figure<-function(plot,name,width,height){
 for(ext in formats){
  path<-file.path(out,paste0(name,'.',ext))
  if(ext=='pdf'){
   if(!capabilities('cairo'))stop('PDF export requires an R build with Cairo; export SVG/PNG separately when unavailable.')
   ggsave(path,plot,width=width,height=height,units='mm',device=cairo_pdf,family=font)
  }
  if(ext=='svg')ggsave(path,plot,width=width,height=height,units='mm',device=svglite::svglite)
  if(ext=='png')ggsave(path,plot,width=width,height=height,units='mm',dpi=300,bg='white',device=ragg::agg_png)
 }
}
save_figure(p,'GHSL_sample_selection',183,172)
save_figure(geography,'GHSL_geographic_composition',145,95)
write.csv(u,file.path(out,'figure_city_values.csv'),row.names=FALSE)
write.csv(d,file.path(out,'figure_numeric_source.csv'),row.names=FALSE)
write.csv(c,file.path(out,'figure_continent_source.csv'),row.names=FALSE)
writeLines(c('Comparison of selected and excluded coastal cities using harmonized urban boundaries.',
 'a-i, Distributions of latitude (a), urban-centre area (b), mapped reclaimed area (c), reclaimed percentage (d), GDP grid indicator (e), population (f), built-up percentage (g), night-light intensity (h) and all-plant occurrence records (i). Points represent independent Global Human Settlement Layer (GHSL) urban centres: 102 selected and 29 excluded centres in every panel. Selected centres contain at least one source city included in the study sample; excluded centres contain none. Of 135 source-city records, 134 were matched to 131 distinct centres; shared centres were counted once.',
 'Boxes show medians and the 25th-75th percentiles; whiskers extend to the most extreme observations within 1.5 times the interquartile range on the plotted scale. All observations are shown. Axes retain original-value labels with pseudo-logarithmic spacing, except latitude and built-up percentage. Reclaimed and built-up percentages use the corresponding GHSL urban-centre area as their denominator. K, M and B denote thousand, million and billion.',
 'Panel annotations report unadjusted P values from two-sided Wilcoxon rank-sum tests (normal approximation with continuity correction) and Benjamini-Hochberg-adjusted P values (q) across ten primary comparisons: the nine variables shown and continent composition. Significance is based on q: *q < 0.05, **q < 0.01, ***q < 0.001; ns, not significant (q >= 0.05).',
 'Population, GDP and built-up indicators refer to 2020; night lights to 2023. GDP denotes the published mean GDP PPP grid indicator, not municipal GDP or verified per-capita GDP. GBIF counts include all available Plantae records passing the coordinate and presence filters, retrieved in October 2026.',
 'The companion geography figure shows within-group continent percentages and counts. The full comparison table retains standardized differences, bootstrap intervals, rank-sum and continent tests, and multiplicity adjustments. The distribution plots do not replace these calculations.',
 'All 135 source records remain in the roster: 134 matched records correspond to these 131 independent units. Tokoname is unmatched. Shared centres count once. Original manuscript boundaries are unchanged.',
 'R output; Arial font; editable SVG and embedded-font PDF. No model fitting or data selection was performed for the display.'),file.path(out,'FIGURE_LEGEND.txt'))
