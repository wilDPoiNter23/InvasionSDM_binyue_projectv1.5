# Regional exclusions using archived city-background summaries, without SDM fitting.
# Usage: Rscript urban_hotspot_region_exclusion.R CONFIG.json
suppressPackageStartupMessages({library(ggplot2);library(patchwork);library(jsonlite);library(digest)})
args<-commandArgs(TRUE);stopifnot(length(args)==1L)
cfg<-fromJSON(paste(readLines(args[1],warn=FALSE,encoding='UTF-8'),collapse='\n'));out<-cfg$output_root
if(dir.exists(out))stop('Existing regional results retained; use a fresh output_root.')
for(p in c('svglite','ragg'))stopifnot(requireNamespace(p,quietly=TRUE))
for(dir in c('tables','figures','logs'))dir.create(file.path(out,dir),recursive=TRUE,showWarnings=FALSE)
key<-function(x)tolower(trimws(x))
d<-read.csv(cfg$city_source,check.names=FALSE)
g<-read.csv(cfg$geographic_crosswalk,check.names=FALSE)
stopifnot(nrow(d)==108L,!anyDuplicated(key(d$City)),!anyDuplicated(key(g$city)))
ix<-match(key(d$City),key(g$city));stopifnot(!anyNA(ix))
d$country<-g$country[ix];d$continent<-g$verified_continent[ix]
stopifnot(all(is.finite(as.matrix(d[,c('City_Pressure','Background_Pressure','Delta_Pressure')]))),
 all(d$Background_Pressure>0),!anyNA(d$continent),
 abs(mean(d$Delta_Pressure)-.181)<.0005,
 max(abs(d$Delta_Pressure-(d$City_Pressure-d$Background_Pressure)))<=.00101)
# Preserve the stored, rounded differences used by the original one-sample test.
# Ratios of group means provide an explicitly defined percentage summary.
groups<-list(All_cities=rep(TRUE,nrow(d)),Without_Europe=d$continent!='Europe',
 Without_North_America=d$continent!='North America',
 Without_both=!d$continent%in%c('Europe','North America'))
labels<-c('All cities','Excluding Europe','Excluding North America','Excluding both regions')
B<-as.integer(cfg$bootstrap_replicates);stopifnot(B>=9999L)
set.seed(cfg$seed);results<-draws<-members<-list()
for(i in seq_along(groups)){
 z<-d[groups[[i]],];n<-nrow(z);stopifnot(n>=2)
 percent<-100*(mean(z$City_Pressure)/mean(z$Background_Pressure)-1)
 boot<-replicate(B,{j<-sample.int(n,n,replace=TRUE)
  100*(mean(z$City_Pressure[j])/mean(z$Background_Pressure[j])-1)})
 stopifnot(all(is.finite(boot)))
 ci<-quantile(boot,c(.025,.975),names=FALSE)
 tt<-t.test(z$Delta_Pressure,mu=0,alternative='two.sided')
 manual_t<-mean(z$Delta_Pressure)/(sd(z$Delta_Pressure)/sqrt(n))
 stopifnot(abs(unname(tt$statistic)-manual_t)<1e-10,
 abs(tt$p.value-2*pt(-abs(manual_t),df=n-1))<1e-12)
 results[[i]]<-data.frame(group=names(groups)[i],label=labels[i],n_cities=n,
  n_positive=sum(z$Delta_Pressure>0),mean_city=mean(z$City_Pressure),
  mean_background=mean(z$Background_Pressure),percent=percent,
  percent_lower=ci[1],percent_upper=ci[2],mean_difference=mean(z$Delta_Pressure),
  difference_lower=tt$conf.int[1],difference_upper=tt$conf.int[2],
  t=unname(tt$statistic),df=unname(tt$parameter),p=tt$p.value)
 draws[[i]]<-data.frame(group=names(groups)[i],replicate=seq_len(B),percent=boot)
 members[[i]]<-data.frame(group=names(groups)[i],city=d$City,country=d$country,
  continent=d$continent,included=groups[[i]])
}
s<-do.call(rbind,results);s$p_holm<-p.adjust(s$p,'holm')
adjustment_statement<-if(all(s$p_holm<.05)) 'All four tests remain significant after adjustment.' else 'Adjusted P values should be consulted when interpreting the four tests together.'
stopifnot(identical(s$n_cities,c(108L,99L,103L,94L)),
 identical(groups[[4]],groups[[2]]&groups[[3]]))
write.csv(d,file.path(out,'tables','archived_city_source_with_geography.csv'),row.names=FALSE)
write.csv(s,file.path(out,'tables','regional_hotspot_summary.csv'),row.names=FALSE)
write.csv(do.call(rbind,members),file.path(out,'tables','regional_membership.csv'),row.names=FALSE)
write.csv(do.call(rbind,draws),file.path(out,'tables','city_pair_bootstrap.csv'),row.names=FALSE)
reproduction<-data.frame(n=108,stored_mean_difference=mean(d$Delta_Pressure),
 manuscript_mean_difference=.181,reported_percent=122,
 ratio_of_means_percent=s$percent[1],mean_of_city_percent=mean(100*(d$City_Pressure/d$Background_Pressure-1)),
 reported_percent_reproduced=abs(s$percent[1]-122)<.5)
write.csv(reproduction,file.path(out,'tables','manuscript_reproduction_check.csv'),row.names=FALSE)

# A single forest plot with aligned labels and numerical annotations.
s$y<-4:1;s$colour<-c('#454B50','#858C91','#858C91','#167D8D')
s$effect_label<-sprintf('%.1f%% [%.1f, %.1f]',s$percent,s$percent_lower,s$percent_upper)
s$p_label<-paste0('P = ',formatC(s$p,format='e',digits=2))
base<-theme_void(base_size=7,base_family='Arial')+
 theme(plot.margin=margin(3,2,3,2),text=element_text(colour='#20262B'))
ys<-scale_y_continuous(limits=c(.45,4.8),expand=c(0,0))
left<-ggplot(s,aes(y=y))+geom_text(aes(x=0,label=label),hjust=0,size=2.45,family='Arial')+
 geom_text(aes(x=0,label=paste0('n = ',n_cities,' cities')),nudge_y=-.23,hjust=0,size=2.3,colour='#60676C',family='Arial')+
 annotate('text',x=0,y=4.65,label='City sample',hjust=0,fontface='bold',size=2.45,family='Arial')+
 scale_x_continuous(limits=c(0,1),expand=c(0,0))+ys+base
xmax<-ceiling(max(s$percent_upper)/25)*25
middle<-ggplot(s,aes(y=y,colour=I(colour)))+
 geom_vline(xintercept=0,linetype='dashed',linewidth=.35,colour='#A4A9AD')+
 geom_segment(aes(x=percent_lower,xend=percent_upper,yend=y),linewidth=.65)+
 geom_point(aes(x=percent),size=2.5)+
 scale_x_continuous(limits=c(-3,xmax),breaks=seq(0,xmax,50),expand=c(0,0))+
 ys+labs(x='Higher suitability within cities (%)')+
 theme_classic(base_size=7,base_family='Arial')+
 theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),
  axis.line.y=element_blank(),axis.line.x=element_line(linewidth=.35),axis.ticks=element_line(linewidth=.3),
  plot.margin=margin(3,4,3,4),axis.title.x=element_text(size=7,margin=margin(t=6)))
right<-ggplot(s,aes(y=y))+
 geom_text(aes(x=0,label=effect_label),hjust=0,size=2.45,family='Arial')+
 geom_text(aes(x=0,label=p_label),nudge_y=-.23,hjust=0,size=2.3,colour='#60676C',family='Arial')+
 annotate('text',x=0,y=4.65,label='Estimate [95% CI]',hjust=0,fontface='bold',size=2.45,family='Arial')+
 scale_x_continuous(limits=c(0,1),expand=c(0,0))+ys+base
fig<-left+middle+right+plot_layout(widths=c(2.5,3.8,2.5))
stem<-file.path(out,'figures','urban_hotspot_region_exclusion')
ggsave(paste0(stem,'.pdf'),fig,width=183,height=88,units='mm',device=cairo_pdf,bg='white')
ggsave(paste0(stem,'.svg'),fig,width=183,height=88,units='mm',device=svglite::svglite,bg='white')
ggsave(paste0(stem,'.png'),fig,width=183,height=88,units='mm',dpi=300,device=ragg::agg_png,bg='white')
legend<-c('Urban suitability contrasts after excluding European and North American cities.',
 paste0('Points show the percentage difference between mean city suitability and mean suitability in the surrounding 50-km rings, calculated as 100 x (mean city / mean background - 1). Cities receive equal weight. Bars show 95% percentile confidence intervals from ',B,' resamples of complete city-background pairs. Sample sizes are the numbers of cities. P values are from two-sided one-sample t-tests of the archived city-minus-background differences against zero, retaining the original test and stored rounding. These P values test the absolute paired differences; the intervals describe the percentage statistic. Nominal P values are displayed; Holm-adjusted values for the four tests are provided in the source table. ',adjustment_statement,' Groups overlap and are not independent datasets. The original 108-city summaries and boundaries are retained; no SDM or spatial extraction was rerun.'),
 'The archived source reproduces the manuscript mean difference of 0.181 but yields 111.9% under the displayed ratio-of-means definition. The manuscript value of 122% was not reproduced and is not used to calibrate the plotted estimates.')
writeLines(legend,file.path(out,'figures','title_and_legend.txt'))
writeLines(c('Role: geographic robustness of the original urban-background contrast.',
 'Archetype: quantitative forest plot with direct labels.',
 'Backend: R only; 183 x 88 mm; Arial; editable PDF and SVG; PNG preview.',
 'Evidence: original archived 108 city pairs; only geographic membership changes.',
 'Percentage provenance: explicitly defined ratio of means; 122% identity remains unresolved.'),file.path(out,'figures','figure_contract.txt'))
# Read through a connection to avoid Windows removable-drive owner lookups.
hash_file<-function(p){con<-file(p,'rb');on.exit(close(con));
 digest(readBin(con,'raw',n=file.info(p,extra_cols=FALSE)$size),algo='sha256',serialize=FALSE)}
write_json(list(status='COMPLETED_WITH_UNRESOLVED_MANUSCRIPT_PERCENTAGE',
 inputs=lapply(cfg[c('city_source','geographic_crosswalk','original_analysis_script')],function(p)list(path=p,sha256=hash_file(p))),
 configuration_sha256=hash_file(args[1]),bootstrap_replicates=B,seed=cfg$seed,
 original_test='Two-sided one-sample t-test of stored Delta_Pressure against zero',
 plotted_statistic='100*(mean(City_Pressure)/mean(Background_Pressure)-1)',
 reported_122_percent_reproduced=FALSE,manuscript_difference_reproduced=TRUE,
 sdms_fitted=0,rasters_processed=0,city_boundaries_changed=FALSE,
 checks=c('108 unique complete pairs','Every city matched to verified geography','Region counts and joint exclusion','Stored difference rounding','Original mean difference','Manual t-test identity'),
 software=list(R=R.version.string,ggplot2=as.character(packageVersion('ggplot2')))),
 file.path(out,'receipt.json'),auto_unbox=TRUE,pretty=TRUE)
capture.output(sessionInfo(),file=file.path(out,'logs','sessionInfo.txt'))
print(s,row.names=FALSE)
