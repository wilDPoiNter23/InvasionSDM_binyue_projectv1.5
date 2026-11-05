# Render manuscript comparisons from existing model estimates and source tables.
# Usage: Rscript render_revision_synthesis.R configuration.json [human_activity]
suppressPackageStartupMessages({library(ggplot2);library(patchwork);library(jsonlite);library(svglite);library(ragg);library(digest)})
args<-commandArgs(TRUE);stopifnot(length(args)%in%1:2)
only_human<-length(args)==2L && args[2]=='human_activity'
stopifnot(length(args)==1L || only_human)
cfg<-fromJSON(args[1]);out<-cfg$output_root
dir.create(out,recursive=TRUE,showWarnings=FALSE)
for(d in c('source_data','tables','preview'))dir.create(file.path(out,d),showWarnings=FALSE)
manifest<-list();exports<-list();captions<-character()
hash_file<-function(path){
 con<-file(path,'rb');on.exit(close(con));n<-file.info(path,extra_cols=FALSE)$size
 digest(readBin(con,'raw',n=n),algo='sha256',serialize=FALSE)
}
load_table<-function(root,file,label){
 p<-file.path(root,file);stopifnot(file.exists(p));d<-read.csv(p,stringsAsFactors=FALSE,check.names=FALSE)
 write.csv(d,file.path(out,'source_data',paste0(label,'.csv')),row.names=FALSE,na='')
 manifest[[label]]<<-data.frame(label=label,source=p,sha256=hash_file(p),rows=nrow(d))
 d
}
table_out<-function(d,name)write.csv(d,file.path(out,'tables',paste0(name,'.csv')),row.names=FALSE,na='')
font<-'Arial';stopifnot(font %in% systemfonts::system_fonts()$family,capabilities('cairo'))
cols<-c('FULL 814'='#40454B','FULL 684'='#447C9D','ENV 684'='#C5824B')
theme_set(theme_classic(base_size=7,base_family=font)+theme(
 axis.text=element_text(size=6.8,colour='#252525'),axis.title=element_text(size=7),
 axis.line=element_line(linewidth=.3,colour='#444444'),axis.ticks=element_line(linewidth=.3),
 plot.title=element_text(size=7,face='bold',margin=margin(b=6)),
 plot.subtitle=element_text(size=6.5,colour='#555555',margin=margin(b=5)),
 plot.tag=element_text(size=8,face='bold'),strip.text=element_text(size=7,face='bold'),
 strip.background=element_blank(),legend.position='bottom',legend.title=element_blank(),
 legend.text=element_text(size=6.8),plot.margin=margin(6,8,6,7)))
save_plot<-function(p,name,height,caption,width=183){
 svglite(file.path(out,paste0(name,'.svg')),width=width/25.4,height=height/25.4);print(p);dev.off()
 cairo_pdf(file.path(out,paste0(name,'.pdf')),width=width/25.4,height=height/25.4,family=font);print(p);dev.off()
 agg_png(file.path(out,'preview',paste0(name,'.png')),width=width,height=height,units='mm',res=190,background='white');print(p);dev.off()
 exports[[name]]<<-data.frame(figure=name,width_mm=width,height_mm=height,pdf_bytes=file.info(file.path(out,paste0(name,'.pdf')),extra_cols=FALSE)$size,svg_bytes=file.info(file.path(out,paste0(name,'.svg')),extra_cols=FALSE)$size)
 captions<<-c(captions,paste0('## ',name),caption,'')
}
forest<-function(d,x='estimate',lo='lower',hi='upper',y='label',colour=NULL){
 p<-ggplot(d,aes(x=.data[[x]],y=.data[[y]]))+geom_vline(xintercept=0,linetype=2,linewidth=.3,colour='#999999')
 if(is.null(colour))p<-p+geom_segment(aes(x=.data[[lo]],xend=.data[[hi]],yend=.data[[y]]),linewidth=.65,colour='#447C9D')+geom_point(size=1.7,colour='#447C9D')
 else p<-p+geom_segment(aes(x=.data[[lo]],xend=.data[[hi]],yend=.data[[y]],colour=.data[[colour]]),linewidth=.65)+geom_point(aes(colour=.data[[colour]]),size=1.7)
 p+labs(y=NULL)
}

if(!only_human) {
# Formation cohorts and all prespecified land-use contrasts.
age<-load_table(cfg$current,'coastal_period_geometry_filtered/old_recent_city_source.csv','cohort_city_values')
age_effect<-load_table(cfg$current,'coastal_period_geometry_filtered/old_recent_contrast.csv','cohort_contrasts')
bf<-load_table(cfg$current,'coastal_full814/report_tables_extended/all_response_contrasts.csv','full814_bayesian_contrasts')
bp<-load_table(cfg$current,'coastal_age_use/report_tables/all_response_contrasts.csv','paired_bayesian_contrasts')
table_out(rbind(bf,bp),'T01_all_bayesian_contrasts')
table_out(age_effect,'T02_cohort_contrasts')
use_names<-c(water='Water',comm='Commercial',bare='Bare land',indus='Industrial',resid='Residential',crop='Cropland',public='Public use',grass='Grassland',forest='Forest')
age_panel<-function(wanted_response,title,col){
 d<-subset(age,scope=='all_mapped' & response==wanted_response);stopifnot(nrow(d)==75)
 m<-data.frame(x=c(0,1),y=c(mean(d$old_mean),mean(d$recent_mean)))
 a<-subset(age_effect,scope=='all_mapped' & response==wanted_response)
 ggplot(d)+geom_segment(aes(x=0,xend=1,y=old_mean,yend=recent_mean),colour='#B3B7BB',alpha=.38,linewidth=.25)+
 geom_point(aes(x=0,y=old_mean),size=.55,colour='#A1A7AC',alpha=.55)+geom_point(aes(x=1,y=recent_mean),size=.55,colour='#A1A7AC',alpha=.55)+
 geom_line(data=m,aes(x=x,y=y),linewidth=1,colour=col)+geom_point(data=m,aes(x=x,y=y),size=2.1,colour=col)+
 scale_x_continuous(breaks=c(0,1),labels=c('Earlier reclamation','Recent reclamation'),limits=c(-.2,1.2))+
 coord_cartesian(ylim=c(0,1))+labs(x=NULL,y='Mean predicted suitability',title=title,subtitle=sprintf('75 cities; recent - early = %.3f',a$recent_minus_old))
}
use_panel<-function(d,title,col){
 d<-subset(d,type=='USE_VS_REFERENCE' & case=='primary');stopifnot(nrow(d)==9)
 d$label<-factor(use_names[d$to],levels=rev(unname(use_names)))
 ggplot(d,aes(difference,label))+geom_vline(xintercept=0,linetype=2,linewidth=.3,colour='#999999')+
 geom_segment(aes(x=lower,xend=upper,yend=label),linewidth=.65,colour=col)+geom_point(size=1.7,colour=col)+
 scale_y_discrete(drop=FALSE)+coord_cartesian(xlim=c(-.14,.07))+
 labs(x='Difference from transport land\nLower suitability < 0 > Higher suitability',y=NULL,title=title,subtitle='Points: mean differences; lines: 95% intervals')+
 theme(legend.position='none')
}
p<-(age_panel('FULL_814','FULL: formation cohorts',cols[1])|age_panel('PAIRED_ENV_684','ENV: formation cohorts',cols[3]))/
 (use_panel(bf,'FULL: land use',cols[1])|use_panel(subset(bp,group=='ENV'),'ENV: land use',cols[3]))
save_plot(p+plot_layout(heights=c(.9,1.1))+plot_annotation(tag_levels='a'),'F01_formation_history_and_land_use',175,
 'a,b, Each grey segment joins early and recent cohort means within one city; coloured segments show equal-city means (75 cities). Earlier cohorts have mapped interval endpoints at or before 2000; recent cohorts at or after 2015. FULL 814-species and ENV 684-species rosters are separate analyses. c,d, All nine land-use contrasts relative to transport land from the primary species-level hierarchical models (71 cities); dots are posterior mean contrasts and bars are 95% credible intervals. Negative values mean lower suitability than transport land. FULL uses the extended-chain 814-species roster, with 811 finite contributors; ENV uses 684 species. Native ranges are excluded and genuinely missing predictions are not set to zero. Cohorts compare present-day predictions across formation periods, not repeated observations of the same land. Complete paired FULL and prior/transform sensitivity estimates are supplied in T01.')

# Continuous and binary response checks.
gm<-load_table(cfg$current,'city_gam/model_summary.csv','gam_models');table_out(gm,'T03_continuous_gam_models')
gp<-load_table(cfg$current,'city_gam/endpoint_profiles.csv','gam_comparison_profiles')
gp<-subset(gp,model=='G0' & mode=='COMMON_ABSOLUTE_IQR')
gp$group<-c(ARCHIVED_GAM_RISK='FULL 814',PAIRED_FULL='FULL 684',PAIRED_ENV='ENV 684')[gp$response]
stopifnot(nrow(gp)==57,all(gp$physically_within_city))
pa<-ggplot(gp,aes(city_area_km2,difference,colour=group,fill=group))+geom_hline(yintercept=0,linetype=2,linewidth=.3,colour='#999999')+
 geom_ribbon(aes(ymin=lower,ymax=upper),alpha=.10,colour=NA)+geom_line(linewidth=.65)+scale_x_log10()+scale_colour_manual(values=cols)+scale_fill_manual(values=cols)+
 labs(x=expression('City area (km'^2*')'),y='Suitability difference\n(higher - lower reclamation)',title='Continuous response',subtitle='Common absolute reclamation comparison; 83 cities')
bd<-load_table(cfg$current,'binary_native/paired_gam/reclamation_contrasts.csv','binary_paired_contrasts');bd$group<-paste(bd$group,'684')
bd2<-load_table(cfg$current,'binary_native/full814_gam/reclamation_contrasts.csv','binary_full814_contrasts');bd2$group<-'FULL 814';bd<-rbind(bd,bd2)
table_out(bd,'T04_binary_gam_contrasts')
z<-subset(bd,region=='CITY' & response=='mean_species_suitable_fraction')
z$estimate<-100*z$mean_q75_minus_q25;z$lower<-100*z$lower;z$upper<-100*z$upper;z$label<-factor(z$group,levels=rev(names(cols)))
pb<-forest(z,colour='group')+facet_wrap(~threshold,nrow=1)+scale_colour_manual(values=cols)+labs(x='Difference in threshold-based suitability (percentage points)',title='Does the pattern persist after applying thresholds?',subtitle='More versus less reclamation; 83 cities')+theme(legend.position='none')
save_plot((pa/pb)+plot_layout(heights=c(1.2,.8))+plot_annotation(tag_levels='a'),'F02_response_sensitivity',145,
 'a, G0 contrasts over the common absolute reclamation comparison range at 19 city sizes, using 83 complete-data cities. Lines and ribbons show fitted contrasts and their pointwise 95% intervals, not longitudinal change. b, City-level upper-minus-lower-quartile reclamation contrasts in suitable fraction under T10 and maximum-TSS thresholds; dots and bars show fitted contrasts and 95% model-based Wald intervals. Fraction denotes the spatial mean of the proportion of available non-native species above their thresholds, not a union of suitable land. These contrasts are distinct from whole-smooth significance tests. All area responses, mapped-reclamation results and model tests remain in the tables.')

}
# Human activity: retain city points and distinguish adjusted coefficients.
if(only_human) {
 source_index<-read.csv(file.path(out,'source_manifest.csv'),stringsAsFactors=FALSE)
 human_source<-function(label) {
  entry<-source_index[source_index$label==label,,drop=FALSE]
  stopifnot(nrow(entry)==1L,file.exists(entry$source),identical(hash_file(entry$source),entry$sha256))
  d<-read.csv(entry$source,stringsAsFactors=FALSE,check.names=FALSE)
  stopifnot(nrow(d)==entry$rows);d
 }
 h<-human_source('human_associations');hs<-human_source('human_city_samples')
} else {
h<-load_table(cfg$historical,'reclamation_human_20261015_v1/associations.csv','human_associations')
hs<-load_table(cfg$historical,'reclamation_human_20261015_v1/analysis_samples.csv','human_city_samples');table_out(h,'T05_human_activity_associations')
}
hn<-c(BUILT_S_E2025_2.5min='Built-up',GDP2020_2.5min='GDP',Pop2020_2.5min='Population',VIIRS_2023_2.5min='Night lights')
stopifnot(nrow(hs)==432L,all(table(hs$indicator)==108L),all(is.finite(hs$indicator_value)),
 all(hs$indicator_value>=0),all(hs$reclamation_amount_ha>=0))
raw<-subset(h,family=='SPEARMAN_AMOUNT');adj<-subset(h,family=='AREA_CONTROLLED_AMOUNT')
stopifnot(nrow(raw)==4L,nrow(adj)==4L,all(raw$n==108),all(adj$n==108),all(raw$status=='FITTED'),all(adj$status=='FITTED'))
fmtq<-function(x) ifelse(x<.001,'q < 0.001',sprintf('q = %.3f',x))
row_plots<-lapply(seq_along(hn),function(i){
 id<-names(hn)[i];z<-hs[hs$indicator==id,,drop=FALSE];r<-raw[raw$indicator==id,,drop=FALSE];a<-adj[adj$indicator==id,,drop=FALSE]
 stopifnot(nrow(z)==108L,nrow(r)==1L,nrow(a)==1L,!anyDuplicated(z$city),
  abs(cor(z$reclamation_amount_ha,z$indicator_value,method='spearman')-r$estimate)<1e-10)
 z$x<-log10(1+z$reclamation_amount_ha);z$y<-as.numeric(scale(log1p(z$indicator_value)))
 lab<-sprintf('rho = %.2f  |  %s',r$estimate,fmtq(r$p_bh))
 p1<-ggplot(z,aes(x,y))+geom_point(colour='#637E89',alpha=.48,size=.9)+
  annotate('label',x=Inf,y=Inf,label=lab,hjust=1,vjust=1,fill='white',linewidth=0,
   family=font,size=2.15,colour='#344248')+
  scale_x_continuous(limits=c(0,5.4),breaks=0:5,expand=expansion(mult=0))+
  scale_y_continuous(breaks=c(-2,0,2),expand=expansion(mult=c(.04,.18)))+
  labs(x=if(i==length(hn))'log10(1 + reclaimed area in ha)' else NULL,y=unname(hn[i]),
   title=if(i==1)'a   City observations (n = 108)' else NULL,
   subtitle=if(i==1)'Indicator values on a standardized log scale' else NULL)+
  theme(axis.title.y=element_text(size=7,face='bold'),axis.text.x=if(i==length(hn))element_text()else element_blank(),
   axis.ticks.x=if(i==length(hn))element_line()else element_blank(),plot.margin=margin(3,5,3,5))
 p2<-ggplot(a,aes(estimate,1))+geom_vline(xintercept=0,linetype=2,linewidth=.3,colour='#9BA5A9')+
  geom_segment(aes(x=lower,xend=upper,yend=1),colour='#A2554B',linewidth=.7)+geom_point(colour='#A2554B',size=1.8)+
  annotate('text',x=.52,y=1,label=fmtq(a$p_bh),hjust=0,family=font,size=2.15,colour='#344248')+
  scale_x_continuous(limits=c(-.32,.82),breaks=c(-.2,0,.2,.4),expand=expansion(mult=0))+
  scale_y_continuous(limits=c(.5,1.5),breaks=NULL)+
  labs(x=if(i==length(hn))'Standardized slope (95% CI)' else NULL,y=NULL,
   title=if(i==1)'b   Adjusted for city area' else NULL,
   subtitle=if(i==1)'Model estimates and 95% confidence intervals' else NULL)+
  theme(axis.line.y=element_blank(),axis.ticks.y=element_blank(),axis.text.y=element_blank(),
   axis.text.x=if(i==length(hn))element_text()else element_blank(),
   axis.ticks.x=if(i==length(hn))element_line()else element_blank(),plot.margin=margin(3,5,3,5))
 p1|p2
})
save_plot(wrap_plots(row_plots,ncol=1)+plot_layout(heights=rep(1,4)),'S01_reclamation_and_human_activity',165,
 'Supplementary Fig. 12 | Reclamation extent and indicators of urban activity. a, Each point represents one of 108 cities. The horizontal axis shows log10(1 + reclamation area in hectares); each vertical axis shows a standardized log(1 + indicator) value. Labels report Spearman correlations between the original variables and false discovery rate adjusted q values. b, Points and bars show standardized slopes and 95% confidence intervals from linear models of log(1 + indicator) against log(1 + reclamation area), adjusted for log(1 + city area). Intervals use HC3 robust standard errors; q values were adjusted across the four indicators separately for each analysis family. Indicator values are city summaries of gridded data. GDP and population data represent 2020, built-up data 2025 and night lights 2023. Samples include 108 cities. The plotted associations and intervals are the saved results; models were not refitted.')

if(!only_human) {

# Observed-richness sensitivity and continental context.
oc<-load_table(cfg$historical,'observed_richness_20261014_v1/association_v3/coefficients.csv','observed_richness_coefficients');table_out(oc,'T06_observed_richness_models')
z<-subset(oc,scope=='RAW_SELECTED_INVASIVE' & term=='reclamation')
windows<-c(ALL_DATED='All dated records',post2000='Later records',post2010='Recent records',common2010_2024='Common recent window')
z$pos<-match(z$window,names(windows));z$yy<-z$pos+ifelse(z$model=='M0',-.12,.12)
z$adjustment<-ifelse(z$model=='M0','Area only','Area + record intensity')
pa<-forest(z,y='yy',colour='adjustment')+scale_y_continuous(breaks=1:4,labels=unname(windows))+scale_colour_manual(values=c('Area only'='#447C9D','Area + record intensity'='#A57959'))+
 labs(x='Association with more reclamation\nFewer recorded species < 0 > More recorded species',title='Do record counts change the relationship?',subtitle='Original 107-city sample; lines: 95% intervals')
cg<-load_table(cfg$historical,'residual_models_20261015_v1/group_data_M1.csv','continental_adjusted_richness')
cc<-load_table(cfg$historical,'residual_models_20261015_v1/group_counts.csv','continental_counts');co<-load_table(cfg$historical,'residual_models_20261015_v1/group_omnibus.csv','continental_omnibus');ct<-load_table(cfg$historical,'residual_models_20261015_v1/group_contrasts.csv','continental_contrasts')
table_out(ct,'T07_continental_contrasts');table_out(co,'T07_continental_overall_tests')
lev<-c('Africa','Asia','Europe','North America','Oceania','South America');cg$continent<-factor(cg$verified_continent,levels=lev)
counts<-table(cg$continent);lab<-paste0(gsub(' ','\n',lev),'\nn=',as.integer(counts))
pb<-ggplot(cg,aes(continent,residual))+geom_hline(yintercept=0,linetype=2,colour='#999999',linewidth=.3)+geom_boxplot(width=.5,outlier.shape=NA,linewidth=.35,fill='#E7ECEF')+
 geom_point(position=position_jitter(width=.12,height=0,seed=41),alpha=.58,size=.8,colour='#516673')+scale_x_discrete(labels=lab)+
 labs(x=NULL,y='Recorded species above or below expectation',title='Which regions have more species than expected?',subtitle='Expectation accounts for area and record counts')
save_plot((pa/pb)+plot_layout(heights=c(.9,1.1))+plot_annotation(tag_levels='a'),'S02_observed_richness_and_geography',145,
 'a, Reclamation coefficients from the original zero-truncated negative-binomial recorded-richness models. Later records means from 2000 onward, recent records from 2010 onward, and the common recent window is 2010-2024. M0 adjusts for city area; M1 also includes target-record intensity. Windows overlap and are not independent replicates. b, Area- and record-intensity-adjusted richness residuals by continent: above zero means more recorded target species than expected for that area and record count. Boxes show median and interquartile range, whiskers extend to 1.5 times the interquartile range, and all cities are plotted. The country-intercept model overall p value is 0.0217; complete Holm-adjusted pairwise tests are in T07. The original 107-city sample includes Danzhou; Oceania has four cities. Target-record intensity is not independent survey effort.')

# Spatial configuration: all 18 planned comparisons.
cf<-load_table(cfg$historical,'reclamation_configuration_20261015_v1/associations.csv','configuration_associations');table_out(cf,'T08_configuration_models')
mn<-c(n_retained='Number of reclaimed patches',effective_patch_number='Area-weighted patch number',largest_patch_fraction='Fraction of reclaimed area\nin largest patch')
cf$metric_label<-factor(mn[cf$metric],levels=rev(unname(mn)));cf$adjustment<-ifelse(cf$model=='M0','Area-adjusted richness','Area + record-adjusted richness')
cf$cutoff<-factor(paste0(cf$cutoff_ha,' ha'),levels=c('1 ha','10 ha','100 ha'))
cf$yy<-as.numeric(cf$metric_label)+(match(cf$cutoff,c('1 ha','10 ha','100 ha'))-2)*.17
p<-ggplot(cf,aes(estimate,yy,colour=cutoff))+geom_vline(xintercept=0,linetype=2,linewidth=.3,colour='#999999')+
 geom_segment(aes(x=lower,xend=upper,yend=yy),linewidth=.65)+geom_point(aes(shape=cutoff),size=1.8)+facet_wrap(~adjustment,nrow=1)+
 scale_colour_manual(values=c('1 ha'='#447C9D','10 ha'='#9D7655','100 ha'='#969A9E'))+scale_shape_manual(values=c(16,17,15))+
 scale_y_continuous(breaks=1:3,labels=levels(cf$metric_label))+labs(x='Association with recorded richness\nFewer species < 0 > More species',y=NULL,title='Does the arrangement of reclaimed patches matter?',subtitle='Minimum patch areas: 1 ha (78 cities), 10 ha (70), 100 ha (55)')
save_plot(p+plot_annotation(tag_levels='a'),'S03_reclamation_configuration',90,
 'All 18 planned associations are displayed: three component metrics, three size cutoffs and two richness-residual definitions. Dots and bars show standardized coefficients and 95% HC3 intervals conditional on the estimated residuals. Area-weighted patch number is the inverse sum of squared patch-area shares; larger values indicate area distributed across more effective patches. Largest-patch fraction divides the area of the largest retained patch by the total area of all retained reclamation patches, not by city area. Models control city area and canonical reclamation amount. BH correction is applied within each nine-test residual family; exact p and q values are in T08. Record-adjusted associations at 1 and 10 ha pass that correction; 100-ha and area-only results are retained. Samples include Danzhou. Mapped reclamation components describe land configuration, not measured vegetation-habitat fragmentation.')

# First-record intervals and the separate temporal prediction test.
lc<-load_table(cfg$historical,'record_lag_20261015_v1/city_balanced_record_lag_source.csv','record_interval_city_values')
ls<-load_table(cfg$historical,'record_lag_20261015_v1/record_lag_summary.csv','record_interval_summary');table_out(ls,'T09_first_record_summary')
z<-subset(lc,unit=='cohort_first_any_target' & scope=='all_frozen_pairs' & cohort_endpoint=='ALL' & n_after_units>0)
stopifnot(nrow(z)==33)
display_city<-tools::toTitleCase(z$city_id)
aliases<-c(zhanzhou='Danzhou',hongkong='Hong Kong',newtaipei='New Taipei')
for(nm in names(aliases))display_city[z$city_id==nm]<-aliases[[nm]]
z$label<-factor(display_city,levels=display_city[order(z$median_lag_lower_years)])
pa<-ggplot(z,aes(median_lag_lower_years,label))+annotate('rect',xmin=13,xmax=18,ymin=-Inf,ymax=Inf,fill='#EFE7DA',alpha=.65)+
 geom_segment(aes(xend=median_lag_upper_years,yend=label),linewidth=.8,colour='#447C9D')+
 geom_point(size=.75,colour='#447C9D')+labs(x='Years from reclamation to first database record',y=NULL,title='First-record intervals',subtitle='33 cities, 96 cohort units; overall median bounds: 13-18 years')+
 theme(axis.text.y=element_text(size=6.2))
lag<-load_table(cfg$historical,'lag_exclude_danzhou_20261015/lead_lag_coefficients.csv','five_year_lag_coefficients');table_out(lag,'T10_temporal_models')
zz<-subset(lag,term=='log_lag' & case %in% c('H1_LAG_M0','H1_LAG_M1','H10_LAG_M0','H10_LAG_M1'))
zz$label<-factor(zz$case,levels=rev(c('H1_LAG_M0','H1_LAG_M1','H10_LAG_M0','H10_LAG_M1')),labels=rev(c('Threshold 1:\nno record-intensity term','Threshold 1:\nwith record-intensity term','Threshold 10:\nno record-intensity term','Threshold 10:\nwith record-intensity term')))
pb<-forest(zz)+labs(x='Association with more new species\nin the next five-year window',title='Does reclamation predict later records?',subtitle='Danzhou excluded; 78 / 68 cities')+theme(axis.text.y=element_text(size=6))
ss<-subset(ls,unit=='cohort_first_any_target' & scope=='all_frozen_pairs' & cohort_endpoint=='ALL');stopifnot(nrow(ss)==1)
support<-data.frame(status=c('After reclamation','Within date interval','Before date interval','No valid record year'),
 n=as.numeric(ss[1,c('count_AFTER_ENDPOINT','count_WITHIN_INTERVAL','count_BEFORE_INTERVAL','count_NO_VALID_YEAR')]))
stopifnot(sum(support$n)==ss$n_units)
support$status<-factor(support$status,levels=rev(support$status))
pc<-ggplot(support,aes(n,status))+geom_col(width=.6,fill='#8AABBf')+geom_text(aes(label=n),hjust=-.25,size=2.3)+
 scale_x_continuous(expand=expansion(mult=c(0,.18)))+labs(x='City-by-cohort units',y=NULL,title='Record coverage',subtitle=paste(ss$n_units,'mapped units; no record is not absence'))+
 theme(axis.text.y=element_text(size=6))
save_plot((pa|(pb/pc))+plot_layout(widths=c(1.05,1))+plot_annotation(tag_levels='a'),'S04_record_history_and_temporal_test',180,
 'a, For each city with post-reclamation first records, segments show the lower and upper city median first-record intervals arising from five-year reclamation-date bins. Shading marks the 13-18-year bounds of the city-balanced overall median, not a confidence interval. This conditional sample has 33 cities and 96 city-by-cohort units, including Danzhou; these intervals describe database recording, not biological arrival. b, Coefficients and 95% city-clustered intervals from the existing five-year temporal models. The main record-support threshold includes 78 cities and 261 city-period rows; the stronger-support threshold includes 68 cities and 235 rows. Both include city and period effects; record-intensity variants are shown. These models exclude Danzhou and do not recalibrate SDMs. c, Record coverage across all mapped city-by-cohort units in the first-record analysis. No valid year means no usable dated record, not an observed absence. All additional lead/lag variants and diagnostic overdispersion values are in T10.')

# Source-city coverage; missingness is not covariate balance.
sc<-load_table(cfg$historical,'source_pool_20261015_v2/continent_counts.csv','source_continent_counts')
sc<-subset(sc,link_definition=='retained_candidate');sc$group[sc$group=='Not linked']<-'Unlinked';sc$continent<-factor(sc$continent,levels=rev(lev))
pa<-ggplot(sc,aes(Freq,continent,fill=group))+geom_col(width=.62)+geom_text(aes(label=Freq),position=position_stack(vjust=.5),size=2.3,colour='white')+
 scale_fill_manual(values=c(Linked='#447C9D',Unlinked='#93989C'))+labs(x='Source-city records',y=NULL,title='Source-city representation',subtitle='106 linked; 29 unlinked; continental p = 0.00009')
cv<-load_table(cfg$selection,'covariate_coverage.csv','source_covariate_coverage');bal<-load_table(cfg$selection,'available_balance.csv','source_available_balance');sp<-load_table(cfg$selection,'source_pool_extended.csv','extended_source_cities')
table_out(cv,'T11_source_covariate_coverage');table_out(bal,'T11_available_balance');table_out(sp,'T11_source_city_crosswalk')
vn<-c(continent='Continent',latitude='Latitude',city_area='City area',reclamation_numeric='Numeric reclamation',reclamation_fraction='Reclamation fraction',GDP='GDP',population='Population',built_up='Built-up surface',night_light='Night-time light',all_plant_record_count='All-plant record count')
cv$variable_label<-factor(vn[cv$variable],levels=rev(unname(vn)));cv$group<-factor(cv$group,levels=c('Linked','Unlinked'))
pb<-ggplot(cv,aes(group,variable_label,fill=fraction_available))+geom_tile(colour='white',linewidth=1)+
 geom_text(aes(label=paste0(n_available,'/',n_group)),size=2.4)+scale_fill_gradient(low='#F0F1F2',high='#BCD0DD',limits=c(0,1),guide='none')+
 labs(x=NULL,y=NULL,title='Availability for comparison',subtitle='Counts available / source-group size')+theme(axis.line=element_blank(),axis.ticks=element_blank())
save_plot((pa|pb)+plot_layout(widths=c(1,1))+plot_annotation(tag_levels='a'),'S05_source_city_selection',112,
 'a, Continental composition of 135 source-city records under exact and coordinate/area-supported candidate links: 106 linked and 29 unlinked. The current 107-city analytical list after exclusion of Danzhou also includes Sanya, which is outside the source table. The continental test is the retained Monte Carlo Fisher test. b, Covariate availability after linking the existing city predictors and recovering coordinates from historical geocoded tables. Missing cells remain missing. This panel displays coverage, not standardized covariate balance. Historical taxon counts originate from regional GloNAF joins and are not all-plant GBIF sampling effort. Source records without quantitative reclamation cannot be assigned zero.')

# Complete numerical tables for pathways; avoid repeated path diagrams.
for(item in list(c('observed_pc_components_20261014_v1/effects.csv','T12_observed_component_sem'),c('residual_models_20261015_v1/effects.csv','T13_residual_sem'))){
 d<-load_table(cfg$historical,item[1],item[2]);table_out(d,item[2])
}
d<-load_table(cfg$current,'city_component_sem/effects.csv','continuous_component_sem');table_out(d,'T14_continuous_component_sem')
for(nm in c('paired_sem','full814_sem')){
 d<-load_table(cfg$current,paste0('binary_native/',nm,'/effects.csv'),paste0('binary_',nm));table_out(d,paste0('T15_binary_',nm))
}
for(nm in c('paired_gam','full814_gam')){
 d<-load_table(cfg$current,paste0('binary_native/',nm,'/model_summary.csv'),paste0('binary_',nm,'_models'));table_out(d,paste0('T04_binary_',nm,'_models'))
}
write.csv(do.call(rbind,manifest),file.path(out,'source_manifest.csv'),row.names=FALSE)
write.csv(do.call(rbind,exports),file.path(out,'figure_manifest.csv'),row.names=FALSE)
writeLines(c('# Figure legends',captions),file.path(out,'FIGURE_LEGENDS.md'))
writeLines(capture.output(sessionInfo()),file.path(out,'sessionInfo.txt'))
write_json(list(status='RENDERED_PENDING_VISUAL_REVIEW',figures=length(exports),backend='R',font=font,
 formats=c('PDF','SVG','PNG preview'),new_model_fits=FALSE,script_sha256=hash_file(cfg$script_path)),
 file.path(out,'render_receipt.json'),auto_unbox=TRUE,pretty=TRUE)
} else {
 writeLines(captions,file.path(out,'S01_reclamation_and_human_activity_legend.txt'))
}
