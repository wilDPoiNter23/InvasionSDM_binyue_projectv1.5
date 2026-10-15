# Render coastal reclamation evidence from verified source tables.
# Usage: Rscript render_reclamation_evidence.R CONFIG.json
suppressPackageStartupMessages({
 library(ggplot2); library(patchwork); library(jsonlite)
 library(svglite); library(ragg); library(digest)
})
args <- commandArgs(TRUE)
stopifnot(length(args)==1L)
cfg <- read_json(args[1], simplifyVector=TRUE)
out <- cfg$output_root
dir.create(out, recursive=TRUE, showWarnings=FALSE)
srcdir <- file.path(out, 'source_data')
dir.create(srcdir, showWarnings=FALSE)
provenance <- list()
read_source <- function(root, name, label) {
 f <- file.path(root,name)
 stopifnot(file.exists(f))
 d <- read.csv(f, check.names=FALSE, stringsAsFactors=FALSE)
 write.csv(d,file.path(srcdir,paste0(label,'.csv')),row.names=FALSE,na='')
 provenance[[label]] <<- list(file=name,sha256=digest(file=f,algo='sha256'),rows=nrow(d))
 d
}
families <- systemfonts::system_fonts()$family
font <- if(!capabilities('cairo'))'Helvetica' else if('Arial'%in%families)'Arial' else 'Liberation Sans'
stopifnot(font=='Helvetica'||font %in% families)
if(!capabilities('cairo'))stopifnot(nzchar(Sys.which('gs')))
theme_set(theme_classic(base_size=6.5,base_family=font)+theme(
 axis.line=element_line(linewidth=.3),axis.ticks=element_line(linewidth=.3),
 axis.text=element_text(size=6,colour='black'),legend.text=element_text(size=6),
 plot.title=element_text(size=7,face='bold'),strip.text=element_text(size=6.5),
 plot.tag=element_text(size=8,face='bold'),plot.margin=margin(5,6,5,5),
 legend.position='bottom',legend.title=element_blank(),panel.grid=element_blank()))
cols <- c(FULL='#356C91',ENV='#29877C')
exports <- list(); captions <- character()
save_figure <- function(p,name,height=96) {
 w <- 183/25.4; h <- height/25.4
 svglite(file.path(out,paste0(name,'.svg')),width=w,height=h);print(p);dev.off()
 pdf_file<-file.path(out,paste0(name,'.pdf'))
 if(capabilities('cairo')) {
  cairo_pdf(pdf_file,width=w,height=h,family=font);print(p);dev.off()
 } else {
  pdf(pdf_file,width=w,height=h,family='Helvetica',useDingbats=FALSE);print(p);dev.off()
  embedded<-file.path(out,paste0(name,'_embedded.pdf'))
  embedFonts(pdf_file,outfile=embedded)
  stopifnot(file.exists(embedded),file.info(embedded)$size>1000,file.rename(embedded,pdf_file))
 }
 agg_tiff(file.path(out,paste0(name,'.tiff')),width=w,height=h,units='in',res=600,compression='lzw');print(p);dev.off()
 agg_png(file.path(out,paste0(name,'.png')),width=w,height=h,units='in',res=200);print(p);dev.off()
 exports[[name]] <<- list(width_mm=183,height_mm=height,raster_dpi=600,font=font)
}
tagged <- function(p) p+plot_annotation(tag_levels='a')
model_name <- function(x) ifelse(grepl('FULL',x),'FULL','ENV')

# Period comparisons: retain both model families and both support scopes.
period <- read_source(cfg$period_root,'old_recent_city_source.csv','period_city_pairs')
pc <- read_source(cfg$period_root,'old_recent_contrast.csv','period_contrasts')
stopifnot(max(abs(period$recent_mean-period$old_mean-period$recent_minus_old))<1e-10)
period$model <- model_name(period$response);pc$model <- model_name(pc$response)
stopifnot(nrow(pc)==4L,all(pc$n_cities[pc$scope=='native_support']==51L),
 all(pc$n_cities[pc$scope=='all_mapped']==76L))
for(i in seq_len(nrow(pc))) {
 z<-period[period$response==pc$response[i]&period$scope==pc$scope[i],]
 stopifnot(nrow(z)==pc$n_cities[i],abs(mean(z$recent_minus_old)-pc$recent_minus_old[i])<1e-10)
}
q <- subset(period,scope=='native_support')
stopifnot(all(table(q$model)==51),!anyDuplicated(q[,c('city_id','model')]))
period_panel <- function(m) {
 d<-q[q$model==m,]
 long<-rbind(data.frame(city=d$city_id,x=1,value=d$old_mean),data.frame(city=d$city_id,x=2,value=d$recent_mean))
 mu<-aggregate(value~x,long,mean)
 ggplot(long,aes(x,value,group=city))+geom_line(colour='grey75',linewidth=.25,alpha=.7)+
 geom_point(colour=cols[m],size=.7,alpha=.65)+geom_line(data=mu,aes(group=1),linewidth=.85,colour='black')+
 geom_point(data=mu,aes(group=1),size=1.5,colour='black')+scale_x_continuous(breaks=1:2,labels=c('Earlier\n1985-2000','Recent\n2010-2020'))+
 labs(x=NULL,y='Current predicted suitability',title=paste0(m,': paired cities (n = 51)'))
}
pc$label<-paste(pc$model,ifelse(pc$scope=='native_support','native support (51)','all mapped (76)'))
pc$label<-factor(pc$label,levels=rev(c('FULL native support (51)','ENV native support (51)','FULL all mapped (76)','ENV all mapped (76)')))
pdelta<-ggplot(pc,aes(recent_minus_old,label,colour=model))+geom_vline(xintercept=0,linetype=2,colour='grey60',linewidth=.3)+
 geom_segment(aes(x=ci_low,xend=ci_high,yend=label),linewidth=.65)+geom_point(size=1.7)+scale_colour_manual(values=cols)+
 labs(x='Recent minus earlier suitability',y=NULL,title='City-mean differences and 95% intervals')+theme(legend.position='none')
save_figure(tagged(period_panel('FULL')+period_panel('ENV')+pdelta+plot_layout(widths=c(1,1,1.35))),'reclamation_period',90)
captions<-c(captions,'Reclamation period. Lines connect current suitability means for earlier (five-year cohort endpoints 1990-2000) and recent (2015-2020) reclaimed land within each city; black lines show means across cities. Both model families use the source native-support comparison (51 cities). The third panel includes the all-mapped sensitivity (76 cities); bars are existing 95% percentile intervals from 9,999 city bootstrap draws. Within-city land-period means are mapped-area weighted; cities receive equal weight in the contrast. FULL includes selected human-associated predictors, including reclamation where selected; ENV excludes human predictors. These are current predictions for land formed in different periods, not past-to-present suitability trajectories. Source tables: period_city_pairs.csv and period_contrasts.csv.')

# First-record intervals and all mapped-cohort denominators.
lag <- read_source(cfg$lag_root,'cohort_first_any_record_lag_bounds.csv','lag_cohorts')
ls <- read_source(cfg$lag_root,'record_lag_summary.csv','lag_summary')
all_lag<-subset(lag,scope=='all_frozen_pairs'&first_record_definition=='FIRST_ANY_TARGET_RECORD')
l<-subset(all_lag,temporal_state=='AFTER_ENDPOINT')
stopifnot(nrow(all_lag)==512,nrow(l)==96,length(unique(l$city_id))==33,
 all(l$lag_upper_years-l$lag_lower_years==5))
l<-l[order(l$lag_lower_years,l$city_id),];l$rank<-seq_len(nrow(l))
ncity<-length(unique(l$city_id));nc<-table(l$city_id)
l$weight<-1/(ncity*as.numeric(nc[l$city_id]))
weighted_curve<-function(field,bound) {
 z<-aggregate(l$weight,list(year=l[[field]]),sum);names(z)[2]<-'mass'
 z<-z[order(z$year),];z$cumulative<-cumsum(z$mass);z$bound<-bound;z
}
ec<-rbind(weighted_curve('lag_lower_years','Lower date bound'),weighted_curve('lag_upper_years','Upper date bound'))
med<-vapply(split(ec,ec$bound),function(d)d$year[which(d$cumulative>=.5-1e-12)[1]],numeric(1))
stopifnot(med['Lower date bound']==13,med['Upper date bound']==18,abs(sum(l$weight)-1)<1e-10)
ec_plot<-do.call(rbind,lapply(split(ec,ec$bound),function(d) {
 rbind(data.frame(year=0,mass=0,cumulative=0,bound=d$bound[1]),d,
 data.frame(year=max(ec$year)+1,mass=0,cumulative=1,bound=d$bound[1]))
}))
write.csv(l,file.path(srcdir,'lag_after_intervals_with_city_weights.csv'),row.names=FALSE)
write.csv(ec,file.path(srcdir,'lag_weighted_cdf.csv'),row.names=FALSE)
pinterval<-ggplot(l,aes(rank,lag_lower_years))+geom_segment(aes(yend=lag_upper_years,xend=rank),colour='#356C91',linewidth=.45)+
 labs(x='Reclamation-period unit (ordered)',y='Years from reclamation to first record',title='Date bounds: 96 units in 33 cities')
pcdf<-ggplot(ec_plot,aes(year,cumulative,linetype=bound))+geom_hline(yintercept=.5,linewidth=.3,colour='grey65')+
 geom_step(colour='#356C91',linewidth=.65)+annotate('segment',x=13,xend=18,y=.5,yend=.5,linewidth=1.7,colour='#29877C')+
 annotate('text',x=23,y=.13,label='Median bounds: 13-18 years',size=2.05,family=font)+
 scale_y_continuous(labels=function(x)paste0(round(100*x),'%'))+labs(x='First-record delay (years)',y='City-balanced cumulative share',title='Equal weight per city')+
 theme(legend.position='bottom')
state<-as.data.frame(table(all_lag$temporal_state));names(state)<-c('state','n')
state$label<-c(AFTER_ENDPOINT='After interval',WITHIN_INTERVAL='Within interval',BEFORE_INTERVAL='Before interval',NO_VALID_YEAR='No dated record')[as.character(state$state)]
stopifnot(!anyNA(state$label),sum(state$n)==512)
pstate<-ggplot(state,aes(n,reorder(label,n)))+geom_col(fill='grey70',width=.6)+
 geom_text(aes(label=n),hjust=-.3,size=2.1,family=font)+scale_x_continuous(expand=expansion(mult=c(0,.18)))+
 labs(x='Mapped city-period units',y=NULL,title='All 512 mapped units')
save_figure(tagged(pinterval+pcdf+pstate+plot_layout(widths=c(1.15,1.2,1))),'first_record_delay',92)
captions<-c(captions,'First-record delay. Each segment represents the five-year date bounds for one city-by-reclamation-period unit with a first dated target-plant record after the reported reclamation interval (96 units, 33 cities). The cumulative curves weight periods equally within a city and cities equally; median lower and upper bounds are 13 and 18 years. This range is date-resolution uncertainty, not a confidence interval. The denominator panel shows all 512 mapped units: 96 after-interval, 7 within-interval, 11 before-interval and 398 without a valid dated target record. Units without records are not treated as uninvaded. Delays combine biological arrival/establishment and observation processes and are not an invasion latent-period estimate. Source tables: lag_cohorts.csv, lag_after_intervals_with_city_weights.csv and lag_weighted_cdf.csv.')

# Residual component models: area-only result and material record-intensity sensitivity.
effects<-read_source(cfg$residual_root,'effects.csv','residual_effects')
edges<-read_source(cfg$residual_root,'edges.csv','residual_edges')
for(m in c('M0','M1')) {
 case<-paste0('AREA_EFFORT_RESIDUAL_',m)
 d<-read_source(cfg$residual_root,paste0('data_',case,'.csv'),paste0('residual_data_',m))
 controls<-c('soc','clim','soil','city_area',if(m=='M1')'intensity')
 dx<-residuals(lm(reformulate(controls,'recl'),data=d))
 dy<-residuals(lm(reformulate(controls,'y'),data=d))
 b<-sum(dx*dy)/sum(dx^2)
 expected<-subset(edges,case==paste0('AREA_EFFORT_RESIDUAL_',m)&response=='y'&predictor=='recl')
 stopifnot(nrow(d)==84,nrow(expected)==1,abs(b-expected$estimate)<1e-8)
 pd<-data.frame(city=d$city,partial_reclamation=dx,partial_richness=dy)
 write.csv(pd,file.path(srcdir,paste0('partial_association_',m,'.csv')),row.names=FALSE)
 pscatter<-ggplot(pd,aes(partial_reclamation,partial_richness))+geom_hline(yintercept=0,linewidth=.25,colour='grey80')+
 geom_point(size=1.2,alpha=.65,colour='#356C91')+geom_abline(intercept=0,slope=b,linewidth=.7,colour='black')+
 labs(x='Partial log(1 + reclamation area in ha)',y='Partial recorded richness (species)',
 title=if(m=='M0')'Area-adjusted association (84 cities)'else 'Record-intensity sensitivity (84 cities)')
 ef<-subset(effects,case==paste0('AREA_EFFORT_RESIDUAL_',m)&effect%in%c('direct','indirect','total'))
 ef$effect<-factor(ef$effect,levels=c('total','indirect','direct'),labels=c('Total','Combined indirect','Direct'))
 peffects<-ggplot(ef,aes(estimate,effect))+geom_vline(xintercept=0,linetype=2,colour='grey60',linewidth=.3)+
 geom_segment(aes(x=lower,xend=upper,yend=effect),linewidth=.7,colour='#356C91')+geom_point(size=1.8)+
 labs(x='Coefficient (recorded species per log-area unit)',y=NULL,title='Component-model effects and 95% intervals')
 save_figure(tagged(pscatter+peffects+plot_layout(widths=c(1.15,1))),paste0('observed_richness_',m),86)
}
captions<-c(captions,'Observed richness. M0 first adjusts raw recorded invasive-plant richness for log(1 + city area); M1 additionally adjusts for log(1 + target-record count). The scatter displays the partial association in the richness component equation after accounting for socioeconomic, climate and soil scores and city area (plus record intensity in M1). Points are 84 cities; the line is the fitted partial coefficient. Effect bars are 95% percentile intervals from 1,999 city bootstrap draws, with first-stage residual adjustment refitted in each draw. All direct, combined indirect and total effects are shown; no paths were removed by significance. Coefficients are unstandardized associations, not identified causal effects. M1 is a required companion sensitivity because target-record intensity changes the inference; target-record count is itself an endogenous proxy for observation intensity. Source tables: residual_effects.csv, residual_edges.csv and residual_data_M0/M1.csv.')

writeLines(captions,file.path(out,'figure_captions.txt'))
writeLines(capture.output(sessionInfo()),file.path(out,'sessionInfo.txt'))
write_json(list(status='RENDERED_FROM_EXISTING_TABLES',backend='R',font=font,exports=exports,
 source_tables=provenance,configuration_sha256=digest(file=args[1],algo='sha256'),
 script_sha256=digest(file=sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1]),algo='sha256'),
 pdf_device=if(capabilities('cairo'))'cairo_pdf'else'R pdf with embedFonts',
 visual_review_required=TRUE),file.path(out,'figure_receipt.json'),auto_unbox=TRUE,pretty=TRUE)
cat('Rendered',length(exports),'figure bundles\n')
