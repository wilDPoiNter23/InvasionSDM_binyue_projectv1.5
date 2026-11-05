#!/usr/bin/env Rscript
# Diagnostic comparison figures from complete binned density and map source tables.
suppressPackageStartupMessages(library(ggplot2))
a=commandArgs(TRUE)
if(length(a)!=1L) stop('Usage: plot_review_paired.R OUTPUT_ROOT')
out=a[1]; density=read.csv(file.path(out,'tables','paired_density_source.csv')); map=read.csv(file.path(out,'tables','paired_map_source_25arcmin.csv'))
k=sum(density$count); bins=length(unique(density$full_bin))
theme_set(theme_classic(base_size=9,base_family='Arial'))
save_plot=function(p,name,width=7.2,height=4.2){ggsave(file.path(out,'figures',paste0(name,'.pdf')),p,width=width,height=height,device=cairo_pdf);ggsave(file.path(out,'figures',paste0(name,'.png')),p,width=width,height=height,dpi=300)}
p=ggplot(density[density$count>0,],aes(FULL,ENV,fill=log10(count)))+geom_tile(width=1/bins,height=1/bins)+geom_abline(slope=1,intercept=0,colour='grey40',linewidth=.35)+scale_fill_viridis_c(name='log10 cells')+coord_equal(xlim=c(0,1),ylim=c(0,1))+labs(x='Paired FULL suitability',y='Paired ENV suitability',title='All finite paired cells',subtitle=paste(format(k,big.mark=','),'cells; bins = 0.004'))
save_plot(p,'paired_cell_density',5.2,4.4)
long=rbind(data.frame(x=map$x,y=map$y,response='PAIRED_FULL_684',value=map$PAIRED_FULL_684),data.frame(x=map$x,y=map$y,response='PAIRED_ENV_684',value=map$PAIRED_ENV_684))
p=ggplot(long,aes(x,y,fill=value))+geom_tile(width=10/24,height=10/24)+facet_wrap(~response,ncol=1)+scale_fill_viridis_c(limits=c(0,1),name='Suitability',na.value='white')+coord_equal()+labs(x='Longitude',y='Latitude',subtitle='Display means over 25-arcmin cells; analyses use native 2.5-arcmin cells')
save_plot(p,'paired_common_scale_maps',7.2,6)
p=ggplot(map,aes(x,y,fill=ENV_MINUS_FULL))+geom_tile(width=10/24,height=10/24)+scale_fill_gradient2(low='#2166AC',mid='white',high='#B2182B',midpoint=0,name='ENV - FULL',na.value='white')+coord_equal()+labs(x='Longitude',y='Latitude',subtitle='Display means over 25-arcmin cells')
save_plot(p,'paired_difference_map',7.2,3.6)
quit(save='no',status=0)
