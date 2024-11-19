##################################################################
##########      Load library and data
##################################################################
rm(list=ls())
library(foreign)
library(data.table)
library(ggplot2)
library(stringr)
library(tidyselect)
library(gridExtra)
library(rlist)
library(geoR)
library(caret)
library(usmap)
library(ModelMetrics)
library(mgcv)

library(sp)
library(maps)
library(maptools)

library(spNNGP)
library(raster)
library(RgoogleMaps)
library(ggmap)
#####################################################################
##########                      Read in covariates
#####################################################################

setwd("~/traffic_data/")
good_tp<-as.data.frame(readRDS('better_tp.rds'))

good_tp_A1<-good_tp[good_tp$CFCC_agre=='A1',]
good_tp_A2<-good_tp[good_tp$CFCC_agre=='A2',]
good_tp_A3<-good_tp[good_tp$CFCC_agre=='A3',]
good_tp_A4<-good_tp[good_tp$CFCC_agre=='A4',]


A1_regre<-good_tp_A1[,c('median_traffic','LINE_LONGI','LINE_LATIT',
                        'land_use_select','population_select',
                        'ndvi_select')]


A2_regre<-good_tp_A2[,c('median_traffic','LINE_LONGI','LINE_LATIT',
                        'land_use_select','population_select',
                        'ndvi_select')]


A3_regre<-good_tp_A3[,c('median_traffic','LINE_LONGI','LINE_LATIT',
                        'land_use_select','population_select',
                        'ndvi_select')]


A4_regre<-good_tp_A4[,c('median_traffic','LINE_LONGI','LINE_LATIT',
                        'land_use_select','population_select',
                        'ndvi_select')]




#####################################################################
##########              load fitted objects
#####################################################################

setwd("~/spNNGP_results/full_model/")


traffic_nngp_a1<-readRDS('a1_full_nocov.rds')
traffic_nngp_a2<-readRDS('a2_full_nocov.rds')
traffic_nngp_a3<-readRDS('a3_full_nocov.rds')
traffic_nngp_a4<-readRDS('a4_full_nocov.rds')




## la_grid

setwd("~/traffic_data/")

load('dr0354_grid_25m_covar.rda')

# log low intensity (3000 m)
dr0354_grid_25m_covar$land_use_select<-
  log(dr0354_grid_25m_covar$rlu_dev_lo_p03000+1)
# log population (1000 m)
dr0354_grid_25m_covar$population_select<-
  log(dr0354_grid_25m_covar$pop_s01000+1)
# median annual ndvi  (500 m)
dr0354_grid_25m_covar$ndvi_select<-
  log(dr0354_grid_25m_covar$ndvi_q50_a00500+1)

la_pre_dat<-dr0354_grid_25m_covar[,c('longitude','latitude',
                                     'population_select','ndvi_select', 'land_use_select')]


##################################################################
##########      Load functions
##################################################################


ncolors=200

prep_ras<-function(grid_dat,ncolors=200){
  
  grid_dat$x<-grid_dat$longitude
  grid_dat$y<-grid_dat$latitude
  
  tmp_ext<-extent(grid_dat[,c('x','y')])
  
  ras_empty<-raster(tmp_ext,ncol=600,nrow=600)
  
  ras_tmp<-rasterize(grid_dat[,c('longitude','latitude')],
                     ras_empty,grid_dat[,'fitted'],
                     fun=mean)
  
  return(ras_tmp)
  #plot(ras_a2,col=terrain.colors(ncolors))
  
  
}


prep_pt<-function(grid_ras,pt_dat,ncolors=200){
  
  coordinates(pt_dat)<-~LINE_LONGI+LINE_LATIT
  
  pt_dat<-crop(pt_dat,extent(grid_ras))
  #plot(sp_a2_la,cex=0.3,pch=1,add=TRUE)
  
  min_ras<-minValue(grid_ras)
  max_ras<-maxValue(grid_ras)
  
  obs_val_int<-(pt_dat$median_traffic-min_ras)/
    (max_ras-min_ras)*ncolors
  
  obs_val_int[obs_val_int>ncolors]<-ncolors
  obs_val_int[obs_val_int<1]<-1
  
  pt_dat$color<-terrain.colors(ncolors)[obs_val_int]
  
  return(pt_dat)
  
}



##################################################################
##########      A1
##################################################################

la_a1_grid<-la_pre_dat


pred_a1<-predict(traffic_nngp_a1, X.0=as.matrix(rep(1,times=dim(la_pre_dat)[1])),
                 coords.0=as.matrix(la_pre_dat[,c('longitude','latitude')]),
                 n.omp.threads = 10, n.report=1000,
                 verbose=FALSE)

la_a1_grid$fitted<-pred_a1$y.0.hat

##################################################################
##########      A2
##################################################################



la_a2_grid<-la_pre_dat

pred_a2<-predict(traffic_nngp_a2, X.0=as.matrix(rep(1,times=dim(la_pre_dat)[1])),
                 coords.0=as.matrix(la_pre_dat[,c('longitude','latitude')]),
                 n.omp.threads = 10, n.report=1000,
                 verbose=FALSE)

la_a2_grid$fitted<-pred_a2$y.0.hat
ncolors<-200
a2_ras<-prep_ras(la_a2_grid)
plot(a2_ras,col=terrain.colors(ncolors))
a2_pts<-prep_pt(a2_ras,A2_regre)
proj4string(a2_pts)<-sp::CRS("+init=epsg:4326")
a2_pdat<-as.data.frame(a2_pts)
#a2_color<-as.numeric(a2_pdat$median_traffic)
a2_color<-a2_pdat$color
par(pty="s")
map_la_a2<-plotmap(lat=LINE_LATIT,
                   lon=LINE_LONGI,
                   zoom=12,
                   pch=20,
                   col=a2_pdat$color,
                   data=a2_pdat,
                   legend("topright", 
                          legend = a2_pdat$median_traffic, 
                          fill = a2_color, 
                          bg = "white"))

setwd('~/spNNGP_results/LA_maps/')

pdf('A2_test.pdf',width=8,height=8)

{
  par(pty='s')
bubbleMap(SP=a2_pts,
          coords = c('LINE_LONGI','LINE_LATIT'),
          map=la_map,zcol='median_traffic',
          legendLoc = 'bottomleft',
          max.radius = 100,
          alpha = 0.5,
          key.entries = 10000+20000*c(0:5))

}
dev.off()

a2_xy<-a2_pdat[,c("LINE_LATIT","LINE_LONGI")]
a2_center<-as.vector(colMeans(a2_xy))
la_map<-GetMap(center=a2_center,zoom=12)
PlotOnStaticMap(la_map)
ggmap(la_map)




plot(a3_pts,cex=0.5,pch=1,col=a3_pts$color,add=TRUE)

la_a2_grid$x<-la_a2_grid$longitude
la_a2_grid$y<-la_a2_grid$latitude

tmp_ext<-extent(la_a2_grid[,c('x','y')])

tmp_ras<-raster(tmp_ext,ncol=600,nrow=600)

ras_a2<-rasterize(la_a2_grid[,c('longitude','latitude')],
                  tmp_ras,la_a2_grid[,'fitted'],
                  fun=mean)

ncolors<-200
plot(ras_a2,col=terrain.colors(ncolors))
plot(sp_a2_la,cex=0.5,pch=1,col=sp_a2_la$color,add=TRUE)

# a2 points


sp_a2<-A2_regre
coordinates(sp_a2)<-~LINE_LONGI+LINE_LATIT

sp_a2_la<-crop(sp_a2,extent(ras_a2))
#plot(sp_a2_la,cex=0.3,pch=1,add=TRUE)

min_ras<-minValue(ras_a2)
max_ras<-maxValue(ras_a2)

obs_val_int<-(sp_a2_la$median_traffic-min_ras)/
    (max_ras-min_ras)*ncolors

obs_val_int[obs_val_int>ncolors]<-ncolors
obs_val_int[obs_val_int<0]<-0

sp_a2_la$color<-terrain.colors(ncolors)[obs_val_int]
plot(sp_a2_la,cex=0.5,pch=1,col=sp_a2_la$color,add=TRUE)

#col=terrain.colors(100)
## A3

la_a3_grid<-la_pre_dat

pred_a3_log<-predict(traffic_nngp_a3_log, X.0=as.matrix(cbind(1,la_pre_dat[,
                                                                           c('land_use_select','population_select','ndvi_select')])),
                     coords.0=as.matrix(la_pre_dat[,c('longitude','latitude')]),
                     n.omp.threads = 10, n.report=1000,
                     verbose=FALSE)


pred_a3<-predict(traffic_nngp_a3, X.0=as.matrix(rep(1,times=dim(la_pre_dat)[1])),
                 coords.0=as.matrix(la_pre_dat[,c('longitude','latitude')]),
                 n.omp.threads = 10, n.report=1000,
                 verbose=FALSE)

la_a3_grid$fitted<-pred_a3$y.0.hat

a3_ras<-prep_ras(la_a3_grid)
plot(a3_ras,col=terrain.colors(ncolors))
a3_pts<-prep_pt(a3_ras,A3_regre)


plot(a3_pts,cex=0.5,pch=1,col=a3_pts$color,add=TRUE)




