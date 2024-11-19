##################################################################
##########      Load library and data
##################################################################
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
library(raster)
library(sp)
library(maps)
library(maptools)

library(spNNGP)
library(geosphere)

#####################################################################
##########                      Read in covariates
#####################################################################

setwd("~/traffic_data/")
good_tp<-as.data.frame(readRDS('better_tp.rds'))
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



### A1 

setwd("~/spNNGP_results/full_model/")
traffic_nngp_a1<-readRDS('a1_full.rds')



pred_a1<-predict(traffic_nngp_a1, X.0=as.matrix(cbind(1,la_pre_dat[,
                         c('population_select','ndvi_select', 'land_use_select')])),
                   coords.0=as.matrix(la_pre_dat[,c('longitude','latitude')]),
                   n.omp.threads = 10, n.report=1000)

la_a1_grid<-la_pre_dat
la_a1_grid$fitted<-pred_a1$y.0.hat



a1_gradient<-data.frame()

for(i_it in 1:20){
  
a1_samp<-la_a1_grid[sample(nrow(la_a1_grid),10000),]
temp_dist<-distm(a1_samp[,c('longitude','latitude')],
                 a1_samp[1,c('longitude','latitude')])
p_diff<-(a1_samp$fitted-as.numeric(a1_samp[1,]$fitted))/
  as.numeric(a1_samp[1,]$fitted)*100
temp_g<-data.frame(dist=temp_dist,per_diff=p_diff)
temp_g<-temp_g[temp_g$dist<1000,]
a1_gradient<-rbind(a1_gradient,temp_g)

}

plot(a1_gradient$dist,abs(a1_gradient$per_diff),
     pch=16, cex=0.6,
     col=rgb(red=0.2, green=0.2, blue=1.0, alpha=0.2))

abline(lm(abs(per_diff)~dist,data=a1_gradient))

#la_obs<-good_tp[good_tp$state=='CA'&good_tp$CFCC_agre=='A1',]

# raster
a1_ras_frame<-la_a1_grid[,c('longitude','latitude','fitted')]
a1_ras<- a1_ras_frame
coordinates(a1_ras) <- ~ longitude+latitude

gridded(a1_ras) <- TRUE
rasterDF <- raster(a1_ras)
#TRAFCA0000082575

ggplot(la_a1_grid, aes(x=longitude, y=latitude, color=fitted)) + geom_point()