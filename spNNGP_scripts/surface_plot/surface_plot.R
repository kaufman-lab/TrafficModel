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

library(sp)
library(maps)
library(maptools)

library(spNNGP)


#####################################################################
##########                      Read in covariates
#####################################################################

setwd("~/traffic_data/")
good_tp<-as.data.frame(readRDS('better_tp.rds'))

good_tp_A1<-good_tp[good_tp$CFCC_agre=='A1',]
good_tp_A2<-good_tp[good_tp$CFCC_agre=='A2',]
good_tp_A3<-good_tp[good_tp$CFCC_agre=='A3',]
good_tp_A4<-good_tp[good_tp$CFCC_agre=='A4',]


A1_regre<-good_tp_A1[,c('log_traffic','LINE_LONGI','LINE_LATIT',
                        'land_use_select','population_select',
                        'ndvi_select')]


A2_regre<-good_tp_A2[,c('log_traffic','LINE_LONGI','LINE_LATIT',
                        'land_use_select','population_select',
                        'ndvi_select')]


A3_regre<-good_tp_A3[,c('log_traffic','LINE_LONGI','LINE_LATIT',
                        'land_use_select','population_select',
                        'ndvi_select')]


A4_regre<-good_tp_A4[,c('log_traffic','LINE_LONGI','LINE_LATIT',
                        'land_use_select','population_select',
                        'ndvi_select')]




#####################################################################
##########              load fitted objects
#####################################################################

setwd("~/spNNGP_results/full_model/")


traffic_nngp_a1<-readRDS('a1_full.rds')
traffic_nngp_a2<-readRDS('a2_full.rds')
traffic_nngp_a3<-readRDS('a3_full.rds')
traffic_nngp_a4<-readRDS('a4_full.rds')


traffic_nngp_a1_log<-readRDS('a1_full_log.rds')
traffic_nngp_a2_log<-readRDS('a2_full_log.rds')
traffic_nngp_a3_log<-readRDS('a3_full_log.rds')
traffic_nngp_a4_log<-readRDS('a4_full_log.rds')

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



setwd("~/spNNGP_results/LA_surface/")

## A1

la_a1_grid<-la_pre_dat

pred_a1_log<-predict(traffic_nngp_a1_log, X.0=as.matrix(cbind(1,la_pre_dat[,
                                                                   c('land_use_select','population_select','ndvi_select')])),
                 coords.0=as.matrix(la_pre_dat[,c('longitude','latitude')]),
                 n.omp.threads = 10, n.report=1000,
                 verbose=FALSE)


pred_a1<-predict(traffic_nngp_a1, X.0=as.matrix(cbind(1,la_pre_dat[,
                                                                           c('land_use_select','population_select','ndvi_select')])),
                     coords.0=as.matrix(la_pre_dat[,c('longitude','latitude')]),
                     n.omp.threads = 10, n.report=1000,
                     verbose=FALSE)

la_a1_grid$fitted_log<-exp(pred_a1_log$y.0.hat)
la_a1_grid$fitted<-pred_a1$y.0.hat

a1_log_plot<-ggplot(la_a1_grid, aes(x=longitude, y=latitude,
                                    color=fitted_log)) +
  geom_point()+ggtitle("A1 fitted surface: log-scale model") +
  theme(plot.title = element_text(hjust = 0.5))


ggsave(a1_log_plot, width=8, height = 6, file = 'a1_surface_log.pdf')



a1_plot<-ggplot(la_a1_grid, aes(x=longitude, y=latitude,
                                    color=fitted)) +
  geom_point()+ggtitle("A1 fitted surface: natural-scale model") +
  theme(plot.title = element_text(hjust = 0.5))


ggsave(a1_plot, width=8, height = 6, file = 'a1_surface.pdf')

## A2

la_a2_grid<-la_pre_dat

pred_a2_log<-predict(traffic_nngp_a2_log, X.0=as.matrix(cbind(1,la_pre_dat[,
                                                                           c('land_use_select','population_select','ndvi_select')])),
                     coords.0=as.matrix(la_pre_dat[,c('longitude','latitude')]),
                     n.omp.threads = 10, n.report=1000,
                     verbose=FALSE)


pred_a2<-predict(traffic_nngp_a2, X.0=as.matrix(cbind(1,la_pre_dat[,
                                                                   c('land_use_select','population_select','ndvi_select')])),
                 coords.0=as.matrix(la_pre_dat[,c('longitude','latitude')]),
                 n.omp.threads = 10, n.report=1000,
                 verbose=FALSE)

la_a2_grid$fitted_log<-exp(pred_a2_log$y.0.hat)
la_a2_grid$fitted<-pred_a2$y.0.hat

a2_log_plot<-ggplot(la_a2_grid, aes(x=longitude, y=latitude,
                                    color=fitted_log)) +
  geom_point()+ggtitle("A2 fitted surface: log-scale model") +
  theme(plot.title = element_text(hjust = 0.5))


ggsave(a2_log_plot, width=8, height = 6, file = 'a2_surface_log.pdf')



a2_plot<-ggplot(la_a2_grid, aes(x=longitude, y=latitude,
                                color=fitted)) +
  geom_point()+ggtitle("A2 fitted surface: natural-scale model") +
  theme(plot.title = element_text(hjust = 0.5))


ggsave(a2_plot, width=8, height = 6, file = 'a2_surface.pdf')


## A3

la_a3_grid<-la_pre_dat

pred_a3_log<-predict(traffic_nngp_a3_log, X.0=as.matrix(cbind(1,la_pre_dat[,
                                                                           c('land_use_select','population_select','ndvi_select')])),
                     coords.0=as.matrix(la_pre_dat[,c('longitude','latitude')]),
                     n.omp.threads = 10, n.report=1000,
                     verbose=FALSE)


pred_a3<-predict(traffic_nngp_a3, X.0=as.matrix(cbind(1,la_pre_dat[,
                                                                   c('land_use_select','population_select','ndvi_select')])),
                 coords.0=as.matrix(la_pre_dat[,c('longitude','latitude')]),
                 n.omp.threads = 10, n.report=1000,
                 verbose=FALSE)

la_a3_grid$fitted_log<-exp(pred_a3_log$y.0.hat)
la_a3_grid$fitted<-pred_a3$y.0.hat

a3_log_plot<-ggplot(la_a3_grid, aes(x=longitude, y=latitude,
                                    color=fitted_log)) +
  geom_point()+ggtitle("A3 fitted surface: log-scale model") +
  theme(plot.title = element_text(hjust = 0.5))


ggsave(a3_log_plot, width=8, height = 6, file = 'a3_surface_log.pdf')



a3_plot<-ggplot(la_a3_grid, aes(x=longitude, y=latitude,
                                color=fitted)) +
  geom_point()+ggtitle("A3 fitted surface: natural-scale model") +
  theme(plot.title = element_text(hjust = 0.5))


ggsave(a3_plot, width=8, height = 6, file = 'a3_surface.pdf')



## A4

la_a4_grid<-la_pre_dat

pred_a4_log<-predict(traffic_nngp_a4_log, X.0=as.matrix(cbind(1,la_pre_dat[,
                                                                           c('land_use_select','population_select','ndvi_select')])),
                     coords.0=as.matrix(la_pre_dat[,c('longitude','latitude')]),
                     n.omp.threads = 10, n.report=1000,
                     verbose=FALSE)


pred_a4<-predict(traffic_nngp_a4, X.0=as.matrix(cbind(1,la_pre_dat[,
                                                                   c('land_use_select','population_select','ndvi_select')])),
                 coords.0=as.matrix(la_pre_dat[,c('longitude','latitude')]),
                 n.omp.threads = 10, n.report=1000,
                 verbose=FALSE)

la_a4_grid$fitted_log<-exp(pred_a4_log$y.0.hat)
la_a4_grid$fitted<-pred_a4$y.0.hat

a4_log_plot<-ggplot(la_a4_grid, aes(x=longitude, y=latitude,
                                    color=fitted_log)) +
  geom_point()+ggtitle("A4 fitted surface: log-scale model") +
  theme(plot.title = element_text(hjust = 0.5))


ggsave(a4_log_plot, width=8, height = 6, file = 'a4_surface_log.pdf')



a4_plot<-ggplot(la_a4_grid, aes(x=longitude, y=latitude,
                                color=fitted)) +
  geom_point()+ggtitle("A4 fitted surface: natural-scale model") +
  theme(plot.title = element_text(hjust = 0.5))


ggsave(a4_plot, width=8, height = 6, file = 'a4_surface.pdf')


## land use surface 

land_use_plot<-ggplot(la_a4_grid, aes(x=longitude, y=latitude,
                                color=land_use_select)) +
  geom_point()+ggtitle("land use surface") +
  theme(plot.title = element_text(hjust = 0.5))


ggsave(land_use_plot, width=8, height = 6, file = 'land_use_surface.pdf')


## land use surface 

population_plot<-ggplot(la_a4_grid, aes(x=longitude, y=latitude,
                                      color=population_select)) +
  geom_point()+ggtitle("population surface") +
  theme(plot.title = element_text(hjust = 0.5))


ggsave(population_plot, width=8, height = 6, file = 'population_plot_surface.pdf')


## ndvi surface 

ndvi_plot<-ggplot(la_a4_grid, aes(x=longitude, y=latitude,
                                      color=ndvi_select)) +
  geom_point()+ggtitle("ndvi surface") +
  theme(plot.title = element_text(hjust = 0.5))


ggsave(ndvi_plot, width=8, height = 6, file = 'ndvi_surface.pdf')
