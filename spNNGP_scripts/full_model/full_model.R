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
##########                   A1
#####################################################################


# setwd("~/caline_traffic/traffic merged diagnostics/linear prediction/linear_fitted/")


## 10 fold CV, A1

set.seed(2020)


theta.alpha <- as.matrix(expand.grid(seq(0.01,1,length.out=10),
                                     seq(1,10,length.out=10)))

colnames(theta.alpha) <- c("alpha","phi")


ptm <- proc.time()



prepare_dat<-A1_regre
traffic_nngp_a1 <- spConjNNGP(median_traffic~land_use_select+population_select+
                                    ndvi_select,
                                  coords=c("LINE_LONGI","LINE_LATIT"),
                                  data=prepare_dat,
                                  cov.model="exponential", sigma.sq.IG=c(2,
                                                                         0.5*var(prepare_dat$median_traffic)),
                                  n.neighbors=15, 
                                  theta.alpha=theta.alpha,
                                  k.fold = 2, score.rule = "rmspe",
                                  fit.rep=TRUE, n.samples=200,
                                  n.omp.threads=10)
  

setwd("~/spNNGP_results/full_model/")

saveRDS(traffic_nngp_a1,'a1_full.rds')
proc.time() - ptm

summary(traffic_nngp_a1)









#####################################################################
##########                   A2
#####################################################################


# setwd("~/caline_traffic/traffic merged diagnostics/linear prediction/linear_fitted/")


## A2
set.seed(2020)


theta.alpha <- as.matrix(expand.grid(seq(0.01,1,length.out=15),
                                     seq(1,15,length.out=15)))

colnames(theta.alpha) <- c("alpha","phi")


ptm <- proc.time()



prepare_dat<-A2_regre
traffic_nngp_a2 <- spConjNNGP(median_traffic~land_use_select+population_select+
                                ndvi_select,
                              coords=c("LINE_LONGI","LINE_LATIT"),
                              data=prepare_dat,
                              cov.model="exponential", sigma.sq.IG=c(2,
                                                                     0.5*var(prepare_dat$median_traffic)),
                              n.neighbors=15, 
                              theta.alpha=theta.alpha,
                              k.fold = 2, score.rule = "rmspe",
                              fit.rep=TRUE, n.samples=200,
                              n.omp.threads=10)


setwd("~/spNNGP_results/full_model/")

saveRDS(traffic_nngp_a2,'a2_full.rds')
proc.time() - ptm

#summary(traffic_nngp_a2)
#







#####################################################################
##########                   A3
#####################################################################


# setwd("~/caline_traffic/traffic merged diagnostics/linear prediction/linear_fitted/")


## A3
set.seed(2020)


theta.alpha <- as.matrix(expand.grid(seq(0.01,1,length.out=15),
                                     seq(1,15,length.out=15)))

colnames(theta.alpha) <- c("alpha","phi")


ptm <- proc.time()



prepare_dat<-A3_regre
traffic_nngp_a3 <- spConjNNGP(median_traffic~land_use_select+population_select+
                                ndvi_select,
                              coords=c("LINE_LONGI","LINE_LATIT"),
                              data=prepare_dat,
                              cov.model="exponential", sigma.sq.IG=c(2,
                                                                     0.5*var(prepare_dat$median_traffic)),
                              n.neighbors=15, 
                              theta.alpha=theta.alpha,
                              k.fold = 2, score.rule = "rmspe",
                              fit.rep=TRUE, n.samples=200,
                              n.omp.threads=10)


setwd("~/spNNGP_results/full_model/")

saveRDS(traffic_nngp_a3,'a3_full.rds')
proc.time() - ptm

#summary(traffic_nngp_a3)
#





#####################################################################
##########                   A4
#####################################################################


# setwd("~/caline_traffic/traffic merged diagnostics/linear prediction/linear_fitted/")


## A4
set.seed(2020)


theta.alpha <- as.matrix(expand.grid(seq(0.01,1,length.out=15),
                                     seq(1,15,length.out=15)))

colnames(theta.alpha) <- c("alpha","phi")


ptm <- proc.time()



prepare_dat<-A4_regre
traffic_nngp_a4 <- spConjNNGP(median_traffic~land_use_select+population_select+
                                ndvi_select,
                              coords=c("LINE_LONGI","LINE_LATIT"),
                              data=prepare_dat,
                              cov.model="exponential", sigma.sq.IG=c(2,
                                                                     0.5*var(prepare_dat$median_traffic)),
                              n.neighbors=15, 
                              theta.alpha=theta.alpha,
                              k.fold = 2, score.rule = "rmspe",
                              fit.rep=TRUE, n.samples=200,
                              n.omp.threads=10)


setwd("~/spNNGP_results/full_model/")

saveRDS(traffic_nngp_a4,'a4_full.rds')
proc.time() - ptm

#summary(traffic_nngp_a4)
#





