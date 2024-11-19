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
##########                   A1
#####################################################################


# setwd("~/caline_traffic/traffic merged diagnostics/linear prediction/linear_fitted/")


## 10 fold CV, A1

set.seed(2020)


theta.alpha <- as.matrix(expand.grid(seq(0.01,1,length.out=20),
                                     seq(0.5,10,length.out=20)))

colnames(theta.alpha) <- c("alpha","phi")


ptm <- proc.time()



prepare_dat<-A1_regre
traffic_nngp_a1 <- spConjNNGP(log_traffic~land_use_select+population_select+
                                    ndvi_select,
                                  coords=c("LINE_LONGI","LINE_LATIT"),
                                  data=prepare_dat,
                                  cov.model="exponential", sigma.sq.IG=c(2,
                                                                         0.5*var(prepare_dat$log_traffic)),
                                  n.neighbors=15, 
                                  theta.alpha=theta.alpha,
                                  k.fold = 2, score.rule = "rmspe",
                                  fit.rep=TRUE, n.samples=200,
                                  n.omp.threads=10)
  

setwd("~/spNNGP_results/full_model/")

saveRDS(traffic_nngp_a1,'a1_full_log.rds')
proc.time() - ptm

summary(traffic_nngp_a1)









#####################################################################
##########                   A2
#####################################################################


# setwd("~/caline_traffic/traffic merged diagnostics/linear prediction/linear_fitted/")


## 10 fold CV, A2

set.seed(2020)


theta.alpha <- as.matrix(expand.grid(seq(0.01,1,length.out=20),
                                     seq(0.5,10,length.out=20)))

colnames(theta.alpha) <- c("alpha","phi")


ptm <- proc.time()



prepare_dat<-A2_regre
traffic_nngp_a2 <- spConjNNGP(log_traffic~land_use_select+population_select+
                                ndvi_select,
                              coords=c("LINE_LONGI","LINE_LATIT"),
                              data=prepare_dat,
                              cov.model="exponential", sigma.sq.IG=c(2,
                                                                     0.5*var(prepare_dat$log_traffic)),
                              n.neighbors=15, 
                              theta.alpha=theta.alpha,
                              k.fold = 2, score.rule = "rmspe",
                              fit.rep=TRUE, n.samples=200,
                              n.omp.threads=10)


setwd("~/spNNGP_results/full_model/")

saveRDS(traffic_nngp_a2,'a2_full_log.rds')
proc.time() - ptm

summary(traffic_nngp_a2)









#####################################################################
##########                   A3
#####################################################################


# setwd("~/caline_traffic/traffic merged diagnostics/linear prediction/linear_fitted/")


## 10 fold CV, A1

set.seed(2020)


theta.alpha <- as.matrix(expand.grid(seq(0.01,1,length.out=20),
                                     seq(0.5,15,length.out=30)))

colnames(theta.alpha) <- c("alpha","phi")


ptm <- proc.time()



prepare_dat<-A3_regre
traffic_nngp_a3 <- spConjNNGP(log_traffic~land_use_select+population_select+
                                ndvi_select,
                              coords=c("LINE_LONGI","LINE_LATIT"),
                              data=prepare_dat,
                              cov.model="exponential", sigma.sq.IG=c(2,
                                                                     0.5*var(prepare_dat$log_traffic)),
                              n.neighbors=15, 
                              theta.alpha=theta.alpha,
                              k.fold = 2, score.rule = "rmspe",
                              fit.rep=TRUE, n.samples=200,
                              n.omp.threads=10)


setwd("~/spNNGP_results/full_model/")

saveRDS(traffic_nngp_a3,'a3_full_log.rds')
proc.time() - ptm




#summary(traffic_nngp_a3)
#





#####################################################################
##########                   A4
#####################################################################


# setwd("~/caline_traffic/traffic merged diagnostics/linear prediction/linear_fitted/")


## 10 fold CV, A4

set.seed(2020)


theta.alpha <- as.matrix(expand.grid(seq(0.01,1,length.out=20),
                                     seq(0.5,15,length.out=30)))

colnames(theta.alpha) <- c("alpha","phi")


ptm <- proc.time()



prepare_dat<-A4_regre
traffic_nngp_a4 <- spConjNNGP(log_traffic~land_use_select+population_select+
                                ndvi_select,
                              coords=c("LINE_LONGI","LINE_LATIT"),
                              data=prepare_dat,
                              cov.model="exponential", sigma.sq.IG=c(2,
                                                                     0.5*var(prepare_dat$log_traffic)),
                              n.neighbors=15, 
                              theta.alpha=theta.alpha,
                              k.fold = 2, score.rule = "rmspe",
                              fit.rep=TRUE, n.samples=200,
                              n.omp.threads=10)


setwd("~/spNNGP_results/full_model/")

saveRDS(traffic_nngp_a4,'a4_full_log.rds')
saveRDS(traffic_nngp_a1,'a1_full_log.rds')

proc.time() - ptm

summary(traffic_nngp_a1)


if(FALSE){
  
a4_obs<-good_tp_A4[good_tp_A4$LATITUDE>33.95&
                     good_tp_A4$LATITUDE<34.1&
                     good_tp_A4$LONGITUDE>(-118.6)&
                     good_tp_A4$LONGITUDE<(-118.3),]



a1_obs<-good_tp_A1[good_tp_A1$LINE_LATIT>33.95&
                     good_tp_A1$LINE_LATIT<34.1&
                     good_tp_A1$LINE_LONGI>(-118.6)&
                     good_tp_A1$LINE_LONGI<(-118.3),]

good_tp_A1$LINE_LONGI
pred_a1<-predict(traffic_nngp_a1, X.0=as.matrix(cbind(1,good_tp_A1[,
                                                                   c('land_use_select','population_select','ndvi_select')])),
                 coords.0=as.matrix(good_tp_A1[,c('LINE_LONGI','LINE_LATIT')]),
                 n.omp.threads = 10, n.report=1000,
                 verbose=FALSE)

good_tp_A1$fitted_log<-pred_a1$y.0.hat

good_tp_A1$fitted<-exp(good_tp_A1$fitted_log)

good_tp_A1$log_traffic[1:10]
good_tp_A1$fitted_log[1:10]

rss <- sum((good_tp_A1$fitted_log - good_tp_A1$log_traffic) ^ 2)  ## residual sum of squares
tss <- var(good_tp_A1$log_traffic)*(length(good_tp_A1$log_traffic)-1)
rsq <- 1 - rss/tss

}

# plotting 

setwd("~/spNNGP_results/full_model/")

saveRDS(traffic_nngp_a4,'a4_full_log.rds')
saveRDS(traffic_nngp_a1,'a1_full_log.rds')

setwd("~/spNNGP_results/LA_surface//")

ggsave(g2, width=10, height = 5, file = 'a1_log.pdf')
