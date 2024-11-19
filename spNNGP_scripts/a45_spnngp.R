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
good_tp_A45<-good_tp[good_tp$CFCC=='A45',]
good_tp_A25<-good_tp[good_tp$CFCC=='A25',]

good_tp_A4_other<-good_tp_A4[good_tp_A4$CFCC!='A45',]
good_tp_A3_other<-good_tp_A3[good_tp_A3$CFCC!='A35',]

good_tp_A2_other<-good_tp_A2[good_tp_A2$CFCC!='A25',]


good_tp_A3$A35_ind<-as.numeric(good_tp_A3$CFCC=='A35')
good_tp_A3$int_lu<-good_tp_A3$land_use_select*good_tp_A3$A35_ind
good_tp_A3$int_pop<-good_tp_A3$population_select*good_tp_A3$A35_ind
good_tp_A3$int_ndvi<-good_tp_A3$ndvi_select*good_tp_A3$A35_ind

A3_regre<-good_tp_A3[,c('median_traffic','LINE_LONGI','LINE_LATIT',
                        'land_use_select','population_select',
                        'ndvi_select','A35_ind','int_lu',
                        'int_pop','int_ndvi')]

A2_other_regre<-good_tp_A2_other[,c('median_traffic','LINE_LONGI','LINE_LATIT',
                          'land_use_select','population_select',
                          'ndvi_select' )]



#####################################################################
##########                   CV
#####################################################################


# setwd("~/caline_traffic/traffic merged diagnostics/linear prediction/linear_fitted/")


## 10 fold CV, A1

set.seed(2020)

cvSplits1 <- createFolds(A3_regre$median_traffic,
                         k = 10, #k-fold,180training20testing
                         returnTrain = TRUE)

good_tp_A3_rep = good_tp_A3[,c('GEOID','median_traffic','LINE_LONGI','LINE_LATIT','CFCC' )]

theta.alpha <- as.matrix(expand.grid(seq(0.01,1,length.out=10),
                                     seq(3,10,length.out=10)))
colnames(theta.alpha) <- c("alpha","phi")

good_tp_A3_rep$fitted <- NA

K <- 10

ptm <- proc.time()


for (k in 1:K)
{
  trRows <- cvSplits1[[k]]
  
  temp_tr<-A3_regre[trRows,]
  temp_test<-A3_regre[-trRows,]
  
  traffic_nngp_temp <- spConjNNGP(median_traffic~land_use_select+population_select+
                                    ndvi_select+A35_ind+int_lu+
                                    int_pop+int_ndvi,
                                  coords=c("LINE_LONGI","LINE_LATIT"),
                                  data=temp_tr,
                                  cov.model="exponential", sigma.sq.IG=c(2,
                                                                         0.5*var(temp_tr$median_traffic)),
                                  n.neighbors=15, 
                                  theta.alpha=theta.alpha,
                                  k.fold = 2, score.rule = "rmspe",
                                  fit.rep=TRUE, n.samples=200,
                                  n.omp.threads=10)
  
  temp_pred<-predict(traffic_nngp_temp, X.0=as.matrix(cbind(1,temp_test[,4:10])),
                     coords.0=as.matrix(temp_test[,2:3]),
                     n.omp.threads = 10, n.report=1000)
  
  good_tp_A3_rep[-trRows,]$fitted <- temp_pred$y.0.hat
  
  
}

setwd("~/spNNGP_results/A3")

saveRDS(good_tp_A3_rep,'A3_3select_a35ind_interact_nat_traf.rds')
proc.time() - ptm

summary(traffic_nngp_temp)
SSTotal <- var( good_tp_A3_rep$median_traffic ) * (nrow(good_tp_A3_rep)-1)
SSE <- sum( (good_tp_A3_rep$fitted-good_tp_A3_rep$median_traffic)^2 )

1-SSE/SSTotal





#####################################################################
##########                   CV
#####################################################################


# setwd("~/caline_traffic/traffic merged diagnostics/linear prediction/linear_fitted/")


## 10 fold CV, A1

set.seed(2020)

cvSplits2o <- createFolds(A2_other_regre$median_traffic,
                         k = 10, #k-fold,180training20testing
                         returnTrain = TRUE)

good_tp_A2_other_rep = good_tp_A2_other[,c('GEOID','median_traffic','LINE_LONGI','LINE_LATIT' )]

theta.alpha <- as.matrix(expand.grid(seq(0.01,1,length.out=15),
                                     seq(1,8,length.out=15)))
colnames(theta.alpha) <- c("alpha","phi")

good_tp_A2_other_rep$fitted <- NA

K <- 10

ptm <- proc.time()


for (k in 1:K)
{
  trRows <- cvSplits2o[[k]]
  
  temp_tr<-A2_other_regre[trRows,]
  temp_test<-A2_other_regre[-trRows,]
  
  traffic_nngp_temp <- spConjNNGP(median_traffic~land_use_select+population_select+
                                    ndvi_select, coords=c("LINE_LONGI","LINE_LATIT"),
                                  data=temp_tr,
                                  cov.model="exponential", sigma.sq.IG=c(2,
                                                                         0.5*var(temp_tr$median_traffic)),
                                  n.neighbors=15, 
                                  theta.alpha=theta.alpha,
                                  k.fold = 2, score.rule = "rmspe",
                                  fit.rep=TRUE, n.samples=200,
                                  n.omp.threads=4)
  
  temp_pred<-predict(traffic_nngp_temp, X.0=as.matrix(cbind(1,temp_test[,4:6])),
                     coords.0=as.matrix(temp_test[,2:3]),
                     n.omp.threads = 10, n.report=1000)
  
  good_tp_A2_other_rep[-trRows,]$fitted <- temp_pred$y.0.hat
  
  
}

setwd("~/spNNGP_results/A2")

saveRDS(good_tp_A2_other_rep,'A2_other_3select_nat_traf.rds')
proc.time() - ptm


good_tp_A2_rep<-rbind(good_tp_A25_rep,good_tp_A2_other_rep)
SSTotal <- var( good_tp_A2_rep$median_traffic ) * (nrow(good_tp_A2_rep)-1)
SSE <- sum( (good_tp_A2_rep$fitted-good_tp_A2_rep$median_traffic)^2 )

1-SSE/SSTotal


setwd("~/spNNGP_results/A3")
good_tp_A4_p1<-readRDS('A3_other_3select_nat_traf.rds')
good_tp_A4_p2<-readRDS('A35_3select_nat_traf.rds')
good_tp_A4_rep<-rbind(good_tp_A4_p1,good_tp_A4_p2)
SSTotal <- var( good_tp_A4_rep$median_traffic ) * (nrow(good_tp_A4_rep)-1)
SSE <- sum( (good_tp_A4_rep$fitted-good_tp_A4_rep$median_traffic)^2 )
