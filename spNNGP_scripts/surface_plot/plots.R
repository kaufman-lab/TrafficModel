
a2_nngp_temp <- spConjNNGP(median_traffic~land_use_select+population_select+
                                  ndvi_select, coords=c("LINE_LONGI","LINE_LATIT"),
                                data=A2_other_regre,
                                cov.model="exponential", sigma.sq.IG=c(2,
                                                                       0.5*var(A2_other_regre$median_traffic)),
                                n.neighbors=15, 
                                theta.alpha=theta.alpha,
                                k.fold = 2, score.rule = "rmspe",
                                fit.rep=TRUE, n.samples=200,
                                n.omp.threads=4)

temp_pred<-predict(a2_nngp_temp, X.0=as.matrix(cbind(1,A25_regre[,4:6])),
                   coords.0=as.matrix(A25_regre[,2:3]),
                   n.omp.threads = 10, n.report=1000)


a25_by_a2o<-good_tp_A25[,c('GEOID','median_traffic','LINE_LONGI','LINE_LATIT' )]
a25_by_a2o$fitted<-temp_pred$y.0.hat

plot(a25_by_a2o$fitted,a25_by_a2o$median_traffic)

a25_from_a2o<-rbind(a25_by_a2o,good_tp_A2_other_rep[,1:5])


SSTotal <- var( a25_from_a2o$median_traffic ) * (nrow(a25_from_a2o)-1)
SSE <- sum( (a25_from_a2o$fitted-a25_from_a2o$median_traffic)^2 )

1-SSE/SSTotal