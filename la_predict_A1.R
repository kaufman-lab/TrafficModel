##################################################################
##########      Load library and data
##################################################################
library(foreign)
library(data.table)
library(ggplot2)
library(stringr)
library(tidyselect)
library(rlist)
library(geoR)
library(usmap)
library(raster)
library(sp)
library(maps)
library(maptools)

library(dplyr)
library(spNNGP)
library(geosphere)
library(rgdal)
library(sf)
library(RgoogleMaps)
# load GEOS,GDAL,PROJ,UDUNITS

#####################################################################
##########      Read data
#####################################################################
## read boundary file


setwd("~/traffic_data")
poly.layer <- paste('City_Boundaries')
poly.path<-'City_Boundaries'
la_shp <- readOGR(dsn = poly.path,verbose = FALSE,
                  layer = as.character(poly.layer))

la_boundary<-la_shp[la_shp$OBJECTID==44767,]


## read street file 
setwd("~/traffic_data/")

if(FALSE){# for first time
  poly.layer <- paste('street_la')
  poly.path<-'street_la'
  la_line<- readOGR(dsn = poly.path,
                    layer = as.character(poly.layer))
}
#save(la_line,file='la_street.rda')
load('la_street.rda')

la_line$CFCC_agre<-substr(la_line$CFCC,1,2)

la_line<-la_line[la_line$CFCC_agre %in% c('A1','A2','A3','A4'),]

# load spNNGP models
setwd("~/spNNGP_results/full_model/")

traffic_nngp_a1<-readRDS('a1_full_log.rds')
traffic_nngp_a2<-readRDS('a2_full_log.rds')
traffic_nngp_a3<-readRDS('a3_full_log.rds')
traffic_nngp_a4<-readRDS('a4_full_log.rds')


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

##################################################################
##########      Prepare raster for covariates
##################################################################

setwd("~/traffic_data/")

# load grid data for covariates in LA
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


# rasterize the covariates data
prep_ras<-function(grid_dat,cov_name){
  
  grid_dat$x<-grid_dat$longitude
  grid_dat$y<-grid_dat$latitude
  
  tmp_ext<-extent(grid_dat[,c('x','y')])
  
  ras_empty<-raster(tmp_ext,ncol=700,nrow=700)
  
  ras_tmp<-rasterize(grid_dat[,c('longitude','latitude')],
                     ras_empty,grid_dat[,cov_name],
                     fun=mean)
  
  return(ras_tmp)
  #plot(ras_a2,col=terrain.colors(ncolors))
  
  
}

pop_ras<-prep_ras(la_pre_dat,'population_select')
ndvi_ras<-prep_ras(la_pre_dat,'ndvi_select')
lu_ras<-prep_ras(la_pre_dat,'land_use_select')



##################################################################
##########   function for getting road segments
##################################################################

get_segs<-function(la_tsp){
  
  temp_xy<-as.data.frame(coordinates(la_tsp))
  
  road_segs_list <-lapply(X = 1:(dim(temp_xy)[1]-1), FUN = function(x) {
    
    return(cbind(temp_xy[x,],temp_xy[x+1,]))
    
  })
  
  road_segs_xy<- do.call("rbind", road_segs_list)
  colnames(road_segs_xy)<- c('X_b','Y_b','X_e','Y_e')
  
  
  
  return(road_segs_xy)
}

##################################################################
##########   road segments for A1
##################################################################
la_a1_sp<-la_street[la_street$CFCC=='A15',]

road_line_obj <- la_a1_sp

CFCC_segs_list <-lapply(X = 1:dim(road_line_obj)[1], FUN = function(x) {
  
  return(get_segs(road_line_obj[x,]))}
)  

CFCC_segs<- do.call("rbind", CFCC_segs_list)

CFCC_segs$X_mid <- (CFCC_segs$X_b+CFCC_segs$X_e)/2
CFCC_segs$Y_mid <- (CFCC_segs$Y_b+CFCC_segs$Y_e)/2

CFCC_segs$pop<-raster::extract(pop_ras,
                                  as.matrix(CFCC_segs[,c('X_mid','Y_mid')]))

CFCC_segs$ndvi<-raster::extract(ndvi_ras,
                                   as.matrix(CFCC_segs[,c('X_mid','Y_mid')]))

CFCC_segs$lu<-raster::extract(lu_ras,
                                 as.matrix(CFCC_segs[,c('X_mid','Y_mid')]))

CFCC_segs_la <- CFCC_segs[!is.na(CFCC_segs$pop),]



##################################################################
##########   Function for prediction and mapping
##################################################################

prep_line_grid<-function(traffic_model, street_grid){
  
  pred_road<-predict(traffic_model, 
                     X.0=as.matrix(cbind(1,street_grid[,c('lu','pop','ndvi')])),
                     coords.0=as.matrix(street_grid[,c('X','Y')]),
                     n.omp.threads = 10, n.report=1000,
                     verbose=FALSE)
  
  street_grid$fitted<-exp(pred_road$y.0.hat)
  
  # get map
  road_xy<-street_grid[,c('Y','X')]
  road_center<-as.vector(colMeans(road_xy))
  la_map<-GetMap(center=road_center,zoom=12)
  #PlotOnStaticMap(la_map)
  
  
  # prepare sp data 
  coordinates(street_grid)<-~X+Y
  proj4string(street_grid)<-sp::CRS("+init=epsg:4326")
  
  return(list(la_map,street_grid))
}



##################################################################
##########   Predict using spNNGP
##################################################################

## making prediction
CFCC_segs_la$X<- CFCC_segs_la$X_mid
CFCC_segs_la$Y<- CFCC_segs_la$Y_mid

a1_obj<-prep_line_grid(traffic_nngp_a1,CFCC_segs_la)

## visualization 
bubbleMap(SP=a1_obj[[2]],
          coords = c('Y','X'),
          map=a1_obj[[1]],zcol='fitted',
          legendLoc = 'bottomleft',
          max.radius = 1.5,
          alpha = 0.5,
          strokeWeight=0)

## prepare input for RLINE
CFCC_segs_la <- as.data.frame(a1_obj[[2]])
CFCC_segs_la$Emis<-CFCC_segs_la$fitted

# add additional columns
CFCC_segs_la$dCL <-0
CFCC_segs_la$sigmaz0 <- 2
CFCC_segs_la$'#lanes' <- 4
CFCC_segs_la$Hw1 <- 0
CFCC_segs_la$dw1 <- 0
CFCC_segs_la$Hw2 <- 0
CFCC_segs_la$dw2 <- 0
CFCC_segs_la$Depth <- 0
CFCC_segs_la$Wtop <- 0
CFCC_segs_la$Wbottom <- 0
CFCC_segs_la$Group <- 'G1'

# convert lat long to utm (meters)
xy_beg <- CFCC_segs_la[,c('X_b','Y_b')]
xy_end <- CFCC_segs_la[,c('X_e','Y_e')]
  
  
latlong_to_utm <- function(xy_frame){
  
  colnames(xy_frame) <- c('X','Y')
  coordinates(xy_frame) <- c('X','Y')
  proj4string(xy_frame) <- CRS("+proj=longlat +datum=WGS84")
  
  zone <- ceiling(as.numeric(xy_frame$X[1])/6 + 30)
  utm_frame <- spTransform(xy_frame, CRS(paste0('+proj=utm +zone=',zone,' ellps=WGS84')))
  
  return(coordinates(utm_frame))
}


CFCC_segs_la[,c('X_b','Y_b')] <-  latlong_to_utm(xy_beg)
CFCC_segs_la[,c('X_e','Y_e')] <-  latlong_to_utm(xy_end)

CFCC_segs_la$Z_b <- 1
CFCC_segs_la$Z_e <- 1

## final processing
line_source <- CFCC_segs_la[,c('Group',  'X_b',  'Y_b' ,
                               'Z_b' , 'X_e' ,'Y_e' ,'Z_e' ,'dCL',
                               'sigmaz0', '#lanes' , 'Emis',  'Hw1',
                               'dw1',  'Hw2',  'dw2', 'Depth',  'Wtop', 'Wbottom')]

line_source <- line_source %>% mutate(across(where(is.numeric), ~ format(round(., 1))))


write.table(line_source,file='LA_a1_rline_input.txt',sep='\t',
            row.names = FALSE,col.names = TRUE,quote=FALSE)



##################################################################
##########   Random location for output
##################################################################

recep_loc <- la_pre_dat[sample(nrow(la_pre_dat),1000),]
recep_loc[,c('X_coordinate','Y_Coordinate')] <-  latlong_to_utm(recep_loc[,c('longitude','latitude')])
recep_loc$Z_Coordinate <- 1

recept_frame <- recep_loc[,c('X_coordinate','Y_Coordinate','Z_Coordinate')]
recept_frame <- recept_frame %>% mutate(across(where(is.numeric), ~ format(round(., 1))))
