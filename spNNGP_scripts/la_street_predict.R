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

library(spNNGP)
library(geosphere)
library(rgdal)
library(sf)
library(RgoogleMaps)
# load GEOS,GDAL,PROJ,UDUNITS

#####################################################################
##########      Read data
#####################################################################

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
##########   Mid point for each roadtype
##################################################################

# function for getting the midpoints from sf (line) objects

st_line_midpoints <- function(sf_lines = NULL) {
  
  g <- st_geometry(sf_lines)
  
  g_mids <- lapply(g, function(x) {
    
    coords <- as.matrix(x)
    
    # this is just a copypaste of View(maptools:::getMidpoints):
    get_mids <- function (coords) {
      dist <- sqrt((diff(coords[, 1])^2 + (diff(coords[, 2]))^2))
      dist_mid <- sum(dist)/2
      dist_cum <- c(0, cumsum(dist))
      end_index <- which(dist_cum > dist_mid)[1]
      start_index <- end_index - 1
      start <- coords[start_index, ]
      end <- coords[end_index, ]
      dist_remaining <- dist_mid - dist_cum[start_index]
      mid <- start + (end - start) * (dist_remaining/dist[start_index])
      return(mid)
    }
    
    mids <- st_point(get_mids(coords))
  })
  
  out <- st_sfc(g_mids, crs = st_crs(sf_lines))
  out <- st_sf(out)
}

prep_street_grid<-function(CFCC_select=NA){
  
la_line_sub<-la_line[la_line$CFCC_agre==CFCC_select,]
la_sub_sf<-st_as_sf(la_line_sub)

la_sub_mid<-st_line_midpoints(la_sub_sf)
la_sub_dat<-as.data.frame(st_coordinates(la_sub_mid))

# extract population, ndvi and landuse
la_sub_dat$pop<-raster::extract(pop_ras,
                                as.matrix(la_sub_dat[,c('X','Y')]))

la_sub_dat$ndvi<-raster::extract(ndvi_ras,
                                as.matrix(la_sub_dat[,c('X','Y')]))

la_sub_dat$lu<-raster::extract(lu_ras,
                                as.matrix(la_sub_dat[,c('X','Y')]))

# drop NA
la_sub_dat<-la_sub_dat[complete.cases(la_sub_dat),]

return(la_sub_dat)
}

### grid for each road type

a1_street_grid<-prep_street_grid('A1')
a2_street_grid<-prep_street_grid('A2')
a3_street_grid<-prep_street_grid('A3')
a4_street_grid<-prep_street_grid('A4')


##################################################################
##########   Predict using spNNGP
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

a2_obj<-prep_line_grid(traffic_nngp_a2,a2_street_grid)

bubbleMap(SP=a2_obj[[2]],
          coords = c('Y','X'),
          map=a2_obj[[1]],zcol='fitted',
          legendLoc = 'bottomleft',
          max.radius = 1.5,
          alpha = 0.5,
          key.entries = c(40000,50000,60000,
                          70000,80000),
          strokeWeight=0)


########################################################
##########   Prediction surface
########################################################

pred_surf_ras<-function(traffic_model,grid_dat){

pred_traf<-predict(traffic_model, 
                   X.0=as.matrix(cbind(1,
                grid_dat[,c('land_use_select','population_select','ndvi_select')])),
                   coords.0=as.matrix(grid_dat[,c('longitude','latitude')]),
                   n.omp.threads = 10, n.report=1000,
                   verbose=FALSE)
grid_dat$fitted<-exp(pred_traf$y.0.hat)

traf_ras<-prep_ras(grid_dat,'fitted')

}

a2_pred_surf<-pred_surf_ras(traffic_nngp_a2,la_pre_dat)
ncolors<-200
plot(a2_pred_surf,col=terrain.colors(ncolors))

########################################################
##########   Observed traffic points
########################################################
la_center<-as.vector(colMeans(la_pre_dat[,
                      c("latitude","longitude")]))

prep_obs_pt<-function(grid_ras,pt_dat){
  
  # create sp object for observed points
  coordinates(pt_dat)<-~LINE_LONGI+LINE_LATIT
  pt_dat<-crop(pt_dat,extent(grid_ras))
  proj4string(pt_dat)<-sp::CRS("+init=epsg:4326")

  # create base map for observed points
  p_dataf<-as.data.frame(pt_dat)
  pt_xy<-p_dataf[,c("LINE_LATIT","LINE_LONGI")]
  pt_center<-as.vector(colMeans(pt_xy))
  la_map<-GetMap(center=pt_center,zoom=12)
  
  
  return(list(la_map,pt_dat))
  
}

a1_pt_obj<-prep_obs_pt(a1_pred_surf,A1_regre)

bubbleMap(SP=a2_pt_obj[[2]],
          coords = c('LINE_LONGI','LINE_LATIT'),
          map=a2_pt_obj[[1]],zcol='median_traffic',
          legendLoc = 'bottomleft',
          max.radius = 100,
          alpha = 0.5,
          key.entries = 10000+20000*c(0:5))

34.023792,-118.355636