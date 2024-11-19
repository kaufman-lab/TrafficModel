##################################################################
##########      Load library 
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
library(rgeos)

# load GEOS,GDAL,PROJ,UDUNITS


#####################################################################
##########      set directory
#####################################################################

data_dir <- 'traffic_data/'
res_dir <- 'results/'
script_dir <- 'scripts/'


##################################################################
##########      Load street by zip code
##################################################################

### part 1 
setwd(data_dir)

if(FALSE){
  
poly.layer <- paste('street_57000_63000')
poly.path<-'data/street/street_57000_63000.shp'
chicago_line<- readOGR(dsn = poly.path,
                   layer = as.character(poly.layer))

#chicago_line <- la_line1

chicago_line$CFCC_agre<-substr(chicago_line$CFCC,1,2)
chicago_line<-chicago_line[chicago_line$CFCC_agre %in% c('A1','A2','A3','A4'),]


chicago_a1_sp <-chicago_line[chicago_line$CFCC_agre=='A1',]
chicago_a2_sp <-chicago_line[chicago_line$CFCC_agre=='A2',]
chicago_a3_sp <-chicago_line[chicago_line$CFCC_agre=='A3',]

#saveRDS(chicago_a1_sp,file='A1_57000_63000.rds')
#save(chicago_a1_sp,file='A1_57000_63000.rda')

#saveRDS(chicago_a2_sp,file='A2_57000_63000.rds')
#save(chicago_a2_sp,file='A2_57000_63000.rda')

#saveRDS(chicago_a3_sp,file='A3_57000_63000.rds')
#save(chicago_a3_sp,file='A3_57000_63000.rda')
}



load(file='A1_57000_63000.rda')
load(file='A2_57000_63000.rda')
load(file='A3_57000_63000.rda')



#### PART_2
setwd(data_dir)

if(FALSE){
  zip_code <-'42000_49000'
  
  ptm <- proc.time()
  system (paste0('wget --no-check-certificate ', 
                 'https://github.com/kaufman-lab/TrafficModel/releases/latest/download/street_',
                 zip_code,'.zip',sep=''))
  
  proc.time() - ptm
  
  system (paste0('unzip street_',
                 zip_code,'.zip',sep=''))
  
}


### part 2
setwd(data_dir)

if(FALSE){
  
  poly.layer <- paste('street_42000_49000')
  poly.path<-'street_42000_49000'
  chicago_line<- readOGR(dsn = poly.path,
                         layer = as.character(poly.layer))
  
  #chicago_line <- la_line1
  
  chicago_line$CFCC_agre<-substr(chicago_line$CFCC,1,2)
  chicago_line<-chicago_line[chicago_line$CFCC_agre %in% c('A1','A2','A3','A4'),]
  
  
  chicago_a1_sp_part2 <-chicago_line[chicago_line$CFCC_agre=='A1',]
  chicago_a2_sp_part2 <-chicago_line[chicago_line$CFCC_agre=='A2',]
  chicago_a3_sp_part2 <-chicago_line[chicago_line$CFCC_agre=='A3',]
  
  #saveRDS(chicago_a1_sp_part2,file='A1_42000_49000.rds')
  #save(chicago_a1_sp_part2,file='A1_42000_49000.rda')
  
  #saveRDS(chicago_a2_sp_part2,file='A2_42000_49000.rds')
  #save(chicago_a2_sp_part2,file='A2_42000_49000.rda')
  
  #saveRDS(chicago_a3_sp_part2,file='A3_42000_49000.rds')
  #save(chicago_a3_sp_part2,file='A3_42000_49000.rda')
}

##################################################################
##########      combine Chicago lines
##################################################################

setwd(data_dir)

chicago_a1_part1 <- readRDS(file='A1_57000_63000.rds')
chicago_a1_part2 <- readRDS(file='A1_42000_49000.rds')

chicago_a2_part1 <- readRDS(file='A2_57000_63000.rds')
chicago_a2_part2 <- readRDS(file='A2_42000_49000.rds')

chicago_a3_part1 <- readRDS(file='A3_57000_63000.rds')
chicago_a3_part2 <- readRDS(file='A3_42000_49000.rds')


chicago_a1_all <- rbind(chicago_a1_part1,chicago_a1_part2)
chicago_a2_all <- rbind(chicago_a2_part1,chicago_a2_part2)
chicago_a3_all <- rbind(chicago_a3_part1,chicago_a3_part2)


##################################################################
##########      load Chicago shapefile
##################################################################

### for example, Chicago
if(FALSE){
setwd("~/Chicago_NOx/Data")
poly.layer <- paste('Chicago_boundary')
poly.path<-'Chicago_boundary'
chicago_shp <- readOGR(dsn = poly.path,verbose = FALSE,
                  layer = as.character(poly.layer))

chicago_shp <- spTransform(chicago_shp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

plot(chicago_shp)

### create buffered region with width 50km 
chicago_tras <- spTransform( chicago_shp, CRS( "+init=epsg:3347" ) ) 
b_chicago_tras <- gBuffer(chicago_tras,width = 100000)

b_chicago_shp <- spTransform(b_chicago_tras, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

plot(b_chicago_shp)
plot(chicago_shp,add=T)
}


pts <- matrix(
  c(
    41.38, -88.5,
    42.4,-88.5,
    42.4,-87.35,
    41.38,-87.35
  ),
  ncol = 2, byrow = TRUE
)

poly.pred <- Polygon(pts[,c(2,1)])      # class Polygon
poly.pred <- SpatialPolygons(list(Polygons(list(poly.pred), ID = 1)))

crs(poly.pred) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

chicago_tras <- spTransform( poly.pred, CRS( "+init=epsg:3347" ) ) 
b_chicago_tras <- gBuffer(chicago_tras,width = 25000)

b_chicago_shp <- spTransform(b_chicago_tras, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

plot(b_chicago_shp)
plot(poly.pred,add=T)

##################################################################
##########      subset street to Chicago region
##################################################################


# only keep streets that are in LA
chicago_buffered_a1_final <- chicago_a1_all[b_chicago_shp,] 
chicago_buffered_a2_final <- chicago_a2_all[b_chicago_shp,] 
chicago_buffered_a3_final <- chicago_a3_all[b_chicago_shp,] 

plot(b_chicago_shp)
plot(poly.pred,add=T)
plot(chicago_buffered_a3_final,add=T)




setwd(data_dir)



if(FALSE){
#saveRDS(chicago_buffered_a1,file='Chicago_buffered_A1.rds')
#saveRDS(chicago_buffered_a2,file='Chicago_buffered_A2.rds')
#saveRDS(chicago_buffered_a3,file='Chicago_buffered_A3.rds')
}

setwd(data_dir)

saveRDS(chicago_buffered_a1_final,file='Chicago_buffered_A1_final.rds')
saveRDS(chicago_buffered_a2_final,file='Chicago_buffered_A2_final.rds')
saveRDS(chicago_buffered_a3_final,file='Chicago_buffered_A3_final.rds')


