# needs to be modified to run in loop

library(spNNGP)
library(tidyverse)
library(sf)

roads <- read_sf('data/us_roads.sqlite')#, query = 'select * from us_roads where state = \'53\' and county = \'073\'')

coords <- roads %>%
	st_geometry() %>%
	st_coordinates() %>% # break linestrings into line segments
	as_tibble() %>%
	group_by(L1) %>%
	rename(
		X_b = X,
		Y_b = Y
	) %>% 
	mutate(
		X_e = lead(X_b),
		Y_e = lead(Y_b),
		X = (X_b + X_e)/2, # Calculate end and midpoints
		Y = (Y_b + Y_e)/2
	) %>%
	na.omit() 

roads <- inner_join( # join attributes back to line segments
	roads %>% mutate(L1 = row_number()),
	coords,
	by = 'L1'
) %>%
	split(.$class)

# Model traffic data for each class of road

prep_line_grid<-function(traffic_model, street_grid){
	
	pred_road<-predict(traffic_model, 
										 X.0=as.matrix(rep(1, dim(street_grid)[1])),
										 coords.0=as.matrix(street_grid[,c('X','Y')]),
										 n.omp.threads = 10, n.report=1000,
										 verbose=FALSE)
	
	fitted<-exp(pred_road$y.0.hat)
	return(fitted[,1])
}

roads$A1$fitted <- st_drop_geometry(roads$A1) %>%
	mutate(batch = ceiling(row_number()/10000)) %>%
	split(.$batch) %>%
	map(function(x) prep_line_grid(readRDS('data/models/a1_full_nocov_log.rds'), x)) %>%
	unlist()
# 	
# roads$A2$fitted <- prep_line_grid(readRDS('data/models/a2_full_nocov_log.rds'), st_drop_geometry(roads$A2))
# roads$A3$fitted <- prep_line_grid(readRDS('data/models/a3_full_nocov_log.rds'), st_drop_geometry(roads$A3))
# 
# # Save off spatial files with modeled traffic for direct use
# saveRDS(roads, 'results/predicted_aadt_spatial.rds')
# 
# 
# # Save off inputs for RLINE/AERMET
# 
# latlong_to_utm <- function(xy_frame){
# 	
# 	colnames(xy_frame) <- c('X','Y')
# 	coordinates(xy_frame) <- c('X','Y')
# 	proj4string(xy_frame) <- CRS("+proj=longlat +datum=WGS84")
# 	
# 	zone <- ceiling(as.numeric(xy_frame$X[1])/6 + 30)
# 	utm_frame <- spTransform(xy_frame, CRS(paste0('+proj=utm +zone=',zone,' ellps=WGS84')))
# 	
# 	return(coordinates(utm_frame))
# }
# # add additional columns needed by RLINE/AERMET
# # not done
# roads <- st_drop_geometry(roads)
# roads$dCL <-0
# roads$sigmaz0 <- 2
# roads$'#lanes' <- 4
# roads$Hw1 <- 0
# roads$dw1 <- 0
# roads$Hw2 <- 0
# roads$dw2 <- 0
# roads$Depth <- 0
# roads$Wtop <- 0
# roads$Wbottom <- 0
# roads$Group <- 'G1'
# roads[,c('X_b','Y_b')] <-  latlong_to_utm(xy_beg)
# roads[,c('X_e','Y_e')] <-  latlong_to_utm(xy_end)
# roads$Z_b <- 1
# roads$Z_e <- 1
