# --------------------------------
# Description: Process Street Data (one off)
# Date: Dec 6 2023
#
# Logan Piepmeier
# --------------------------------

library(sf)
library(tidyverse)
map(
	list.files('data/street', pattern = 'shp$', full.names = TRUE),
	function(x) read_sf(x) %>% 
		transmute(OBJECTID, class = substr(CFCC, 1, 2), state = STATE00_R, county = CTY00_R, label = LABEL) %>%
		filter(class %in% paste0('A', 1:3))
) %>% 
	bind_rows() %>%
	write_sf('data/us_roads.sqlite')
