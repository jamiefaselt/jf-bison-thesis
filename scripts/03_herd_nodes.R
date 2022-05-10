# herd nodes

library(fasterize)
library(raster)
library(sp)
library(sf)
library(rgeos)
library(rgdal)
library(tidyverse)
library(tigris)
library(dplyr)

r <- raster("data/template_raster.tif")
states <- tigris::states()
mt <- states %>% filter(., NAME=="Montana", drop=TRUE)
counties <- tigris::counties()
mt.counties<-counties %>% filter(STATEFP %in%  c("30"))
mt.counties<-st_transform(mt.counties,st_crs(r))

#bring in the data, match projection and make valid 
mt_reservations <- st_read("data/original/mt_reservations/MontanaReservations.shp") %>% 
  st_transform(.,st_crs(r)) %>% 
  st_make_valid()

#apr shapefiles from APR staff
apr <- st_read("data/original/AP_Property_Boundaries_011022/doc.kml") %>% 
  st_transform(.,st_crs(r)) %>% 
  st_make_valid() 
apr$area <- st_area(apr) %>% 
  as.numeric(.)

#trying to get this centroid to be in the middle of the three locations that have bison... but don't have the names on the dataframe
lg.apr <- apr %>% 
  filter(., area > 20000000)
lg.apr<-lg.apr[!(lg.apr$Name=="73 Ranch"),]
apr <- st_combine(lg.apr) %>% 
  st_as_sf %>% 
  rename(geometry = x) 
apr$NAME <-  seq(1, nrow(apr)) 
apr[apr$NAME==1, "NAME"] <- "American Prairie Reserve"


# Yellowstone
mt_NPS <- st_read("data/original/nps_boundaries/NationalParkServiceAdminBoundaries_Montana.shp") %>% 
  st_transform(.,st_crs(r)) %>% 
  st_make_valid()
yellowstone <- mt_NPS %>% 
  filter(., grepl('Yellowstone National Park',  UNIT_NAME))
yellowstone <- subset(yellowstone, select=c(geometry, UNIT_NAME)) %>%
  rename(NAME = UNIT_NAME)
herds <- bind_rows(mt_reservations, apr, yellowstone) 
plot(st_geometry(herds))


st_write(herds, "data/processed/herd_shapefile_outline.shp", delete_layer=TRUE )

# take the centroid
herds_cent <- st_centroid(herds)
plot(st_geometry(herds_cent))

herds_cent$ID <- seq(1, nrow(herds_cent))
new_herd <- subset(herds_cent, select = c(NAME, geometry, ID))
st_write(new_herd,"data/processed/herd_centroids.shp")

all.nodes <- herds_cent %>% st_buffer(., 10000) # buffer of 10 km         
plot(all.nodes)
node.rast<-fasterize::fasterize(all.nodes, r, field = 'ID')
plot(node.rast)
writeRaster(node.rast, "data/processed/all_nodes.tif", overwrite = TRUE)


