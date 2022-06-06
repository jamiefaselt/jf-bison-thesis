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
  st_make_valid() %>% st_combine() %>% st_as_sf() %>% rename(geometry = x) 
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

herds <- subset(herds, select = c(NAME, geometry))


st_write(herds, "data/processed/herd_shapefile_outline.shp", append = FALSE)

# take the centroid
herds_cent <- st_centroid(herds)
plot(st_geometry(herds_cent), col = "red", add = TRUE)

herds_cent$ID <- seq(1, nrow(herds_cent))
new_herd <- subset(herds_cent, select = c(NAME, geometry, ID))
st_write(new_herd,"data/processed/herd_centroids.shp", append = FALSE)

all.nodes <- herds_cent %>% st_buffer(., 500) # buffered to have something to rasterize anything below 500 was not giving me any values in the raster 
plot(all.nodes)
node.rast<-fasterize::fasterize(all.nodes, r, field = 'ID')
plot(node.rast)
writeRaster(node.rast, "data/processed/all_nodes.tif", overwrite = TRUE)

# by end of this script have: shapefile of herd outlines, shapefile of herd centroids, raster of herd centroids


