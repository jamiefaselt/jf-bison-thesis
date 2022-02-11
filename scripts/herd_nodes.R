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

r <- raster("Data/temp_rstr.tif")
hsi <- raster("Data/hsi_crop_540.tif")
st_crs(hsi)==st_crs(r)

#bring in the data, match projection and make valid 
mt_reservations <- st_read("/Users/jamiefaselt/Research/Data/MontanaReservations_shp 3/MontanaReservations.shp") %>% 
  st_transform(.,st_crs(r)) %>% 
  st_make_valid()

mt_fws <- st_read("/Users/jamiefaselt/Research/Data//MT Data/MT_FWS.shp") %>% 
  st_transform(.,st_crs(r))

mt_CMR <- mt_fws %>% 
  filter(., ORGNAME=="CHARLES M. RUSSELL NATIONAL WILDLIFE REFUGE",  drop=TRUE) %>% 
  st_transform(.,st_crs(r)) %>% 
  filter(., SUM_GISACR > 530517) %>% 
  st_make_valid()
plot(mt_CMR)

mt_NPS <- st_read("/Users/jamiefaselt/Research/Data/NationalParkServiceAdminBoundaries_shp 2") %>% 
  st_transform(.,st_crs(r)) %>% 
  st_make_valid()

yellowstone <- mt_NPS %>% 
  filter(., grepl('Yellowstone National Park',  UNIT_NAME))

# take the centroids
rez.nodes <- st_centroid(mt_reservations)
cmr.nodes <- st_centroid(mt_CMR)
nps.nodes <- st_centroid(yellowstone)

#combine centroids into one shapefile
reznode <- subset(rez.nodes, select=c(geometry, NAME))
cmrnode <- subset(cmr.nodes, select=c(geometry, ORGNAME)) %>% 
  rename(NAME = ORGNAME)
npsnode <- subset(nps.nodes, select=c(geometry, UNIT_NAME)) %>%
  rename(NAME = UNIT_NAME)

all.nodes <- bind_rows(reznode, cmrnode, npsnode)
plot(all.nodes)

all.nodes$ID <- seq(1, nrow(all.nodes))
all.nodes <- all.nodes %>% st_buffer(., 10000) # buffer of 10 km         
plot(all.nodes)
st_write(all.nodes, "Data/all_nodes_correct.shp", overwite = TRUE)
node.rast<-fasterize::fasterize(all.nodes, r, field = 'ID')
plot(node.rast)
writeRaster(node.rast, "Data/node.rast.tif", overwrite = TRUE)
