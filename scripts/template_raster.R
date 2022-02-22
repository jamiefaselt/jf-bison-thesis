# making my template raster
library(fasterize)
library(raster)
library(sp)
library(sf)
library(rgeos)
library(rgdal)
library(tidyverse)
library(tigris)
library(dplyr)

#bring in shapefile of mt and wy
states <- c( "MT", "WY" )
mtwy <- tigris::states(cb=TRUE) %>%
  filter(STUSPS %in% states)
states <- tigris::states()
mt <- states %>% filter(., NAME=="Montana", drop=TRUE)

#bring in hsi layer from Brent to set crs
hsi <- raster("data/original/SUMMER_HSI_clip/SUMMER_HSI_clip.tif")
hsi

# load the herd locations
mt_reservations <- st_read("data/original/mt_reservations/MontanaReservations.shp")
mt_fws <- st_read("data/original/mt_fws/MT_FWS.shp")
mt_CMR <- mt_fws %>% 
  filter(., ORGNAME=="CHARLES M. RUSSELL NATIONAL WILDLIFE REFUGE",  drop=TRUE)
mt_NPS <- st_read("data/original/nps_boundaries/NationalParkServiceAdminBoundaries_Montana.shp")
yellowstone <- mt_NPS %>% 
  filter(., UNIT_NAME=="Yellowstone National Park",  drop=TRUE)

# make sure all the projections are the same
reservations <- mt_reservations %>% st_transform(crs=st_crs(hsi)) %>% 
  st_make_valid()
cmr <- mt_CMR %>% st_transform(crs=st_crs(hsi)) %>% 
  st_make_valid()
yellowstone <- yellowstone %>% st_transform(crs=st_crs(hsi)) %>% 
  st_make_valid()
mtwy <- mtwy %>% st_transform(crs=st_crs(hsi))
mt <- mt %>% st_transform(crs=st_crs(hsi))

#combine them into one shapefile
yellowstone <- yellowstone %>%
  rename(NAME = UNIT_NAME)
cmr <- cmr %>% 
  rename(NAME = ORGNAME)

all.nodes <- bind_rows(reservations, cmr, yellowstone)
class(all.nodes)
plot(st_geometry(all.nodes)) # looks good

#create a bounding box and buffer 50km to the south
ext <- st_bbox(all.nodes)
poly <- st_as_sfc(st_bbox(c(xmin = st_bbox(mt)[[1]], xmax = st_bbox(mt)[[3]], ymax = st_bbox(mt)[[4]], ymin = st_bbox(all.nodes)[[2]]-50000), crs = st_crs(hsi)))
r <- raster(crs= proj4string(as(poly, "Spatial")), ext=raster::extent(as(poly, "Spatial")), resolution= 540)
extent(r)
extent(st_bbox(all.nodes))

# save the template raster
writeRaster(r, "data/template_raster.tif", overwrite = TRUE)

