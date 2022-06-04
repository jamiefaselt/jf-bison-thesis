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
reservations <- st_read("data/original/mt_reservations/MontanaReservations.shp") %>% 
  st_transform(crs=st_crs(hsi)) %>% 
  st_make_valid(.)
apr <- st_read("data/original/AP_Property_Boundaries_011022/doc.kml") %>% 
  st_transform(crs=st_crs(hsi)) %>% 
  st_make_valid(.)
mt_NPS <- st_read("data/original/nps_boundaries/NationalParkServiceAdminBoundaries_Montana.shp")
yellowstone <- mt_NPS %>% 
  filter(., UNIT_NAME=="Yellowstone National Park",  drop=TRUE) %>% 
  st_transform(crs=st_crs(hsi)) %>% 
  st_make_valid(.)

# make sure all the projections are the same
st_crs(reservations)==st_crs(hsi)
st_crs(apr)==st_crs(hsi)
st_crs(yellowstone)==st_crs(hsi)
mtwy <- mtwy %>% st_transform(crs=st_crs(hsi))
mt <- mt %>% st_transform(crs=st_crs(hsi))

#combine them into one shapefile
yellowstone <- yellowstone %>%
  rename(NAME = UNIT_NAME)
apr <- apr %>% 
  rename(NAME = Name)

all.nodes <- bind_rows(reservations, apr, yellowstone)
class(all.nodes)
plot(st_geometry(all.nodes)) # looks good

#create a bounding box and buffer 50km to the south to include area around the YNP herd
ext <- st_bbox(all.nodes)
poly <- st_as_sfc(st_bbox(c(xmin = st_bbox(mt)[[1]], xmax = st_bbox(mt)[[3]], ymax = st_bbox(mt)[[4]], ymin = st_bbox(all.nodes)[[2]]-50000), crs = st_crs(hsi)))
r <- raster(crs= proj4string(as(poly, "Spatial")), ext=raster::extent(as(poly, "Spatial")), resolution= 540)
extent(r)
extent(st_bbox(all.nodes))

# add values
values(r) <- 1:ncell(r)

# save the template raster
writeRaster(r, "data/template_raster.tif", overwrite = TRUE)


# by the end of this script have a template raster matched to my largest raster (the habitat suitability layer or hsi) that is cropped to a bounding box of montana and 50 km south of the yellowstone herd
