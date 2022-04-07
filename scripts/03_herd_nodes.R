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

# Yellowstone
mt_NPS <- st_read("data/original/nps_boundaries/NationalParkServiceAdminBoundaries_Montana.shp") %>% 
  st_transform(.,st_crs(r)) %>% 
  st_make_valid()
yellowstone <- mt_NPS %>% 
  filter(., grepl('Yellowstone National Park',  UNIT_NAME))

#subset and change names for mering
rez <- subset(mt_reservations, select=c(geometry, NAME))
lg.apr <- subset(lg.apr, select=c(geometry, Name)) %>%
  rename(NAME = Name)
yellowstone <- subset(yellowstone, select=c(geometry, UNIT_NAME)) %>%
  rename(NAME = UNIT_NAME)
herds_shapefiles <- bind_rows(rez, lg.apr, yellowstone)
plot(st_geometry(herds_shapefiles))
st_write(herds_shapefiles, "data/processed/herd_shapefile_outline.shp", delete_layer=TRUE )

# take the centroids
rez.nodes <- st_centroid(rez)
nps.nodes <- st_centroid(yellowstone)
# need to do some work to get this to have the right column names to combine them into one shapefile
apr <- st_combine(lg.apr)
apr.nodes <- st_centroid(apr) %>% 
  st_as_sf %>% 
  rename(geometry = x) 
apr.nodes$NAME <-  seq(1, nrow(apr.nodes)) %>% 
  as.character(.) %>% 
  rename(apr.nodes$NAME==1)
apr.nodes[apr.nodes$NAME==1, "NAME"] <- "American Prairie Reserve"

#combine centroids into one shapefile
all.nodes <- bind_rows(rez.nodes, apr.nodes, nps.nodes) 
all.nodes[8,1] <- "American Prairie Reserve"
plot(all.nodes)
plot(st_geometry(all.nodes))
plot(st_geometry(herds_shapefiles), add = TRUE)

all.nodes$ID <- seq(1, nrow(all.nodes))
all.nodes <- all.nodes %>% st_buffer(., 10000) # buffer of 10 km         
plot(all.nodes)
st_write(all.nodes, "data/processed/all_nodes_correct.shp", delete_layer=TRUE )
node.rast<-fasterize::fasterize(all.nodes, r, field = 'ID')
plot(node.rast)
writeRaster(node.rast, "data/processed/all_nodes.tif", overwrite = TRUE)

