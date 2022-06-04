# new nodes

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
apr <- st_read("/Users/jamiefaselt/Downloads/AP_Property_Boundaries_011022/doc.kml") %>% 
  st_transform(.,st_crs(r)) %>% 
  st_make_valid() 
apr$area <- st_area(apr) %>% 
  as.numeric(.)

#trying to get this centroid to be in the middle of the three locations that have bison... but don't have the names on the dataframe
lg.apr <- apr %>% 
  filter(., area > 20000000)
lg.apr<-lg.apr[!(lg.apr$Name=="73 Ranch"),]

# OLD
#mt_fws <- st_read("data/original/mt_fws/MT_FWS.shp") %>% 
# st_transform(.,st_crs(r))
#mt_CMR <- mt_fws %>% 
#filter(., ORGNAME=="CHARLES M. RUSSELL NATIONAL WILDLIFE REFUGE",  drop=TRUE) %>% 
#st_transform(.,st_crs(r)) %>% 
#filter(., SUM_GISACR > 530517) %>% 
#st_make_valid()

# Yellowstone
mt_NPS <- st_read("data/original/nps_boundaries/NationalParkServiceAdminBoundaries_Montana.shp") %>% 
  st_transform(.,st_crs(r)) %>% 
  st_make_valid()
yellowstone <- mt_NPS %>% 
  filter(., grepl('Yellowstone National Park',  UNIT_NAME))

mt.padus <- st_read("data/processed/new_pa_herds.shp")%>% 
  st_transform(.,st_crs(r)) %>% 
  st_make_valid()  


#subset and change names for mering
rez <- subset(mt_reservations, select=c(geometry, NAME))
lg.apr <- subset(lg.apr, select=c(geometry, Name)) %>%
  rename(NAME = Name)
yellowstone <- subset(yellowstone, select=c(geometry, UNIT_NAME)) %>%
  rename(NAME = UNIT_NAME)
new <- subset(mt.padus, select=c(geometry, Unit_Nm)) %>%
  rename(NAME = Unit_Nm)
herds_shapefiles <- bind_rows(rez, lg.apr, yellowstone, new)
plot(st_geometry(herds_shapefiles))
st_write(herds_shapefiles, "data/processed/new_herd_shapefiles.shp", delete_layer = TRUE)
#st_write(herds_shapefiles, "data/processed/herd_shapefile_outline.shp", delete_layer=TRUE )

# take the centroids
rez.nodes <- st_centroid(rez)
nps.nodes <- st_centroid(yellowstone)
pa.nodes <- st_centroid(new)
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
all.nodes <- bind_rows(rez.nodes, apr.nodes, nps.nodes, pa.nodes) %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])
plot(all.nodes)
plot(st_geometry(all.nodes))
plot(st_geometry(herds_shapefiles), add = TRUE)

composite <- ggplot()+
  geom_sf(data=all.nodes, color = "orange")
composite + geom_text_repel(data = all.nodes, aes(lon, lat, label = NAME), size = 3)

all.nodes$ID <- seq(1, nrow(all.nodes))
all.nodes <- all.nodes %>% st_buffer(., 10000) # buffer of 10 km         
plot(all.nodes)
st_write(all.nodes, "data/processed/new_nodes.shp", delete_layer = TRUE)
#st_write(all.nodes, "data/processed/all_nodes_correct.shp", delete_layer=TRUE )
node.rast<-fasterize::fasterize(all.nodes, r, field = 'ID')
plot(node.rast)
writeRaster(node.rast, "data/processed/new_nodes.tif", overwrite = TRUE)



