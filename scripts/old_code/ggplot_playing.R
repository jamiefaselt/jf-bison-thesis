library(fasterize)
library(raster)
library(sp)
library(sf)
library(rgeos)
library(rgdal)
library(tidyverse)
library(tigris)
library(dplyr)
library(usmap)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(osmdata)

map("world", "China")
map.cities(country = "China", capitals = 2)
map("state", "Montana")
data(us.cities)
map.cities(us.cities, country="MT")



mt.highways <- getbb("Montana") %>% 
  opq() %>% 
  add_osm_feature(key = "highway", 
                  value = c("primary", "motorway")) %>% 
  osmdata_sf()
View(mt.highways[["osm_lines"]])

river <- getbb("Montana")%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

ggplot() +
  geom_sf(data = mt.highways$osm_lines,
          inherit.aes = FALSE,
          color = "black") +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = .8,
          alpha = .3) 

# load the nodes
mt_reservations <- st_read("data/original/mt_reservations/MontanaReservations.shp")
mt_fws <- st_read("data/original/mt_fws/MT_FWS.shp")
mt_CMR <- mt_fws %>% 
  filter(., ORGNAME=="CHARLES M. RUSSELL NATIONAL WILDLIFE REFUGE",  drop=TRUE)

mt_NPS <- st_read("data/original/nps_boundaries/NationalParkServiceAdminBoundaries_Montana.shp")
mt_cities <- st_read("/Users/jamiefaselt/Research/Data/MT Data/MT_gis/MontanaIncorporatedCitiesTowns_shp/")
roads <- st_read("/Users/jamiefaselt/Research/Data/ShapeFiles/Roads.shp")

# bring in state of montana
states <- tigris::states()
mt <- states %>% filter(., NAME=="Montana", drop=TRUE)
plot(mt)

# bring in counties of montana
counties <- map_data("county")
mt_counties <- subset(counties, region == "montana")

# match crs to template raster
r <- raster("data/template_raster.tif")
cmr <- st_transform(mt_CMR, crs(r))
nps <- st_transform(mt_NPS, crs(r))
reservations <- st_transform(mt_reservations, crs(r))
cities <- st_transform(mt_cities, crs(r))
roads <- st_transform(roads, crs(r))

#trying to add terrain
elevation <- raster("Data/MT Data/DEM_1000m_1992/DEM_1000m_1992.tif")
plot(elevation)

#plot
ggplot() +
  geom_sf(data=cmr, color="black", fill="darkgrey") +
  geom_sf(data=nps, color="black", fill="lightblue") +
  geom_sf(data=reservations, color="black", fill="yellow") +
  geom_sf(data=mt, color="black", fill="transparent") +
  geom_sf(data = mt.highways$osm_lines,
          inherit.aes = FALSE,
          color = "black") +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = .8,
          alpha = .3) +
  geom_raster(data = biophys.edit.cs,
              aes(x = x,
                  y = y,
                  fill = layer)) +
  scale_fill_continuous() +
  coord_sf(xlim = c(-1401787, -556147.3), 
           ylim = c(515947, 1208767),
           expand = FALSE)
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Study Area: Bison Herds in Montana")


ggplot()+
  geom_raster(data = biophys.edit.cs, 
            mapping = aes(x = x, y = y), size = 0.25, fill = NA, color = alpha("gray", 0.25)) 
plot(biophys.edit.cs)  
    