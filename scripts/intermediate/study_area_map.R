library(raster)
library(tidyverse)
library(sf)
library(ggplot2)
# template raster
cattle <- raster("data/raster_layers/cattle_sales_layer.tif")
r <- raster("data/template_raster.tif") %>% 
  mask(., cattle)


# make the template raster an sf to crop counties to
r_sf <- stars::st_as_stars(r) %>% 
  st_as_sf(.)

counties <- tigris::counties()
counties<-counties %>% filter(STATEFP %in%  c("30", "56"))
counties<-st_transform(counties,st_crs(r)) %>% 
  st_crop(., r_sf) %>% 
  st_simplify(.)
st_crs(counties)
st_write(counties, "data/processed/county_shapefile.shp")
extent(counties)

# Custom map theme --------------------------------------------------------
theme_map <- function(...) {
  theme_minimal() +
    theme(
      #text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_line(color = "black", size=0.02),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "white", size = 0.002),
      panel.grid.minor = element_line(color = "white", size = 0.002),
      plot.background = element_rect(fill = "white", color = NA), 
      panel.background = element_rect(fill = "white", color = NA), 
      legend.background = element_rect(fill = "white", color = NA),
      #panel.border = element_rect(fill=NA, color = "black"),
      ...
    )
}
# Load Elev data and calc hillshade --------------------------------------- 
#not necessary, purely aesthetic
elev <- getData('alt', country = 'USA')
elev.proj <- projectRaster(elev[[1]], r)


elev.crop <- mask(elev.proj, r)
elev.mod <- elev.crop *10
slope <- terrain(elev.mod, opt='slope')
aspect <- terrain(elev.mod, opt='aspect')
hill = hillShade(slope, aspect, 40, 270)
hill2 <- aggregate(hill , fact = 5 , method = "bilinear" )
hills3 <- focal(hill2, w=matrix(1/9, nc=3, nr=3), mean)
writeRaster(hills3, "data/processed/hillshade.tif", overwrite = TRUE)

# Get vectors for maps ----------------------------------------------------
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
plot(st_geometry(apr))


# Yellowstone
mt_NPS <- st_read("data/original/nps_boundaries/NationalParkServiceAdminBoundaries_Montana.shp") %>% 
  st_transform(.,st_crs(r)) %>% 
  st_make_valid()
yellowstone <- mt_NPS %>% 
  filter(., grepl('Yellowstone National Park',  UNIT_NAME))
yellowstone <- subset(yellowstone, select=c(geometry, UNIT_NAME)) %>%
  rename(NAME = UNIT_NAME)

# fuck
herds <- bind_rows(mt_reservations, apr, yellowstone) 
plot(st_geometry(herds))
st_write(herds, "data/processed/herd_shapefile.shp")


sts <- tigris::states() %>% 
  dplyr::filter(., STUSPS %in% c("MT", "WY")) %>% 
  st_transform(., crs(r)) %>% 
  as(., "Spatial")

conus <-  tigris::states() %>%
  dplyr::filter(,, !STUSPS %in% c("AK","AS", "HI", "GU", "VI", "DC", "PR", "MP")) %>% 
  st_transform(., crs(r))

sts.crop <- crop(sts, r)

# just the centroids
pa.cents <- st_read("data/processed/all_nodes_correct.shp") %>% 
  as(., "sf") %>% 
  st_centroid(.)
pa.cents[is.na(pa.cents)] = "American Prairie Reserve"
pa.cents$lab <- c("Blackfeet", "Rocky Boy's" , "Fort Belknap", "Fort Peck", "Northern Cheyenne", "Crow", "Flathead", "American Prairie Reserve", "Yellowstone")

pa.cents <- pa.cents

library(tidyverse)

pa.cents <- pa.cents  %>%
  cbind(pa.cents, st_coordinates(pa.cents))

# want to get a point for Helina, Missoula and Bozeman?
cities <- st_read("data/original/MontanaIncorporatedCitiesTowns_shp/MontanaIncorporatedCitiesTowns.shp") %>% 
  st_transform(.,st_crs(r)) %>% 
  st_make_valid(.)%>% 
  filter(., grepl('Helena',  NAME)) %>% 
  subset(., NAME!='East Helena') %>% 
  st_centroid(.) %>% 
  st_buffer(., 10000) 

helena <- cities %>% 
  st_centroid(.)%>% 
  cbind(., st_coordinates(.))

# make the map
p <- RStoolbox::ggR(hills3, alpha = .7) + 
  geom_sf(data = counties, fill = NA) +
  geom_sf(data = yellowstone, fill = "forestgreen") +
  geom_sf(data = apr, fill = "yellow", color = "yellow") + 
  geom_sf(data = mt_reservations, fill = "orange") + 
  geom_sf(data = pa.cents, fill  = NA) + 
  geom_sf(data = cities, fill = "red", shape ="star") + 
  theme_map() + 
  theme(panel.background = element_rect(fill = "white", color = "black"),
        plot.background = element_rect(fill = NA, color = NA))
p
p1 <- p + geom_label_repel(data = pa.cents, aes(X, Y, label = lab), color = "black", fontface="bold", size = 3) + geom_label_repel(data = helena, aes(X, Y, label = NAME), color = "black", size = 2)

p1
p3 <- ggplot()+
  geom_sf(data=conus, fill="white") +
  geom_sf(data =st_as_sfc(st_bbox(r)), fill=NA, color="red") +
  theme_map() +
  theme(panel.background = element_rect(fill = "gray", color = "black"),
        plot.background = element_rect(fill = NA, color = NA))

library(cowplot)
full <- ggdraw(p1)  +
  draw_plot(p3, x = 0.72, y = 0,  
            width = 0.3, height = 0.2)

ggsave(here::here("plots/study_area.png"), plot =full)

