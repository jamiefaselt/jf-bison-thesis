library(raster)
library(rasterVis)
library(ggplot2)
library(tidyverse)
library(cmocean)
library(viridis)
library(sf)
library(patchwork)
rescale01 <- function(r1) {
  r.rescale <- (r1 - cellStats(r1, min))/(cellStats(r1, max) - cellStats(r1, min))
}
r <- raster("data/template_raster.tif")

# load inital cs layers ---------------------------------------------------
implement.cs <- raster(here::here('circuitscape_outputs/social_cs/social_out_cum_curmap.asc')) %>% 
  mask(., r)
biophys.cs <- raster(here::here('circuitscape_outputs/biophys_cs/biophys_out_cum_curmap.asc')) %>% 
  mask(., r)
econ.cs <- raster(here::here('circuitscape_outputs/econ_cs/econ_scenario_out_cum_curmap.asc'))%>% 
  mask(., r)
gov.cs <- raster(here::here("circuitscape_outputs/gov_cs/tribal_scenario_out_cum_curmap.asc"))%>% 
                   mask(., r)
new.node.cs <- raster(here::here("circuitscape_outputs/newherd_shortcircuit_cs/newnode_out_cum_curmap.asc"))%>% 
  mask(., r)
implement.min <- raster(here::here("circuitscape_outputs/null_cs/null_out_cum_curmap.asc"))%>% 
  mask(., r)
# normalize ---------------------------------------------------------------

implement.norm <- implement.cs/implement.min
econ.norm <- econ.cs/implement.min
gov.norm <- gov.cs/implement.min
newnode.norm <- new.node.cs/implement.min

# Convert to df -----------------------------------------------------------

implement.norm.df <- implement.norm %>%
  projectRaster(., res=300, crs = crs(implement.cs)) %>%
  rasterToPoints %>%
  as.data.frame() %>%
  `colnames<-`(c("x", "y", "implement")) %>% 
  mutate(., channel = ifelse(implement > 1.2, 1, 
                             ifelse(2 > implement & implement >= 0.8, NA, 0)))

econ.norm.df <- econ.norm %>%
  projectRaster(., res=300, crs = crs(implement.cs)) %>%
  rasterToPoints %>%
  as.data.frame() %>%
  `colnames<-`(c("x", "y", "econ")) %>% 
  mutate(., channel = ifelse(econ > 1.2, 1, 
                             ifelse(2 > econ & econ >= 0.8, NA, 0)))

gov.norm.df <- gov.norm %>%
  projectRaster(., res=300, crs = crs(implement.cs)) %>%
  rasterToPoints %>%
  as.data.frame() %>%
  `colnames<-`(c("x", "y", "gov")) %>% 
  mutate(., channel = ifelse(gov > 1.2, 1, 
                             ifelse(2 > gov & gov >= 0.8, NA, 0)))

newnode.norm.df <- newnode.norm %>%
  projectRaster(., res=300, crs = crs(implement.cs)) %>%
  rasterToPoints %>%
  as.data.frame() %>%
  `colnames<-`(c("x", "y", "newnode")) %>% 
  mutate(., channel = ifelse(newnode > 1.2, 1, 
                             ifelse(2 > newnode & newnode >= 0.8, NA, 0)))

# Add addtional map eleements ---------------------------------------------

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
elev <- getData('alt', country = 'USA')
elev.proj <- projectRaster(elev[[1]], implement.cs) %>% 
  mask(r)


elev.crop <- crop(elev.proj, implement.cs)
elev.mod <- elev.crop *10
slope <- terrain(elev.mod, opt='slope')
aspect <- terrain(elev.mod, opt='aspect')
hill = hillShade(slope, aspect, 40, 270)
hill2 <- aggregate(hill , fact = 5 , method = "bilinear" )
hills3 <- focal(hill2, w=matrix(1/9, nc=3, nr=3), mean)

# Load centroids ----------------------------------------------------------
origins <- st_read(here::here("data/processed/herd_centroids.shp")) %>% 
  st_centroid(.) %>% 
  # dplyr::filter(., Unit_Nm == "Weminuche Wilderness") %>% 
  as(. , "Spatial")
goals <- st_read(here::here("data/processed/herd_centroids.shp")) %>% 
  st_centroid(.) %>% 
  # dplyr::filter(., Unit_Nm == "Yellowstone National Park") %>% 
  as(. , "Spatial")
origin.proj <- spTransform(origins, crs(biophys.cs))
goals.proj <- spTransform(goals, crs(biophys.cs))
# Get vectors for maps ----------------------------------------------------

PAs <- st_read(here::here("data/processed/herd_shapefile_outline.shp")) #%>% 
# dplyr::filter(. , Unit_Nm == "Yellowstone National Park" | Unit_Nm == "Weminuche Wilderness") %>% 
#  st_transform(. , crs = crs(biophys.cs))

sts <- tigris::states() %>% 
  dplyr::filter(., STUSPS %in% c("ID", "MT", "UT", "WY", "CO", "AZ", "NM")) %>% 
  st_transform(., crs(biophys.cs)) %>% 
  as(., "Spatial")

conus <-  tigris::states() %>%
  dplyr::filter(,, !STUSPS %in% c("AK","AS", "HI", "GU", "VI", "DC", "PR", "MP")) %>% 
  st_transform(., crs(biophys.cs))


sts.crop <- crop(sts, biophys.cs)

pa.cents <- as(goals.proj, "sf")
pa.cents$lab <- c("Blackfeet", "Rocky Boy's", "Fort Belknap",
                  "Fort Peck","Northern Cheyenne","Crow",
                  "Flathead","American Prairie Reserve",
                  "Yellowstone")

# Make the plots ----------------------------------------------------------

p1 <- RStoolbox::ggR(hills3) +
  geom_raster(
    data = implement.norm.df[!is.na(implement.norm.df$channel),],
    aes(
      x=x,
      y=y,
      fill = factor(channel)
    ),
    interpolate = TRUE,
  ) +
  scale_fill_viridis(labels = c("Impeded", "Channelized"), discrete = TRUE, begin = 0.33, end = 0.8,option = "D") +
  geom_sf(data = PAs, fill = "forestgreen") +
  geom_sf(data = as(sts.crop, "sf"), fill = NA, color="black")+
  ggrepel::geom_text_repel(data = pa.cents, aes(x = st_coordinates(pa.cents)[,1], y = st_coordinates(pa.cents)[,2], label=lab), 
                           box.padding = 1, max.overlaps = Inf,fontface="bold", color = "black", size=4.5)+
  guides(fill = guide_legend(title.position = "top", 
                             label.position="bottom", title = NULL)) +
  theme_map() +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.justification = "center"  )

p2 <- RStoolbox::ggR(hills3) +
  geom_raster(
    data = econ.norm.df[!is.na(econ.norm.df$channel),],
    aes(
      x=x,
      y=y,
      fill = factor(channel)
    ),
    interpolate = TRUE,
  ) +
  scale_fill_viridis(labels = c("Impeded", "Channelized"), discrete = TRUE, begin = 0.33, end = 0.8,option = "D") +
  geom_sf(data = PAs, fill = "forestgreen") +
  geom_sf(data = as(sts.crop, "sf"), fill = NA, color="black")+
  ggrepel::geom_text_repel(data = pa.cents, aes(x = st_coordinates(pa.cents)[,1], y = st_coordinates(pa.cents)[,2], label=lab), 
                           box.padding = 1, max.overlaps = Inf,fontface="bold", color = "black", size=4.5)+
  guides(fill = guide_legend(title.position = "top", 
                             label.position="bottom", title = NULL)) +
  theme_map() +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.justification = "center"  )

p3 <- RStoolbox::ggR(hills3) +
  geom_raster(
    data = gov.norm.df[!is.na(gov.norm.df$channel),],
    aes(
      x=x,
      y=y,
      fill = factor(channel)
    ),
    interpolate = TRUE,
  ) +
  scale_fill_viridis(labels = c("Impeded", "Channelized"), discrete = TRUE, begin = 0.33, end = 0.8,option = "D") +
  geom_sf(data = PAs, fill = "forestgreen") +
  geom_sf(data = as(sts.crop, "sf"), fill = NA, color="black")+
  ggrepel::geom_text_repel(data = pa.cents, aes(x = st_coordinates(pa.cents)[,1], y = st_coordinates(pa.cents)[,2], label=lab), 
                           box.padding = 1, max.overlaps = Inf,fontface="bold", color = "black", size=4.5)+
  guides(fill = guide_legend(title.position = "top", 
                             label.position="bottom", title = NULL)) +
  theme_map() +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.justification = "center"  )

p4 <- RStoolbox::ggR(hills3) +
  geom_raster(
    data = newnode.norm.df[!is.na(newnode.norm.df$channel),],
    aes(
      x=x,
      y=y,
      fill = factor(channel)
    ),
    interpolate = TRUE,
  ) +
  scale_fill_viridis(labels = c("Impeded", "Channelized"), discrete = TRUE, begin = 0.33, end = 0.8,option = "D") +
  geom_sf(data = PAs, fill = "forestgreen") +
  geom_sf(data = as(sts.crop, "sf"), fill = NA, color="black")+
  ggrepel::geom_text_repel(data = pa.cents, aes(x = st_coordinates(pa.cents)[,1], y = st_coordinates(pa.cents)[,2], label=lab), 
                           box.padding = 1, max.overlaps = Inf,fontface="bold", color = "black", size=4.5)+
  guides(fill = guide_legend(title.position = "top", 
                             label.position="bottom", title = NULL)) +
  theme_map() +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.justification = "center"  )

combined.plot <- p1+p2+p3+p4+ plot_layout(guides = "collect") +  plot_annotation(tag_levels = "A", tag_suffix = ")") & theme(legend.position = "bottom", legend.justification = "center",
                                                                                                                           legend.margin=margin(0,0,0,0),
                                                                                                                           legend.box.margin=margin(-15,-10,-10,-10)) 
ggsave(here::here("plots/channelization.png"), plot =combined.plot)
