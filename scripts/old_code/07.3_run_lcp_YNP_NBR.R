library(raster)
library(gdistance)
library(yenpathy)
library(rgdal)
library(sf)
library(magrittr)

# Load top paths function -------------------------------------------------


source(here::here("scripts/06_fun_K_lcp.R"))




# Load Data---------------------------------------------

biophys.resist <- raster("data/raster_layers/biophys_resistance_layer.tif")
biophys.resist[is.na(biophys.resist[])] <- 2* cellStats(biophys.resist, max)## drop NAs for costodistance
implementation.resist1 <- raster("data/raster_layers/social_resistance_layer.tif")
implementation.resist1[is.na(implementation.resist1[])] <- 5* cellStats(implementation.resist1, max)## drop NAs for costodistance

# Prep resistance surfaces for gdist  ---------------------------------------------

origins <- st_read(here::here("data/processed/all_nodes_correct.shp")) %>% 
  dplyr::filter(., NAME == "Yellowstone National Park" | NAME == "FLATHEAD") %>% 
  st_centroid(.) %>% 
  st_transform(. , crs(biophys.resist))

origins$ID <- c(1,2)

pa.rast <- rasterize(as(origins, "Spatial"), biophys.resist, field="ID")
#writeRaster(pa.rast, here::here("data/Processed/pa_locs_bf_NBR.tif"), overwrite=TRUE)


# Create Transition Matrix ------------------------------------------------
#need to convert resistance to conductance
#biophys.tr <- transition(1/biophys.resist, transitionFunction = mean, 16)
#biophys.tr <- geoCorrection(biophys.tr, "c")
#saveRDS(biophys.tr, here::here('data/Processed/TransitionLayers/biophystrans.rds'))
biophys.tr <- readRDS("data/processed/TransitionLayers/biophystrans.rds")
#social.tr1 <- transition(1/implementation.resist1, transitionFunction = mean, 16)
#social.tr1 <- geoCorrection(social.tr1, "c")
#saveRDS(social.tr1, here::here('data/Processed/TransitionLayers/socialtrans1.rds'))
social.tr1 <- readRDS("data/processed/TransitionLayers/socialtrans1.rds")


# Estimate k low cost paths -----------------------------------------------

origins <- st_read(here::here("data/processed/all_nodes_correct.shp")) %>% 
  dplyr::filter(., NAME == "Yellowstone National Park") %>% 
  st_centroid(.) %>% 
  as(. , "Spatial")
goals <- st_read(here::here("data/processed/all_nodes_correct.shp")) %>% 
  dplyr::filter(., NAME == "FLATHEAD") %>% 
  st_centroid(.) %>% 
  as(. , "Spatial")
origin.proj <- spTransform(origins, crs(biophys.resist))
goals.proj <- spTransform(goals, crs(biophys.resist))

social1 <- gen_top_paths(tr = social.tr1, resist = implementation.resist1, numpath = 5, bufdist = 4000, orig = origin.proj, goal=goals.proj)

biophys <- gen_top_paths(tr = biophys.tr, resist = biophys.resist, numpath = 5, bufdist = 4000, orig = origin.proj, goal=goals.proj)

saveRDS(social1, here::here('data/Processed/TransitionLayers/socialtop5_YNP_NBR.rds'))
saveRDS(biophys, here::here('data/Processed/TransitionLayers/biophystop5_YNP_NBR.rds'))
