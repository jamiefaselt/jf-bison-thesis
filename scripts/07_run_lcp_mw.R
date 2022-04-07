library(raster)
library(gdistance)
library(yenpathy)
library(leastcostpath)
library(rgdal)
library(sf)
library(magrittr)

# Load top paths function -------------------------------------------------


source(here::here("functions/06_fun_K_lcp.R"))



# Prep resistance surfaces for gdist  ---------------------------------------------
biophys.resist <- raster("data/raster_layers/biophys_resistance_layer.tif")
implementation.resist1 <- raster("data/raster_layers/social_resistance_layer.tif")
implementation.resist1[is.na(implementation.resist1[])] <- 5* cellStats(implementation.resist1, max)## drop NAs for costodistance


origins <- st_read("data/processed/all_nodes_correct.shp" )%>% 
  dplyr::filter(., NAME == "BLACKFEET" | NAME == "FORT PECK") %>%
  st_transform(. , crs(biophys.resist))


pa.rast <- rasterize(as(origins, "Spatial"), biophys.resist, field="ID")


# Create Transition Matrix ------------------------------------------------
#need to convert resistance to conductance
biophys.tr <- transition(1/biophys.resist, transitionFunction = mean, 16)
biophys.tr <- geoCorrection(biophys.tr, "c")
saveRDS(biophys.tr, here::here('data/processed/TransitionLayers/biophystrans.rds'))

social.tr1 <- transition(1/implementation.resist1, transitionFunction = mean, 16)
social.tr1 <- geoCorrection(social.tr1, "c")
saveRDS(social.tr1, here::here('data/processed/TransitionLayers/socialtrans1.rds'))

# can use the same code above to run for interventio scenarios in the future
#social.tr2 <- transition(1/implementation.resist2, transitionFunction = mean, 16)
#social.tr2 <- geoCorrection(social.tr2, "c")
#saveRDS(social.tr2, here::here('Data/ProcessedData/TransitionLayers/socialtrans_jurisdiction.rds'))

#social.tr3 <- transition(1/implementation.resist3, transitionFunction = mean, 16)
#social.tr3 <- geoCorrection(social.tr3, "c")
#saveRDS(social.tr3, here::here('Data/ProcessedData/TransitionLayers/socialtrans_cattle.rds'))

# Estimate k low cost paths -----------------------------------------------

origins <- st_read(here::here("data/processed/all_nodes_correct.shp")) %>% 
  st_centroid(.) %>% 
  dplyr::filter(., NAME == "BLACKFEET") %>% 
  as(. , "Spatial")
goals <- st_read(here::here("data/processed/all_nodes_correct.shp")) %>% 
  st_centroid(.) %>% 
  dplyr::filter(., NAME == "FORT PECK") %>% 
  as(. , "Spatial")
origin.proj <- spTransform(origins, crs(biophys.resist))
goals.proj <- spTransform(goals, crs(biophys.resist))

social1 <- gen_top_paths(tr = social.tr1, resist = implementation.resist1, numpath = 5, bufdist = 4000, orig = origin.proj, goal=goals.proj)

#social2 <- gen_top_paths(tr = social.tr2, resist = implementation.resist2, numpath = 5, bufdist = 4000, orig = origin.proj, goal=goals.proj)

biophys <- gen_top_paths(tr = biophys.tr, resist = biophys.resist, numpath = 5, bufdist = 4000, orig = origin.proj, goal=goals.proj)

saveRDS(social1, here::here('data/processed/TransitionLayers/socialtop5.rds'))
saveRDS(biophys, here::here('data/processed/TransitionLayers/biophystop5.rds'))
