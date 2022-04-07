library(raster)
library(gdistance)
library(yenpathy)
library(leastcostpath)
library(rgdal)
library(sf)
library(magrittr)

# Load top paths function -------------------------------------------------


source(here::here("functions/04_fun_K_lcp.R"))


# Load Data ---------------------------------------------------------------

hmi <- raster(here::here("data/processed/hsi_resample.tif"))
slope <- raster(here::here("data/ProcessedData/rasters/slope_crop_resamp.tif"))
imp.fuz.sum1 <- raster(here::here('data/ProcessedData/rasters/imp_fuz_sum_nohouse.tif'))
imp.fuz.sum2 <- raster(here::here('data/ProcessedData/rasters/imp_fuz_sum_jurisdiction_scenario.tif'))
imp.fuz.sum3 <- raster(here::here('data/ProcessedData/rasters/imp_fuz_sum_cattle_scenario.tif'))
house.val <- raster(here::here("data/ProcessedData/rasters/landval_crop_unscaled.tif"))

# Export resistance surfaces  for use in Circuitscape---------------------------------------------

biophys.resist <- (hmi + 1)^10 + (slope/4) #Dickson et al 2017
implementation.resist1 <- (imp.fuz.sum1 + 1)^10 + (house.val/4) 
implementation.resist2 <- (imp.fuz.sum2 + 1)^10 + (house.val/4) 
implementation.resist3 <- (imp.fuz.sum3 + 1)^10 + (house.val/4) 
writeRaster(biophys.resist, here::here("data/ProcessedData/ResistanceSurfaces/biophys_resist.tif"), overwrite=TRUE)
writeRaster(implementation.resist1, here::here("data/ProcessedData/ResistanceSurfaces/implement_resist.tif"), overwrite=TRUE)
writeRaster(implementation.resist2, here::here("data/ProcessedData/ResistanceSurfaces/implement_resist_jurisdiction.tif"), overwrite=TRUE)
writeRaster(implementation.resist3, here::here("data/ProcessedData/ResistanceSurfaces/implement_resist_cattle.tif"), overwrite=TRUE)

# Prep resistance surfaces for gdist  ---------------------------------------------
biophys.resist <- raster("data/raster_layers/biophys_resistance_layer.tif")
implementation.resist1 <- raster("data/raster_layers/social_resistance_layer.tif")
implementation.resist1[is.na(implementation.resist1[])] <- 5* cellStats(implementation.resist1, max)## drop NAs for costodistance

biophys.resist <- (hmi + 1)^10 + (slope/4) #Dickson et al 2017
implementation.resist1 <- (imp.fuz.sum1 + 1)^10 + (house.val/4) 
implementation.resist1[is.na(implementation.resist1[])] <- 5* cellStats(implementation.resist1, max)## drop NAs for costodistance
implementation.resist2 <- (imp.fuz.sum2 + 1)^10 + (house.val/4) 
implementation.resist2[is.na(implementation.resist2[])] <- 5* cellStats(implementation.resist2, max)## drop NAs for costodistance
implementation.resist3 <- (imp.fuz.sum3 + 1)^10 + (house.val/4) 
implementation.resist3[is.na(implementation.resist3[])] <- 5* cellStats(implementation.resist3, max)## drop NAs for costodistance


origins <- st_read("data/processed/all_nodes_correct.shp" )%>% 
  dplyr::filter(., NAME == "BLACKFEET" | NAME == "FORT PECK") %>%
  st_transform(. , crs(biophys.resist))

#origins <- st_read(here::here("data/ProcessedData/shapefiles/studyPAcentroids.shp")) %>% 
 # dplyr::filter(., Unit_Nm == "Weminuche Wilderness" | Unit_Nm == "Yellowstone National Park") %>%
  #st_transform(. , crs(biophys.resist))


pa.rast <- rasterize(as(origins, "Spatial"), biophys.resist, field="ID")
#writeRaster(pa.rast, here::here("data/ProcessedData/rasters/pa_locs.tif"), overwrite=TRUE)


# Create Transition Matrix ------------------------------------------------
#need to convert resistance to conductance
biophys.tr <- transition(1/biophys.resist, transitionFunction = mean, 16)
biophys.tr <- geoCorrection(biophys.tr, "c")
saveRDS(biophys.tr, here::here('data/processed/TransitionLayers/biophystrans.rds'))

social.tr1 <- transition(1/implementation.resist1, transitionFunction = mean, 16)
social.tr1 <- geoCorrection(social.tr1, "c")
saveRDS(social.tr1, here::here('data/processed/TransitionLayers/socialtrans1.rds'))

#social.tr2 <- transition(1/implementation.resist2, transitionFunction = mean, 16)
#social.tr2 <- geoCorrection(social.tr2, "c")
#saveRDS(social.tr2, here::here('Data/ProcessedData/TransitionLayers/socialtrans_jurisdiction.rds'))

#social.tr3 <- transition(1/implementation.resist3, transitionFunction = mean, 16)
#social.tr3 <- geoCorrection(social.tr3, "c")
#saveRDS(social.tr3, here::here('Data/ProcessedData/TransitionLayers/socialtrans_cattle.rds'))

# Estimate k low cost paths -----------------------------------------------

origins <- st_read(here::here("data/processed/all_nodes_correct.shp")) %>% 
  dplyr::filter(., NAME == "BLACKFEET") %>% 
  as(. , "Spatial")
goals <- st_read(here::here("data/processed/all_nodes_correct.shp")) %>% 
  dplyr::filter(., NAME == "FORT PECK") %>% 
  as(. , "Spatial")
origin.proj <- spTransform(origins, crs(biophys.resist))
goals.proj <- spTransform(goals, crs(biophys.resist))

social1 <- gen_top_paths(tr = social.tr1, resist = implementation.resist1, numpath = 5, bufdist = 4000, orig = origin.proj, goal=goals.proj)

#social2 <- gen_top_paths(tr = social.tr2, resist = implementation.resist2, numpath = 5, bufdist = 4000, orig = origin.proj, goal=goals.proj)

biophys <- gen_top_paths(tr = biophys.tr, resist = biophys.resist, numpath = 5, bufdist = 4000, orig = origin.proj, goal=goals.proj)

saveRDS(social1, here::here('data/ProcessedData/TransitionLayers/socialtop5.rds'))
saveRDS(biophys, here::here('data/ProcessedData/TransitionLayers/biophystop5.rds'))