library(raster)
library(gdistance)
library(yenpathy)
library(rgdal)
library(sf)
library(magrittr)
library(matrixStats)

# Load top paths function -------------------------------------------------


source(here::here("scripts/07_fun_K_mst.R"))


# Load the data -----------------------------------------------------------
implementation.resist1 <- raster("data/raster_layers/social_composite_layer.tif")
implementation.resist1[is.na(implementation.resist1[])] <- 5* cellStats(implementation.resist1, max)## drop NAs for costodistance
biophys.resist <- raster("data/raster_layers/biophys_resistance_layer.tif")
biophys.resist[is.na(biophys.resist[])] <- 5* cellStats(biophys.resist, max)## drop NAs for costodistance


pts <- st_read("data/processed/herd_centroids.shp") %>% 
  st_transform(. , crs(biophys.resist))  %>% 
  st_centroid(.) %>% 
  as(. , "Spatial")


# Create or load Transition Matrix ------------------------------------------------
biophys.tr <- readRDS("data/processed/TransitionLayers/biophystrans.rds")
#biophys.tr <- transition(1/biophys.resist, transitionFunction = mean, 16)
#biophys.tr <- geoCorrection(biophys.tr, "c")
#saveRDS(biophys.tr, here::here('data/Processed/TransitionLayers/biophystrans.rds'))

social.tr1 <- readRDS("data/processed/TransitionLayers/socialtrans1.rds")
#social.tr1 <- transition(1/implementation.resist1, transitionFunction = mean, 16)
#social.tr1 <- geoCorrection(social.tr1, "c")
#saveRDS(social.tr1, here::here('data/Processed/TransitionLayers/socialtrans1.rds'))


# get k top tree ----------------------------------------------------------
ms_tree <- gen_top_tree(tr=biophys.tr, resist=biophys.resist, numpath = 3, bufdist = 4000, pts=pts)
rlist::list.save(ms_tree,"data/processed/TransitionLayers/ms_tree.rds")

# try it with a slight buffer to calc distnace...
ms_tree_nobuf <- gen_top_tree(tr=biophys.tr, resist=biophys.resist, numpath = 3, bufdist = 0, pts=pts)
rlist::list.save(ms_tree,"data/processed/TransitionLayers/ms_tree_nobuf.rds")

social1 <- gen_top_tree(tr = social.tr1, resist = implementation.resist1, numpath = 3, bufdist = 4000, pts=pts)
rlist::list.save(social1, "data/processed/transitionlayers/social_ms_tree.rds")
