library(raster)
library(gdistance)
library(yenpathy)
library(rgdal)
library(sf)
library(magrittr)
library(matrixStats)

# Load top paths function -------------------------------------------------


source(here::here("scripts/11_fun_K_mst.R"))


# Load the data -----------------------------------------------------------

biophys.tr <- readRDS("data/processed/TransitionLayers/biophystrans.rds")
biophys.resist <- raster("data/raster_layers/biophys_resistance_layer.tif")
pts <- st_read("data/processed/herd_centroids.shp") %>% 
  st_centroid %>% 
  as(. , "Spatial")

tr <- biophys.tr
resist <- biophys.resist

# get k top tree ----------------------------------------------------------

ms_tree <- gen_top_tree(tr=biophys.tr, resist=biophys.resist, numpath = 3, bufdist = 4000, pts=pts)
