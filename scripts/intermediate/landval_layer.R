# private land value
# https://www.pnas.org/content/117/47/29577

library(sf)
library(tidyverse)
library(tidycensus)
library(tmap)
library(tigris)
library(car)
library(rgdal)
library(raster)
library(dplyr)

r <- raster("Data/template_raster.tif")
landval <- raster("data/original/places_fmv_all.tif")
landval <- projectRaster(from = landval, to= r)
plot(landval)
rescale <- rescale01(landval)
plot(rescale)
writeRaster(rescale, "data/Raster_Layers/landval_layer.tif")
