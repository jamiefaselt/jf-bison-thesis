# make bison decrease raster from Kate's model

library(raster)
library(terra)
library(dplyr)
library(sf)
library(spatialEco)
library(ggmap)
library(rgdal)

#template raster
r <- raster("data/template_raster.tif")

#bison increase preference from Kate's model
bis.inc <- raster("data/raster_Layers/bis_inc.tif") 
plot(bis.inc)
bis.inc 

bis.res <- 1-bis.inc
plot(bis.res)
bis.res

writeRaster(bis.res, "data/raster_layers/bison_decrease_layer.tif", overwrite = TRUE)
