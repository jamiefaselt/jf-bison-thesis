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

#bison increase preference from Kate's model (made in the wildlife.gov repo)
# this shows the MRP estimate of the proportion of each census tract that supports bison increasing somewhat and increasing greatly
bis.inc <- raster("data/wildlife_model_tifs/bison_decrease_layer.tif") 
plot(bis.inc)
bis.inc 

# take the compliment of the increase layer to represent bison resistasnce
bis.res <- 1-bis.inc
plot(bis.res)
bis.res

writeRaster(bis.res, "data/raster_layers/bison_decrease_layer.tif", overwrite = TRUE)
