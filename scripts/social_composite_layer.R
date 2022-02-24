# composite social layer

library(raster)
library(terra)
library(dplyr)
library(sf)
library(ggmap)
library(rgdal)
library(maptools)

r <- raster("data/template_raster.tif")
bis.dec <- raster("data/raster_layers/bison_decrease_layer.tif") 
cattle.sales <- raster("data/raster_layers/cattle_sales_layer.tif")
repub <- raster("data/raster_layers/repub_vote_layer.tif")
landval.pnas <- raster("data/raster_layers/landval_layer.tif")
parceldensity <- raster("data/raster_layers/parcel_density_layer.tif")


rc1.1m <- 1-bis.dec
rc2.1m <- 1-cattle.sales
rc3.1m <- 1-repub
rc5.1m <- 1-parceldensity

fuz.sum <- 1-(rc1.1m*rc2.1m*rc3.1m*rc5.1m)
plot(fuz.sum) 

writeRaster(fuz.sum, "data/raster_layers/social_composite_layer.tif", overwrite = TRUE)
social_resistance <- (1+fuz.sum)^10 + landval.pnas/4
plot(social_resistance)
writeRaster(social_resistance, "data/raster_layers/social_resistance_layer.tif")
social_comp <- raster("data/raster_layers/social_composite_layer.tif")

