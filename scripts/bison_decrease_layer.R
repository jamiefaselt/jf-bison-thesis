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
bis.inc <- raster("data/raster_Layers/bis_inc.tif") %>% 
  resample(., r)
plot(bis.inc)
#bis.inc <- resample(bis.inc, r)
bis.inc 

bis.res <- 1-bis.inc
plot(bis.res)
bis.res

writeRaster(bis.res, "data/raster_layers/bison_decrease_layer.tif", overwrite = TRUE)


# fuzzy sum approach to combine them from Theobald 2013
biophys.resist <- raster("data/raster_layers/biophys_resistance.tif")
rc1.1m <- 1-bis.res
rc2.1m <- 1-biophys.resist

fuz.sum <- 1-(rc1.1m*rc2.1m)
plot(fuz.sum)
fuz.sum

# save layer of biophys+survey bison pref
writeRaster(fuz.sum, "data/raster_layers/bison_decrease_biophys_layer", overwrite = TRUE)
