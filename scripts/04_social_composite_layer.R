# composite social layer


library(raster)
library(terra)
library(dplyr)
library(sf)
library(ggmap)
library(rgdal)
library(maptools)

herds <- st_read("data/processed/herd_shapefile_outline.shp")

r <- raster("data/template_raster.tif")


# Need to run scripts in scripts/intermediate folder once -----------------

bis.dec <- raster("data/raster_layers/bison_decrease_layer.tif") 
cattle.sales <- raster("data/raster_layers/cattle_sales_layer.tif")
repub <- raster("data/raster_layers/repub_vote_layer.tif")
landval.pnas <- raster("data/raster_layers/landval_layer.tif")
parceldensity <- raster("data/raster_layers/parcel_density_layer.tif")


# fuzzy sum approach
rc1.1m <- 1-bis.dec
rc2.1m <- 1-cattle.sales
rc3.1m <- 1-repub
rc4.1m <- 1-parceldensity

fuz.sum <- 1-(rc1.1m*rc2.1m*rc3.1m*rc4.1m)
plot(fuz.sum) 

writeRaster(fuz.sum, "data/processed/social_fuzsum.tif")
social_resistance <- ((1+fuz.sum)^10 + landval.pnas/4)
plot(social_resistance)
plot(st_geometry(herds), add= TRUE)

writeRaster(social_resistance, "data/raster_layers/social_resistance_layer.tif", overwrite = TRUE)


# make a null raster based on min social resistance 
cellStats(s, min)
r <- setMinMax(r)
values(r) <- 160.084

writeRaster(r, "data/processed/null_resistance_layer.tif", overwrite = TRUE)
