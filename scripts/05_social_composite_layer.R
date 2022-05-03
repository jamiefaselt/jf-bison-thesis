# composite social layer
# just once need to run all files in intermediate script folder

library(raster)
library(terra)
library(dplyr)
library(sf)
library(ggmap)
library(rgdal)
library(maptools)

herds <- st_read("data/processed/herd_shapefile_outline.shp")

r <- raster("data/template_raster.tif")
bis.dec <- raster("data/raster_layers/bison_decrease_layer.tif") 
cattle.sales <- raster("data/raster_layers/cattle_sales_layer.tif")
repub <- raster("data/raster_layers/repub_vote_layer.tif")
landval.pnas <- raster("data/raster_layers/landval_layer.tif")
parceldensity <- raster("data/raster_layers/parcel_density_layer.tif")


# fuzzy sum approach
rc1.1m <- 1-bis.dec
rc2.1m <- 1-cattle.sales
rc3.1m <- 1-repub
rc5.1m <- 1-parceldensity

fuz.sum <- 1-(rc1.1m*rc2.1m*rc3.1m*rc5.1m)
plot(fuz.sum) 

writeRaster(fuz.sum, "data/processed/social_fuzsum.tif")
social_resistance <- ((1+fuz.sum)^10 + landval.pnas/4)
plot(social_resistance)
plot(st_geometry(herds), add= TRUE)

writeRaster(social_resistance, "data/raster_layers/social_resistance_layer.tif", overwrite = TRUE)
s <- raster("data/raster_layers/social_resistance_layer.tif")
s
#check to see what is contributing most?
nf <- layout( matrix(c(1,2), ncol=2) )
plot(social_resistance)
plot(bis.dec)
plot(social_resistance)
plot(cattle.sales)
plot(social_resistance)
plot(repub)
plot(social_resistance)

# make a null raster
cellStats(s, min)
r <- setMinMax(r)
values(r) <- 160.084

writeRaster(r, "data/processed/null_resistance_layer.tif")
