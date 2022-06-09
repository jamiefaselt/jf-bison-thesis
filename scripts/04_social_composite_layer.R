# composite social layer
# rescale function to use later on for combining rasters
rescale01 <- function(r1) {
  r.rescale <- (r1 - cellStats(r1, min))/(cellStats(r1, max) - cellStats(r1, min))
}

library(raster)
library(terra)
library(dplyr)
library(sf)
library(ggmap)
library(rgdal)
library(maptools)
library(terra)

herds <- st_read("data/processed/herd_shapefile_outline.shp")

r <- raster("data/template_raster.tif")


# Need to run scripts in scripts/intermediate folder once -----------------

bis.dec <- raster("data/raster_layers/bison_decrease_layer.tif") 
cattle.sales <- raster("data/raster_layers/cattle_sales_layer.tif")
repub <- raster("data/raster_layers/repub_vote_layer.tif")
landval.pnas <- raster("data/raster_layers/landval_layer.tif")
parceldensity <- raster("data/processed/parcel_dens_update.tif") %>% 
  terra::resample(., r) %>% 
  rescale01(.)
  
plot(parceldensity)

# fuzzy sum approach
rc1.1m <- 1-bis.dec
rc2.1m <- 1-cattle.sales
rc3.1m <- 1-repub
rc4.1m <- 1-parceldensity

fuz.sum <- 1-(rc1.1m*rc2.1m*rc3.1m*rc4.1m)
plot(fuz.sum) 

writeRaster(fuz.sum, "data/processed/social_fuzsum.tif", overwrite = TRUE)
social_resistance <- ((1+fuz.sum)^10 + landval.pnas/4)
plot(social_resistance)
plot(st_geometry(herds), add= TRUE)

writeRaster(social_resistance, "data/raster_layers/social_resistance_layer.tif", overwrite = TRUE)


# make a null raster based on min social resistance 
cellStats(social_resistance, min)
r <- setMinMax(r)
values(r) <-cellStats(social_resistance, min)

writeRaster(r, "data/processed/null_resistance_layer.tif", overwrite = TRUE)
