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

rescale01 <- function(r1) {
  r.rescale <- (r1 - cellStats(r1, min))/(cellStats(r1, max) - cellStats(r1, min))
}

cattle.sales <- raster("data/raster_layers/cattle_sales_layer.tif")
r <- raster("Data/template_raster.tif") %>% 
  mask(., cattle.sales)
landval <- raster("data/original/places_fmv_pnas_dryad/places_fmv_all.tif")
landval <- projectRaster(from = landval, to= r)
landval.mask <- mask(landval, r)
plot(landval.mask)

# need to deal with NAs where there are large bodies of water
spat <- as(landval.mask, "SpatRaster")
new <- terra::focal(spat, w = 33, fun = "modal", na.policy="only", na.rm=TRUE)
plot(new, colNA="red")
new <- as(new, "Raster")
new <- mask(new, r)

rescale <- rescale01(new)
plot(rescale) # fixed the nas!
writeRaster(rescale, "data/Raster_Layers/landval_layer.tif", overwrite = TRUE)
