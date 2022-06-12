# make bison decrease raster from Kate's model

library(raster)
library(terra)
library(dplyr)
library(sf)
library(spatialEco)
library(ggmap)
library(rgdal)

#template raster
cattle <- raster("data/raster_layers/cattle_sales_layer.tif")
r <- raster("data/template_raster.tif") %>% 
  mask(., cattle)

#bison increase preference from Kate's model (made in the wildlife.gov repo)
# this shows the MRP estimate of the proportion of each census tract that supports bison increasing somewhat and increasing greatly
bis.inc <- raster("data/wildlife_model_tifs/bis.increase.map.tif") %>% 
  terra::resample(., r)

# need to fix the NA area
spat <- as(bis.inc, "SpatRaster")
new <- terra::focal(spat, w = 33, fun = "modal", na.policy="only", na.rm=TRUE)
plot(new, colNA="red")
new <- as(new, "Raster")
bis.inc.fill <- mask(new, r)
writeRaster(bis.inc.fill, "data/wildlife_model_tifs/bis.inc.fill.tif")

# take the compliment of the increase layer to represent bison resistasnce
bis.res <- 1-bis.inc.fill
plot(bis.res)
bis.res

writeRaster(bis.res, "data/raster_layers/bison_decrease_layer.tif", overwrite = TRUE)
