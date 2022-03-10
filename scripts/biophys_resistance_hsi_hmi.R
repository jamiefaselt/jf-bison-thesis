# biophysical resistance layer

library(raster)
library(terra)
library(dplyr)
library(sf)
library(spatialEco)
library(ggmap)
library(rgdal)
library(maptools)

#template raster
r <- raster("data/template_raster.tif")
herds <- st_read("data/processed/herd_shapefile_outline.shp")
# resample the hsi layer to match the extent and resolution of template raster
hsi <- raster("data/original/SUMMER_HSI_clip/SUMMER_HSI_clip.tif")
hsi.resample <- resample(hsi, r)
plot(hsi.resample)
#write this for future use so I won't have to resample again!

# according to Brent (creater of hsi layer) the max value should be 73 NOT 128
# fix the max value here
hsi.resample[hsi.resample>73] <- NA
hsi.resample
writeRaster(hsi.resample, "data/processed/hsi_resample.tif")

# take the inverse of habitat suitability for resistance
hsi.resample <- raster("data/processed/hsi_resample.tif")
plot(hsi.resample)
plot(st_geometry(herds), add = TRUE)
hsi.inverse <- 1/hsi.resample
plot(hsi.inverse)

# rescale to 0-1 for standardization
hsi.rescale <- rescale01(hsi.inverse)
hsi.rescale[is.na(hsi.rescale)]=1
hsi.rescale
plot(hsi.rescale, col=plasma(256), axes = TRUE, main = "Habitat Suitability Resistance Layer")
plot(st_geometry(mt.counties), add = TRUE)

# bring in the human modification layer
hmi <- raster("data/processed/hmi.crop.tif")
plot(hmi, col=plasma(256), axes = TRUE, main = "Human Modification Layer")
plot(st_geometry(mt.counties), add = TRUE)

# fuzzy sum approach to combine them from Theobald 2013
biophys_fuzsum <- fuzzysum(hsi.rescale, hmi)
plot(biophys_fuzsum, col=plasma(256), axes = TRUE, main = "HSI+HMI Resistance Layer")
plot(st_geometry(mt.counties), add = TRUE)


biophys_resistance <- (1+biophys_fuzsum)^10
plot(biophys_resistance, col=plasma(256), axes = TRUE, main = "HSI+HMI Resistance Layer")


#write raster (saving both gdrive and local computer)
writeRaster(biophys_fuzsum, "data/raster_layers/biophys_resistance_layer.tif", overwrite = TRUE)
b <- raster("data/raster_layers/biophys_resistance_layer.tif")
plot(b)
