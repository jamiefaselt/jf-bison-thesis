# biophysical resistance layer
# just once need to run intermediate/hmi_layer for this resistance layer

library(raster)
library(terra)
library(dplyr)
library(sf)
library(spatialEco)
library(ggmap)
library(rgdal)
library(maptools)
library(viridis)

rescale01 <- function(r1) {
  r.rescale <- (r1 - cellStats(r1, min))/(cellStats(r1, max) - cellStats(r1, min))
}

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
# set the NA values to 1 for highest resistance (won't run in CS otherwise)
hsi.rescale <- rescale01(hsi.inverse)
hsi.rescale[is.na(hsi.rescale)]=1
hsi.rescale
plot(hsi.rescale, col=plasma(256), axes = TRUE, main = "Habitat Suitability Resistance Layer")

# bring in the human modification layer
hmi <- raster("data/processed/hmi.crop.tif")
plot(hmi, col=plasma(256), axes = TRUE, main = "Human Modification Layer")

# fuzzy sum approach to combine them from Theobald 2013
fuzzysum <- function(r1, r2) {
  rc1.1m <- (1-r1)
  rc2.1m <- (1-r2)
  fuz.sum <- 1-(rc1.1m*rc2.1m)
}
biophys_fuzsum <- fuzzysum(hsi.rescale, hmi)
plot(biophys_fuzsum, col=plasma(256), axes = TRUE, main = "HSI+HMI Resistance Layer")

# make into resistance surface
biophys_resistance <- (1+biophys_fuzsum)^10
plot(biophys_resistance, col=plasma(256), axes = TRUE, main = "HSI+HMI Resistance Layer")


#write raster (saving both gdrive and local computer)
writeRaster(biophys_resistance, "data/raster_layers/biophys_resistance_layer.tif", overwrite = TRUE)
