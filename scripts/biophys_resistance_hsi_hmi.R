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
hsi.inverse <- 1/hsi.resample
plot(hsi.inverse)

# rescale to 0-1 for standardization
hsi.rescale <- rescale01(hsi.inverse)
hsi.rescale[is.na(hsi.rescale)]=1
hsi.rescale
plot(hsi.rescale)

# bring in the human modification layer
hmi <- raster("data/processed/hmi.crop.tif")
plot(hmi)
# fuzzy sum approach to combine them from Theobald 2013
biophys_fuzsum <- fuzzysum(hsi.rescale, hmi)
plot(biophys_fuzsum)
biophys_fuzsum

#this does the same thing as the function above...
rc1.1m <- 1-hsi.rescale
rc2.1m <- 1-hmi
fuz.sum <- 1-(rc1.1m*rc2.1m)
plot(fuz.sum) 
fuz.sum

#write raster (saving both gdrive and local computer)
writeRaster(biophys_fuzsum, "data/raster_layers/biophys_resistance_nas_edited.tif", overwrite = TRUE)

