# biophysical resistance layer

library(raster)
library(terra)
library(dplyr)
library(sf)
library(spatialEco)
library(ggmap)
library(rgdal)
library(maptools)
library(spatialEco)
library(climateStability)

r <- raster("/Users/jamiefaselt/Google Drive/My Drive/SpaSES Lab/Shared Data Sets/JF_Data/temp_rstr.tif")

#bring in HSI
hsi <- raster("/Users/jamiefaselt/Google Drive/My Drive/SpaSES Lab/Shared Data Sets/JF_Data/SUMMER_HSI_clip.tif")
plot(hsi)
hsi # this says the max value is 73
hist(hsi) # the histogram still shows a bunch of 128
hsi.crop <- crop(hsi, r) 
hsi.crop # this shows the max value now at 128
hsi.agg <- aggregate(hsi.crop, fact=18) # this changes the min. value to 8.29...
hsi.agg
hsi.agg[hsi.agg>73.00000001] <- NA # I know the max should only be 73
hsi.agg # this gives me seemingly correct max but different min from the intitial raster

# take the inverse of habitat suitability for resistance
hsi.inverse <- 1/hsi.agg
plot(hsi.inverse)
r1 <- hsi.inverse
rescaled.raster <- (r1 - cellStats(r1, min))/(cellStats(r1, max) - cellStats(r1, min))
plot(rescaled.raster)
diff <- rescaled.raster-hsi.rescale
plot(diff)
diff
hsi.rescale <- rescale0to1(hsi.inverse)
plot(hsi.rescale)
hsi.rescale

# bring in the human modification layer
hmi <- raster("/Users/jamiefaselt/Google Drive/My Drive/SpaSES Lab/Shared Data Sets/JF_Data/hmi.crop.tif")
hist(hmi)
plot(hmi)
hmi

# fuzzy sum approach to combine them from Theobald 2013
rc1.1m <- 1-hsi.rescale
rc2.1m <- 1-hmi
fuz.sum <- 1-(rc1.1m*rc2.1m)
plot(fuz.sum) # the result is a bunch of zeros / NA data where I think there should be higher resistance (roads, cropland and crossing private property). 
fuz.sum

rc1.1m <- 1-hsi.01
rc2.1m <- 1-hmi
fuz.sum2 <- 1-(rc1.1m*rc2.1m)
plot(fuz.sum2) # the result is a bunch of zeros / NA data where I think there should be higher resistance (roads, cropland and crossing private property). 
fuz.sum2

#write raster
writeRaster(fuz.sum, "Resistance_Layers/biophys_resistance.tif", overwrite=TRUE)
biophys <- raster("Resistance_Layers/biophys_resistance.tif")
plot(biophys)
biophys
