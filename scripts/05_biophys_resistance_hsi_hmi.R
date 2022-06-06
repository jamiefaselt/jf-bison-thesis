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

# rescale function to use later on for combining rasters
rescale01 <- function(r1) {
  r.rescale <- (r1 - cellStats(r1, min))/(cellStats(r1, max) - cellStats(r1, min))
}

#template raster
r <- raster("data/template_raster.tif")
cattle <- raster("data/raster_layers/cattle_sales_layer.tif") # using this as my mask since it has data for the area
r <- raster("data/template_raster.tif") %>% 
  mask(., cattle)
herds <- st_read("data/processed/herd_shapefile_outline.shp")
# resample the hsi layer to match the extent and resolution of template raster
hsi <- raster("data/original/SUMMER_HSI_clip/SUMMER_HSI_clip.tif")
hsi # it says the max is 73 but the histogram shows that there are values over 100, Brock thinks this is from converting the file from ArcGIS
# hist(hsi) this takes a while but shows how there are a bunch of values around 128
hsi.resample <- resample(hsi, r, na.rm = TRUE) # choosing to resample here to get the extents and resolution match up-- already same projection since I built the template raster off of the hsi projection
hsi.resample # something happens where the max is now 128
plot(hsi.resample)
#table(is.na(hsi.resample[]))
# according to Brent (creater of hsi layer) the max value should be 73 NOT 128
# fix the max value here
hsi.resample[hsi.resample>73] <- NA
hsi.resample 
plot(hsi.resample, colNA="red")
#write this for future use so I won't have to resample again!
writeRaster(hsi.resample, "data/processed/hsi_resample.tif", overwrite = TRUE) 

# take the inverse of habitat suitability for resistance
hsi.resample <- raster("data/processed/hsi_resample.tif")
hsi.inverse <- 1/hsi.resample
plot(hsi.inverse)
table(is.na(hsi.resample[]))
plot(hsi.resample, colNA="red")

# rescale to 0-1 for standardization
# set the NA values to 1 for highest resistance (won't run in CS otherwise)
hsi.rescale <- rescale01(hsi.inverse)
hsi.rescale[is.na(hsi.rescale)]=1
hsi.rescale

# bring in the human modification layer
hmi <- raster("data/original/Human_Modification_Index/prj.adf")
hmi.resample <- resample(hmi,r)
plot(hmi.resample)
ext(hmi.resample)
ext(r)
st_crs(r)==st_crs(hmi.resample)
writeRaster(hmi.resample, "data/processed/hmi_crop.tif", overwrite = TRUE)

hmi <- raster::raster("data/processed/hmi_crop.tif")

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
biophys_mask <- mask(biophys_resistance, r)
plot(biophys_mask, col=plasma(256), axes = TRUE, main = "HSI+HMI Resistance Layer")

#write raster (saving both gdrive and local computer)
writeRaster(biophys_mask, "data/raster_layers/biophys_resistance_layer.tif", overwrite = TRUE)


# now we have the biophysical resistance surface based on the habitat suitability index and human modification layers combined with a fuzzy sum approach and turned into a resistance layer using methods from Dickson et al.

# before moving on to the bivariate maps need to run the biophysical resistance layer and social composite resistance layers in circuitscape

# open julia
# can use parallel processors with this code: JULIA_NUM_THREADS=4
# enter: using Circuitscape
# set working directory: cd("/Users/NAME/jf-bison-thesis/")
# enter: compute("ini_files/biophys.ini")
# compute("ini_files/social.ini")


