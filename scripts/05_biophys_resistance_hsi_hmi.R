# biophysical resistance layer

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
r <- rast("data/template_raster.tif")
landval <- rast("data/raster_layers/landval_layer.tif") #loading this to mask lakes from biophys layer 
lake.mask <- is.na(landval)
lake.mask[lake.mask != 1] <- NA
lake.mask <- mask(lake.mask, r)
plot(lake.mask)
#herds <- st_read("data/processed/herd_shapefile_outline.shp")
# resample the hsi layer to match the extent and resolution of template raster
hsi <- rast("data/original/SUMMER_HSI_clip/SUMMER_HSI_clip.tif")
hsi.sa <- terra::project(hsi, r) #this crops and resamples to the proper extent
hsi.sa[hsi.sa>73] <- NA # it says the max is 73 but the histogram shows that there are values over 100, Brock thinks this is from converting the file from ArcGIS
hsi.sa <- mask(hsi.sa, r)

#write this for future use so I won't have to resample again!
writeRaster(hsi.sa, "data/processed/hsi_resample.tif", overwrite = TRUE) 

# take the inverse of habitat suitability for resistance
#hsi.resample <- raster("data/processed/hsi_resample.tif")
hsi.inverse <- 1/hsi.sa
plot(hsi.inverse)
#table(is.na(hsi.resample[]))
plot(hsi.inverse, colNA="red")

# rescale to 0-1 for standardization
# set the NA values to 1 for highest resistance (won't run in CS otherwise)
hsi.rescale <- rescale01(raster(hsi.inverse))
hsi.rescale[is.na(hsi.rescale)]<-1
hsi.rescale[raster(lake.mask)] <- NA
hsi.rescale <- mask(hsi.rescale, raster(r))


# bring in the human modification layer
hmi <- rast("data/original/Human_Modification_Index/prj.adf")
hmi.resample <- raster(terra::project(hmi,r))
plot(hmi.resample)
ext(hmi.resample)
ext(r)
st_crs(r)==st_crs(hmi.resample)
writeRaster(hmi.resample, "data/processed/hmi_crop.tif", overwrite = TRUE)

#hmi <- raster::raster("data/processed/hmi_crop.tif")

# fuzzy sum approach to combine them from Theobald 2013
fuzzysum <- function(r1, r2) {
  rc1.1m <- (1-r1)
  rc2.1m <- (1-r2)
  fuz.sum <- 1-(rc1.1m*rc2.1m)
}
biophys_fuzsum <- fuzzysum(hsi.rescale, hmi.resample)
plot(biophys_fuzsum, col=plasma(256), axes = TRUE, main = "HSI+HMI Resistance Layer")

elev <- getData(name='alt', country= 'USA')[[1]]
slope <- terrain(elev, v="slope", unit="degrees")
slope.rast <- rast(slope)
slope.sa <- terra::project(slope.rast, r)
slope.mask <- mask(slope.sa, r)
# make into resistance surface
biophys_resistance <- ((1+biophys_fuzsum)^10) + (raster(slope.mask)/4)
plot(biophys_resistance, col=plasma(256), axes = TRUE, main = "HSI+HMI Resistance Layer")

#write raster (saving both gdrive and local computer)
writeRaster(biophys_resistance, "data/raster_layers/biophys_resistance_layer.tif", overwrite = TRUE)

# now we have the biophysical resistance surface based on the habitat suitability index and human modification layers combined with a fuzzy sum approach and turned into a resistance layer using methods from Dickson et al.

# before moving on to the bivariate maps need to run the biophysical resistance layer and social composite resistance layers in circuitscape

# open julia
# can use parallel processors with this code: JULIA_NUM_THREADS=4
# enter: using Circuitscape
# set working directory: cd("/Users/NAME/jf-bison-thesis/")
# enter: compute("ini_files/biophys.ini")
# compute("ini_files/social.ini")


