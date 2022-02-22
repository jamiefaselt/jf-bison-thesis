# human modificaiton index

library(sf)
library(terra)
library(raster)
r <- rast("data/template_raster.tif")
hmi <- rast("data/original/Human_Modification_Index/prj.adf")

hmi.crop <- project(hmi, r)
plot(hmi.crop)

hmi.resample <- resample(hmi.crop,r)

plot(hmi.resample)
ext(hmi.resample)
ext(r)
st_crs(r)==st_crs(hmi.resample)

writeRaster(hmi.resample, "data/processed/hmi.crop.tif", overwrite = TRUE)
