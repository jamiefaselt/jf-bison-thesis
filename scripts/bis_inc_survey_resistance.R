# bison increase raster from Kate's model

library(raster)
library(terra)
library(dplyr)
library(sf)
library(spatialEco)
library(ggmap)
library(rgdal)


r <- raster("/Users/jamiefaselt/jf_resist/Data/temp_rstr.tif")

bis.inc <- raster("Raster_Layers/bis_inc.tif") %>% 
  resample(., r)
plot(bis.inc)
#bis.inc <- resample(bis.inc, r)
plot(bis.inc)
bis.inc
bis.inc <- rescale0to1(bis.inc)

bis.res <- 1-bis.inc
plot(bis.res)
bis.res <- rescale0to1(bis.res)
bis.res
hist(bis.res)
plot(bis.res)
# won't run alone in circuitscape because of zero values
bis.res[bis.res==0] <- 0.00001 

writeRaster(bis.res, "Resistance_Layers/survey.bis.dec.alone.tif", overwrite = TRUE)


# fuzzy sum approach to combine them from Theobald 2013
rc1.1m <- 1-bis.res
rc2.1m <- 1-biophys.resist

fuz.sum <- 1-(rc1.1m*rc2.1m)
plot(fuz.sum)
fuz.sum

# save layer of biophys+survey bison pref
writeRaster(fuz.sum, "Resistance_Layers/bison_inc_biophys.tif", overwrite = TRUE)
