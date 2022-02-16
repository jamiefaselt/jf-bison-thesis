# intervention scenarios
library(raster)
library(terra)
library(dplyr)
library(sf)
library(ggmap)
library(rgdal)
library(maptools)

econ.incentive <- raster("data/raster_layers/econ_incentive_layer.tif")
plot(econ.incentive)

tribal.wildlife <- raster("data/raster_layers/tribal_wildlife_gov_jf.tif")
plot(tribal.wildlife)
tribal.resist <- 1-tribal.wildlife
plot(tribal.resist)

social.composite <- raster("data/raster_layers/social_composite_layer.tif")
plot(social.composite)

# not sure how to do this part
tribal.scenario <- social.composite-tribal.wildlife
plot(tribal.scenario)
writeRaster(tribal.scenario, "data/raster_layers/tribal_scenario.tif")

econ.scenario.survey <- social.composite-econ.incentive
plot(econ.scenario.survey)
# econ incentive values are v small
poverty <- raster("data/raster_layers/poverty_layer.tif")
econ.scenario <- social.composite-econ.incentive-poverty
plot(econ.scenario)
econ.rescale <- rescale01(econ.scenario)
econ.rescale[econ.rescale==0] <- 0.0001 
writeRaster(econ.rescale, "data/raster_layers/econ_scenario.tif", overwrite = TRUE)
