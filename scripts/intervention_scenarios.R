# intervention scenarios
rescale01 <- function(r1) {
  r.rescale <- (r1 - cellStats(r1, min))/(cellStats(r1, max) - cellStats(r1, min))
}

library(raster)
library(terra)
library(dplyr)
library(sf)
library(ggmap)
library(rgdal)
library(maptools)

herds <- st_read("data/processed/herd_shapefile_outline.shp")

social.composite <- raster("data/raster_layers/social_composite_layer.tif") #or?
social.resistance <- raster("data/raster_layers/social_resistance_layer.tif")
plot(social.composite) # values .66 to 1
plot(social.resistance) # values 0 to 1
landval.pnas <- raster("data/raster_layers/landval_layer.tif")
r <- raster("data/template_raster.tif")

# Economic Incentive ------------------------------------------------------
econ.incentive <- raster("data/raster_layers/econ_incentive_layer.tif")
plot(econ.incentive)

econ.scenario.survey <- social.composite-econ.incentive # social composite is already a fuzzy sum..
plot(econ.scenario.survey) # values now .59-.93
# econ incentive values are v small
# calculate new resistance here?
econ.resistance <- ((1+econ.scenario.survey)^10 + landval.pnas/4) %>% 
  rescale01(.)
plot(econ.resistance)
econ.resistance
plot(social.resistance)
econ.resistance[econ.resistance==0] <- .0001
writeRaster(econ.resistance, "data/raster_layers/econ_scenario.tif", overwrite = TRUE)


# Tribal Governance Scenario ----------------------------------------------
tribal.wildlife <- raster("data/raster_layers/tribal_wildlife_gov_tract.tif") %>% 
  resample(., r)
# need to think through this more philosophically
tribal.scenario <- social.composite-tribal.wildlife
plot(tribal.scenario)
tribal.resistance.scenario <- ((1+tribal.scenario)^10 + landval.pnas/4) %>% 
  rescale01(.)
tribal.resistance.scenario[tribal.resistance.scenario==0] <- .0001
writeRaster(tribal.resistance.scenario, "data/raster_layers/tribal_scenario.tif", overwrite = TRUE)
plot(tribal.resistance.scenario)
plot(st_geometry(herds), add= TRUE)

plot(social.resistance)
plot(st_geometry(herds), add= TRUE)

# Public Lands Scenario ---------------------------------------------------
# first want to find areas with large enough tracts of lands


