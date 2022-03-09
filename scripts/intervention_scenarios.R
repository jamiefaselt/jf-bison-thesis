# intervention scenarios
library(raster)
library(terra)
library(dplyr)
library(sf)
library(ggmap)
library(rgdal)
library(maptools)

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
plot(social.resistance)
# does it make sense to posit that higher poverty counties may also be more likely to accept economic incentives? had spoken about this way earlier
poverty <- raster("data/raster_layers/poverty_layer.tif")
econ.scenario2 <- social.composite-econ.incentive-poverty
econ.resistance2 <- ((1+econ.scenario2)^5 + landval.pnas/4) %>% #not really usre how to mathematically justify changing the power raised...
  rescale01(.)
plot(econ.resistance2)


# Tribal Governance Scenario ----------------------------------------------
tribal.wildlife <- raster("data/raster_layers/tribal_wildlife_gov_tract.tif") %>% 
  resample(., r)
plot(tribal.wildlife)
tribal.resist <- 1-tribal.wildlife
plot(tribal.resist)
# need to think through this more philosophically
tribal.scenario <- social.composite-tribal.wildlife
plot(tribal.scenario)
writeRaster(tribal.scenario, "data/raster_layers/tribal_scenario.tif")


# Public Lands Scenario ---------------------------------------------------



