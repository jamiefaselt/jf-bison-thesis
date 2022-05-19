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

herds <- st_read("data/processed/all_nodes.tif")

#to compare later
social.composite <- raster("data/raster_layers/social_composite_layer.tif") #or?
social.resistance <- raster("data/raster_layers/social_resistance_layer.tif")
r <- raster("data/template_raster.tif")

# Economic Incentive ------------------------------------------------------
econ.incentive <- raster("data/raster_layers/econ_incentive_layer.tif")
# going to subtract this from cattle layer- do another fuzzy sum and convert back to resistance
bis.dec <- raster("data/raster_layers/bison_decrease_layer.tif") 
cattle.sales <- raster("data/raster_layers/cattle_sales_layer.tif")
econ.intervention <- cattle.sales-econ.incentive
repub <- raster("data/raster_layers/repub_vote_layer.tif")
landval.pnas <- raster("data/raster_layers/landval_layer.tif")
parceldensity <- raster("data/raster_layers/parcel_density_layer.tif")

# fuzzy sum approach
rc1.1m <- 1-bis.dec
rc2.1m <- 1-econ.intervention
rc3.1m <- 1-repub
rc5.1m <- 1-parceldensity

fuz.sum <- 1-(rc1.1m*rc2.1m*rc3.1m*rc5.1m)
plot(fuz.sum) 

#make resistance
econ.scenario <- ((1+fuz.sum)^10 + landval.pnas/4)
plot(econ.scenario)
plot(st_geometry(herds), add= TRUE)
writeRaster(econ.scenario, "data/raster_layers/econ_scenario_rl.tif", overwrite = TRUE)


# Tribal Governance Scenario ----------------------------------------------
tribal.wildlife <- raster("data/raster_layers/tribal_wildlife_gov_tract.tif") 
plot(tribal.wildlife)
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


