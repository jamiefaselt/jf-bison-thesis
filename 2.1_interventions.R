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

cattle <- raster("data/raster_layers/cattle_sales_layer.tif")
r <- raster("data/template_raster.tif") %>% 
  mask(., cattle)

herds <- raster("data/processed/all_nodes.tif")

# Economic Intervention Scenario ------------------------------------------
econ.data <- raster("data/raster_layers/econ_incentive_layer.tif")
bison.inc <- raster("data/raster_layers/bis_inc.tif")
econ.intervention <- 1-(bison.inc- econ.data)
cattle.sales <- raster("data/raster_layers/cattle_sales_layer.tif")
repub <- raster("data/raster_layers/repub_vote_layer.tif")
parceldensity <- raster("data/raster_layers/parcel_density_layer.tif")
landval.pnas <- raster("data/raster_layers/landval_layer.tif")

# fuzzy sum approach
rc1.1m <- 1-econ.intervention
rc2.1m <- 1-repub
rc3.1m <- 1-parceldensity
rc4.1m <- 1-cattle.sales

fuz.sum <- 1-(rc1.1m*rc2.1m*rc3.1m*rc4.1m)
plot(fuz.sum) 

#make resistance
econ.scenario <- ((1+fuz.sum)^10 + landval.pnas/4)
plot(econ.scenario)
plot(st_geometry(herds), add= TRUE)
writeRaster(econ.scenario, "data/raster_layers/econ_scenario.tif", overwrite = TRUE)

# Tribal Governance Scenario ----------------------------------------------
tribal.wildlife <- raster("data/raster_layers/tribal_wildlife_gov_tract.tif") %>% 
  resample(., bison.inc) 
bison.inc <- raster("data/raster_layers/bis_inc.tif")
tribal.scenario <- 1-(bison.inc-tribal.wildlife)
cattle.sales <- raster("data/raster_layers/cattle_sales_layer.tif")
repub <- raster("data/raster_layers/repub_vote_layer.tif")
parceldensity <- raster("data/raster_layers/parcel_density_layer.tif")
landval.pnas <- raster("data/raster_layers/landval_layer.tif")

# fuzzy sum approach
rc1.1m <- 1-tribal.scenario
rc2.1m <- 1-repub
rc4.1m <- 1-cattle.sales

fuz.sum <- 1-(rc1.1m*rc2.1m*rc3.1m*rc4.1m)
plot(fuz.sum) 

#make resistance
tribal.scenario <- ((1+fuz.sum)^10 + landval.pnas/4)
plot(tribal.scenario)
plot(st_geometry(herds), add= TRUE)
writeRaster(tribal.scenario, "data/raster_layers/tribal_scenario.tif", overwrite = TRUE)



# DOI Friendly Scenario ---------------------------------------------------
new.herds <- st_read("data/processed/publand_shortcircuit.shp")
new.herds$ID <- seq(1, nrow(new.herds))

newherd.rast<-fasterize::fasterize(new.herds, r, field = 'ID')
plot(newherd.rast)
writeRaster(newherd.rast, "data/processed/shortcircuit.tif")

# the same composite fuzzy sum approach is used for a resistance layer but the shortcircuit file is added to the ini file for CS
