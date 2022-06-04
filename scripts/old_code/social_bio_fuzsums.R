# create social + biophys layers

library(raster)
library(terra)
library(dplyr)
library(sf)
library(ggmap)
library(rgdal)
library(maptools)


rescale01 <- function(r1) {
  r.rescale <- (r1 - cellStats(r1, min))/(cellStats(r1, max) - cellStats(r1, min))
}
r <- raster("data/template_raster.tif")
cattle <- raster("data/raster_layers/cattle_sales_layer.tif") # using this as my mask since it has data for the area
r <- raster("data/template_raster.tif") %>% 
  mask(., cattle)
herds <- st_read("data/processed/herd_shapefile_outline.shp")


hsi.resample <- raster("data/processed/hsi_resample.tif")
plot(st_geometry(herds), add = TRUE)
hsi.inverse <- 1/hsi.resample
# rescale to 0-1 for standardization
# set the NA values to 1 for highest resistance (won't run in CS otherwise)
hsi.rescale <- rescale01(hsi.inverse)
hsi.rescale[is.na(hsi.rescale)]=1
hsi.rescale <- hsi.rescale %>% 
  resample(., r)
plot(hsi.rescale, col=plasma(256), axes = TRUE, main = "Habitat Suitability Resistance Layer")

# bring in the human modification layer
hmi <- raster::raster("data/processed/hmi.crop.tif") %>% 
  mask(., cattle)


# Need to run scripts in scripts/intermediate folder once -----------------
bis.dec <- raster("data/raster_layers/bison_decrease_layer.tif") 
cattle.sales <- raster("data/raster_layers/cattle_sales_layer.tif")
repub <- raster("data/raster_layers/repub_vote_layer.tif")
landval.pnas <- raster("data/raster_layers/landval_layer.tif")
parceldensity <- raster("data/raster_layers/parcel_density_layer.tif")


# fuzzy sum approach for bio plus social
rc1.1m <- 1-bis.dec
rc2.1m <- 1-cattle.sales
rc3.1m <- 1-repub
rc4.1m <- 1-parceldensity
rc5.1m <- 1-hsi.rescale
rc6.1m <- 1-hmi

fuz.sum <- 1-(rc1.1m*rc2.1m*rc3.1m*rc4.1m*rc5.1m*rc6.1m)
plot(fuz.sum) 

s_b_resistance <- ((1+fuz.sum)^10 + landval.pnas/4)
plot(s_b_resistance)

writeRaster(s_b_resistance, "data/raster_layers/s_b_resistance.tif", overwrite = TRUE)



# econ intervention b + s -------------------------------------------------
econ.data <- raster("data/raster_layers/econ_incentive_layer.tif")
bison.inc <- raster("data/raster_layers/bis_inc.tif")

econ.intervention <- 1-(bison.inc+ econ.data)
rc1.1m <- 1-econ.intervention
rc2.1m <- 1-cattle.sales
rc3.1m <- 1-repub
rc4.1m <- 1-parceldensity
rc5.1m <- 1-hsi.rescale
rc6.1m <- 1-hmi

fuz.sum <- 1-(rc1.1m*rc2.1m*rc3.1m*rc4.1m*rc5.1m*rc6.1m)
plot(fuz.sum) 


econ_b_resistance <- ((1+fuz.sum)^10 + landval.pnas/4)
plot(s_b_resistance)

writeRaster(econ_b_resistance, "data/raster_layers/econ_b_resistance.tif")


# gov intervention b + s -------------------------------------------------
tribal.wildlife <- raster("data/raster_layers/tribal_wildlife_gov_tract.tif") %>% 
  resample(., bison.inc) 

gov.intervention <- 1-(bison.inc+ tribal.wildlife)
rc1.1m <- 1-gov.intervention
rc2.1m <- 1-cattle.sales
rc3.1m <- 1-repub
rc4.1m <- 1-parceldensity
rc5.1m <- 1-hsi.rescale
rc6.1m <- 1-hmi

fuz.sum <- 1-(rc1.1m*rc2.1m*rc3.1m*rc4.1m*rc5.1m*rc6.1m)
plot(fuz.sum) 


gov_b_resistance <- ((1+fuz.sum)^10 + landval.pnas/4)
plot(gov_b_resistance)

writeRaster(gov_b_resistance, "data/raster_layers/gov_b_resistance.tif")



# view cs outputs ---------------------------------------------------------
library(raster)
library(dplyr)
library(sf)
library(ggmap)
library(rgdal)
library(maptools)
library(rasterVis)
library(cowplot)
library(ggplot2)
library(viridis)

biophys.cs<- raster("data/circuitscape_outputs/biophys_resistance_layer/biophys_out_cum_curmap.asc")
quantile(biophys.cs) 
plot(biophys.cs, col=plasma(256), zlim=c(0,.1), axes = TRUE, main = "Biophysical Baseline")
plot(st_geometry(mt.counties), add = TRUE)

bio_social_cs <- raster("data/circuitscape_outputs/social_bio/socialbio_out_cum_curmap.asc")
quantile(bio_social_cs)
plot(bio_social_cs, col=plasma(256), zlim=c(0,.1), axes = TRUE, main = "Social and Bio")
plot(st_geometry(mt.counties), add = TRUE)

econ_bio_social_cs <- raster("data/circuitscape_outputs/social_bio_econ/socialbio_econ_out_cum_curmap.asc")
plot(econ_bio_social_cs, col=plasma(256), zlim=c(0,.08), axes = TRUE, main = "Econ Intervention")
plot(st_geometry(mt.counties), add = TRUE)

gov_bio_social_cs <- raster("data/circuitscape_outputs/social_bio_gov/")
plot(gov_bio_social_cs, col=plasma(256), zlim=c(0,.08), axes = TRUE, main = "Tribal Gov Intervention")
plot(st_geometry(mt.counties), add = TRUE)

newnode_bio_social_cs <- raster("data/circuitscape_outputs/social_bio_newnode/social_bio_newnode_cum_curmap.asc")
plot(newnode_bio_social_cs, col=plasma(256), zlim=c(0,.08), axes = TRUE, main = "New Node Intervention")
plot(st_geometry(mt.counties), add = TRUE)





