# all layers
library(raster)
library(terra)
library(dplyr)
library(sf)
library(spatialEco)
library(ggmap)
library(rgdal)
library(maptools)
library(spatialEco)
library(climateStability)
r <- raster("data/template_rstr.tif")
states <- tigris::states()
mt <- states %>% filter(., NAME=="Montana", drop=TRUE)
counties <- tigris::counties()
mt.counties<-counties %>% filter(STATEFP %in%  c("30"))
mt.counties<-st_transform(mt.counties,st_crs(r))
###################################################################
biophys <- raster("data/raster_layers/biophys_resistance.tif")
plot(biophys)
plot(st_geometry(mt.counties), add = T)
biophys
biophys.CS <- raster("circuit_outputs/biophysical_/biophysical_cum_curmap.asc")
biophys.CS
plot(log(biophys.CS))
plot(biophys.CS)
###################################################################
bis.dec <- raster("data/raster_layers/bison_decrease_layer.tif") 
plot(bis.dec)
plot(st_geometry(mt.counties), add = T)
bis.dec
###################################################################
cattle.sales <- raster("data/raster_layers/cattle_sales_layer.tif")
cattle.sales
plot(cattle.sales)
###################################################################
repub <- raster("data/raster_layers/repub_vote_layer.tif")
plot(repub)
repub
###################################################################
landval.pnas <- raster("data/raster_layers/landval_layer.tif")
landval.pnas
plot(landval.pnas)
plot(st_geometry(mt.counties), add = T)
###################################################################
parceldensity <- raster("data/raster_layers/parcel_density_layer.tif")
parceldensity
plot(parceldensity)
##################################################################
econ.incentive <- raster("data/raster_layers/econ_incentive_layer.tif")
plot(econ.incentive)
##################################################################
tribal.wildlife <- raster("data/raster_layers/tribal_wildlife_gov_jf.tif")
plot(tribal.wildlife)
##################################################################
# older data i'm not sure i will be using
###################################################################
pov <- raster("Raster_Layers/RescaledRasters/poverty.rescale.tif")
plot(pov)
plot(st_geometry(mt.counties), add = T)
pov
###################################################################
unemployment <- raster("Raster_Layers/RescaledRasters/unemployment.rescale.tif")
unemployment[unemployment==0] <- 0.00001 
plot(unemployment)
plot(st_geometry(mt.counties), add = T)
writeRaster(unemployment, "Resistance_Layers/unemployment.tif")