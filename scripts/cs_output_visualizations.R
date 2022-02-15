# visualizations
library(raster)
library(terra)
library(dplyr)
library(sf)
library(ggmap)
library(rgdal)
library(maptools)
library(rasterVis)
library(cowplot)
library(ggplot2)
library(viridis)
##### biophys CS #####
biophys_layer <- raster("data/raster_layers/biophys_resistance_layer.tif")
biophys.cs <- raster("data/circuitscape_outputs/biophys_resistance_layer/biophys_out_cum_curmap.asc")
biophys.cs
plot(log(biophys.cs))
plot(biophys.cs)
quantile(biophys.cs)
# just playing with/learning 
levelplot(log(biophys.cs))
levelplot(biophys.cs)
plot(biophys.cs, breaks = c(0, 0.01596094, 0.04852184, .06))
image(biophys.cs, col=magma(256), zlim=c(0,.08), axes = TRUE, xaxs="i", xaxt='n', yaxt='n', ann=FALSE)

##### social composite CS #####
social_layer <- raster("data/raster_layers/social_composite_layer.tif")
social.cs <- raster("data/circuitscape_outputs/composite_social_layer/composite_social_out_cum_curmap.asc")
plot(social.cs)
plot(log(social.cs))
levelplot(log(social.cs))
quantile(social.cs)
image(social.cs, col=magma(256), zlim=c(0,.07), axes = TRUE, xaxs="i", xaxt='n', yaxt='n', ann=FALSE)


##### tribal scenario CS #####
tribal_layer <- raster("data/raster_layers/tribal_scenario.tif")
tribal.cs <- raster("data/circuitscape_outputs/tribal_scenario/tribal_scenario_out_cum_curmap.asc")
plot(tribal.cs)
plot(log(tribal.cs))
quantile(tribal.cs)
image(tribal.cs, col=magma(256), zlim=c(0,.07), axes = TRUE, xaxs="i", xaxt='n', yaxt='n', ann=FALSE)
