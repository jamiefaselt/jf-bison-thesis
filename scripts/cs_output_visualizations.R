# visualizations
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

r <- raster("data/template_raster.tif")
states <- tigris::states()
mt <- states %>% filter(., NAME=="Montana", drop=TRUE)
counties <- tigris::counties()
mt.counties<-counties %>% filter(STATEFP %in%  c("30"))
mt.counties<-st_transform(mt.counties,st_crs(r))

##### biophys CS nas edited#####
biophys <- raster("data/raster_layers/biophys_resistance_layer.tif")
plot(biophys)
biophys.cs<- raster("data/circuitscape_outputs/biophys_resistance_layer/biophys_out_cum_curmap.asc")
plot(biophys.cs, col=plasma(256), zlim=c(0,.08), axes = TRUE, main = "Biophysical Baseline")
plot(st_geometry(mt.counties), add = TRUE)
##### social composite CS #####
social_layer <- raster("data/raster_layers/social_composite_layer.tif")
social.cs <- raster("data/circuitscape_outputs/composite_social_layer/composite_social_out_cum_curmap.asc")
plot(social.cs)
plot(log(social.cs))
levelplot(log(social.cs))
quantile(social.cs)
plot(social.cs, col=plasma(256), zlim=c(0,.08), axes = TRUE, main = "Social Composite Circuitscape Output")
plot(st_geometry(mt.counties), add = TRUE)
plot(st_geometry(PAs, add = TRUE))

##### tribal scenario CS #####
tribal_layer <- raster("data/raster_layers/tribal_scenario.tif")
tribal.cs <- raster("data/circuitscape_outputs/tribal_scenario/tribal_scenario_out_cum_curmap.asc")
plot(tribal.cs)
plot(log(tribal.cs))
quantile(tribal.cs)
plot(tribal.cs, col=plasma(256), zlim=c(0,.08), axes = TRUE, main = "Tribal Governance Scenario")
plot(st_geometry(mt.counties), add = TRUE)

###### econ scenario cs ######
econ.cs <- raster("data/circuitscape_outputs/econ_scenario/econ_scenario_out_cum_curmap.asc")
quantile(econ.cs)
plot(econ.cs, col=plasma(256), zlim=c(0,.08), axes = TRUE, main="Economic Incentive Scenario")
plot(st_geometry(mt.counties), add = TRUE)

#### new herds scenario
new.node <- raster("data/circuitscape_outputs/newnode_composite_social_layer/newnode_composite_social_out_cum_curmap.asc")
quantile(new.node)
plot(new.node, col=plasma(256), zlim=c(0,.08), axes = TRUE, main="New Herd Incentive Scenario")
plot(st_geometry(mt.counties), add = TRUE)
