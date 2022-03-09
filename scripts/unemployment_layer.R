# unemployment

library(sf)
library(tidyverse)
library(tidycensus)
library(tmap)
library(tigris)
library(car)
library(rgdal)
library(raster)

#template raster
r <- raster("data/template_raster.tif")

#load the tidycensus variables
v17 <- load_variables(2019, "acs5", cache = TRUE)
# load specific to what i'm looking for
var <- load_variables(year = 2019, dataset = "acs5/profile") %>%
  filter(str_detect(label, "Unemploy"))
# Percent!!EMPLOYMENT STATUS!!Civilian labor force!!Unemployment Rate = DP03_0005P

#download mt unemployment data and match crs
mt.tracts <- get_acs(geography = "tract",
                     variables = c(unemployment = "DP03_0005P"),
                     state = "MT",
                     year = 2019,
                     geometry = TRUE) %>% 
  #st_transform(.,st_crs(r))

#want this in a decimal form
mt.tracts <- mt.tracts %>% 
  dplyr::select(-(moe)) %>% 
  spread(key = variable, value = estimate) %>%
  mutate(rate = (unemployment/100))
mt.tracts$rate <- as.numeric(mt.tracts$rate)
max <- max(mt.tracts$rate, na.rm=TRUE)
#check the data
head(mt.tracts)

#calculate percent of max (.209)
mt.tracts <- mt.tracts %>% 
  mutate(percmax = rate/max)

#make it a raster
mt.unemp.rstr<-fasterize::fasterize(mt.tracts, r, field = 'percmax')
plot(mt.unemp.rstr)
plot(1-mt.unemp.rstr)

#download wy unemployment data and match crs
wy.tracts <- get_acs(geography = "tract",
                     variables = c(unemployment = "DP03_0005P"),
                     state = "WY",
                     year = 2019,
                     geometry = TRUE) %>% 
  st_transform(.,st_crs(r))

#want this in a decimal form
wy.tracts <- wy.tracts %>% 
  dplyr::select(-(moe)) %>% 
  spread(key = variable, value = estimate) %>%
  mutate(rate = (unemployment/100))
#check the data
head(wy.tracts)

#make it a raster
wy.unemp.rstr<-fasterize::fasterize(wy.tracts, r, field = 'rate')
plot(wy.unemp.rstr)

mtwy.unem <- merge(mt.unemp.rstr, wy.unemp.rstr)
plot(mtwy.unem)
plot(st_geometry(herds), add= TRUE)
writeRaster(unemp.resist, "data/raster_layers/unemployment_layer.tif", overwrite = TRUE)
unemp <- raster("data/raster_layers/unemployment_layer.tif")
plot(unemp)
unemp.resist <- 1-unemp
plot(unemp.resist)

herds <- st_read("data/processed/herd_shapefile_outline.shp")

# x/max(x) * 100)
max <- maxValue(mtwy.unem)
max <- (mtwy.unem/max)
unmp <- 1-max
plot(unmp)
hist(max)
hist(mtwy.unem)
plot(max)
r <- mtwy.unem

diff <- maxValue(r)-minValue(r)
max.diff <- (diff/max) 
plot(max.diff)
plot(max.diff)
q <- rescale