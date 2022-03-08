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
r <- raster("Data/template_raster.tif")

#load the tidycensus variables
v17 <- load_variables(2019, "acs5", cache = TRUE)
# load specific to what i'm looking for
var <- load_variables(year = 2019, dataset = "acs5/profile") %>%
  filter(str_detect(label, "Unemploy"))

#download mt unemployment data and match crs
mt.tracts <- get_acs(geography = "tract",
                     variables = c(unemployment = "DP03_0005P"),
                     state = "MT",
                     year = 2019,
                     geometry = TRUE) %>% 
  st_transform(.,st_crs(r))

#want this in a decimal form
mt.tracts <- mt.tracts %>% 
  dplyr::select(-(moe)) %>% 
  spread(key = variable, value = estimate) %>%
  mutate(rate = (unemployment/100))
#check the data
head(mt.tracts)

#make it a raster
mt.unemp.rstr<-fasterize::fasterize(mt.tracts, r, field = 'rate')
plot(mt.unemp.rstr)


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
writeRaster(mtwy.unem, "data/raster_layers/unemployment_layer.tif", overwrite = TRUE)
