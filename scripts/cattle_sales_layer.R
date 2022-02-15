# cattle sales_incl calves
# URL=https://quickstats.nass.usda.gov/results/82C79A41-78F2-3241-A349-33BFD4790E4B

library(tigris)
library(ggplot2)
library(tidyverse)
library(sf)
library(sp)
library(raster)
library(dplyr)
library(rgdal)
library(ggmap)
library(usmap)
library(fasterize)

# bring in hsi and temp raster
r <- raster("data/template_raster.tif")
hsi <- raster("data/processed/hsi_resample.tif")
cattle_sales <- read.csv("data/original/NASS_data/cattle_sales_MTWY.csv")
#bring in counties
counties <- tigris::counties()
counties<-counties %>% filter(STATEFP %in%  c("30", "56"))
counties<-st_transform(counties,st_crs(hsi))
st_crs(counties)

# make columns match to nass data
counties$NAME <- toupper(counties$NAME)
counties <- rename(counties, State.ANSI = STATEFP)
counties <- rename(counties, County.ANSI = COUNTYFP)
counties$State.ANSI <- as.numeric(counties$State.ANSI)
counties$County.ANSI <- as.numeric(counties$County.ANSI)

#plot(counties) #checking and this doesn't have weird gaps yet
cattle_sales$Value <- gsub(",","",cattle_sales$Value)
cattle_sales$Value <- as.numeric(cattle_sales$Value)

# join
cattlesales.spatial <- left_join(counties, cattle_sales)

# double check projection
st_crs(counties) == st_crs(cattlesales.spatial) #true
st_is_valid(cattlesales.spatial) #true
plot(cattlesales.spatial)
#subset to relevant variables
cattle.sales.sub <- cattlesales.spatial %>% 
  dplyr::select(geometry,Value,County.ANSI,State.ANSI)
class(cattle.sales.sub)
cattlesales <- st_as_sf(cattle.sales.sub)
#make this a raster with temp.raster already loaded
rstr<<-fasterize::fasterize(cattlesales, r, field = 'Value')
hist(rstr)
plot(rstr) 

#rescale this from 0-1
rstr <- rescale01(rstr)
plot(rstr)
writeRaster(rstr, "data/raster_layers/cattle_sales_layer.tif", overwrite=TRUE)

