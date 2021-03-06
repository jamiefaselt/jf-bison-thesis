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

rescale01 <- function(r1) {
  r.rescale <- (r1 - cellStats(r1, min))/(cellStats(r1, max) - cellStats(r1, min))
}


# bring in hsi and temp raster
r <- raster("data/template_raster.tif")
hsi <- raster("data/original/SUMMER_HSI_clip/SUMMER_HSI_clip.tif")
cattle_sales <- read.csv("data/original/NASS_data/cattle_sales_MTWY.csv")

#bring in counties to make the NASS data spatial
counties <- tigris::counties() %>% 
  filter(STATEFP %in% c("30", "56")) %>% 
  st_transform(., st_crs(hsi))

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

# there are NA values for non-disclosed counties-- need Park County, Wyoming for this analysis
# look at the total animal sales and filter to similar values to Park County
sales <- read_csv("data/original/NASS_data/animal_sales_totals.csv")
sales$Value <- gsub(",","",sales$Value)
sales$Value <- as.numeric(sales$Value)

# park county is 24,112,000 making range plus and minus 2

park.range <- sales %>% 
  filter(Value %in% (22112000:26112000)) 

new <- cattle_sales[cattle_sales$County %in% c("HILL", "POWELL", "SWEET GRASS", "UINTA"), ] 

median <- median(new$Value)


# back to the spatial dataset
cattlesales.spatial[is.na(cattlesales.spatial)] <- median



# double check projection
st_crs(counties) == st_crs(cattlesales.spatial) #true
st_is_valid(cattlesales.spatial) #true
#subset to relevant variables
cattle.sales.sub <- cattlesales.spatial %>% 
  dplyr::select(geometry,Value,County.ANSI,State.ANSI)
class(cattle.sales.sub)
cattlesales <- st_as_sf(cattle.sales.sub)
#make this a raster with temp.raster already loaded
rstr<<-fasterize::fasterize(cattlesales.spatial, r, field = 'Value')
hist(rstr)
plot(rstr) 

#rescale this from 0-1
rstr <- rescale01(rstr)
plot(rstr)
writeRaster(rstr, "data/raster_layers/cattle_sales_layer.tif", overwrite=TRUE)

