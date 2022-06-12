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

# Prep data ---------------------------------------------------------------

# in this script I am creating a template raster, and prepping all of my social data to elimante NAs, scale from 0-1 and to match the extent, resolution, and crs of the study area.

# Template Raster ---------------------------------------------------------
#bring in shapefile of mt and wy
states <- c( "MT", "WY" )
mtwy <- tigris::states(cb=TRUE) %>%
  filter(STUSPS %in% states)
states <- tigris::states()
mt <- states %>% filter(., NAME=="Montana", drop=TRUE)

#bring in hsi layer from Brent to set crs
hsi <- raster("data/original/SUMMER_HSI_clip/SUMMER_HSI_clip.tif")
hsi

# load the herd locations
reservations <- st_read("data/original/mt_reservations/MontanaReservations.shp") %>% 
  st_transform(crs=st_crs(hsi)) %>% 
  st_make_valid(.)
apr <- st_read("data/original/AP_Property_Boundaries_011022/doc.kml") %>% 
  st_transform(crs=st_crs(hsi)) %>% 
  st_make_valid(.)
mt_NPS <- st_read("data/original/nps_boundaries/NationalParkServiceAdminBoundaries_Montana.shp")
yellowstone <- mt_NPS %>% 
  filter(., UNIT_NAME=="Yellowstone National Park",  drop=TRUE) %>% 
  st_transform(crs=st_crs(hsi)) %>% 
  st_make_valid(.)

# make sure all the projections are the same
st_crs(reservations)==st_crs(hsi)
st_crs(apr)==st_crs(hsi)
st_crs(yellowstone)==st_crs(hsi)
mtwy <- mtwy %>% st_transform(crs=st_crs(hsi))
mt <- mt %>% st_transform(crs=st_crs(hsi))

#combine them into one shapefile
yellowstone <- yellowstone %>%
  rename(NAME = UNIT_NAME)
apr <- apr %>% 
  rename(NAME = Name)

all.nodes <- bind_rows(reservations, apr, yellowstone)
class(all.nodes)
plot(st_geometry(all.nodes)) # looks good

#create a bounding box and buffer 50km to the south to include area around the YNP herd
ext <- st_bbox(all.nodes)
poly <- st_as_sfc(st_bbox(c(xmin = st_bbox(mt)[[1]], xmax = st_bbox(mt)[[3]], ymax = st_bbox(mt)[[4]], ymin = st_bbox(all.nodes)[[2]]-50000), crs = st_crs(hsi)))
r <- raster(crs= proj4string(as(poly, "Spatial")), ext=raster::extent(as(poly, "Spatial")), resolution= 540)
extent(r)
extent(st_bbox(all.nodes))

# add values
values(r) <- 1:ncell(r)
plot(r)

# by the end of this script have a template raster matched to my largest raster (the habitat suitability layer or hsi) that is cropped to a bounding box of montana and 50 km south of the yellowstone herd


# Cattle Sales Layer ----------------------------------------------------
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


# Save the template raster with a mask to cut out the Idaho area ----------
r <- mask(r, rstr)
writeRaster(r, "data/template_raster.tif", overwrite = TRUE)


# Bison Decrease Layer ----------------------------------------------------
#bison increase preference from Kate's model (made in the wildlife.gov repo)
# this shows the MRP estimate of the proportion of each census tract that supports bison increasing somewhat and increasing greatly
bis.inc <- raster("data/wildlife_model_tifs/bis.increase.map.tif") %>% 
  terra::resample(., r)
# need to fix the NA area
spat <- as(bis.inc, "SpatRaster")
new <- terra::focal(spat, w = 33, fun = "modal", na.policy="only", na.rm=TRUE)
plot(new, colNA="red")
new <- as(new, "Raster")
bis.inc.fill <- mask(new, r)
writeRaster(bis.inc.fill, "data/wildlife_model_tifs/bis.inc.fill.tif", overwrite = TRUE)
# take the compliment of the increase layer to represent bison resistasnce
bis.res <- 1-bis.inc.fill
plot(bis.res)
bis.res
writeRaster(bis.res, "data/raster_layers/bison_decrease_layer.tif", overwrite = TRUE)


# Voting Trends Layer -----------------------------------------------------
rescale01 <- function(r1) {
  r.rescale <- (r1 - cellStats(r1, min))/(cellStats(r1, max) - cellStats(r1, min))
}

votes2000.2020 <- read_csv("data/original/pres_voting/countypres_2000-2020.csv")
colnames(votes2000.2020)
head(votes2000.2020)
counties <- tigris::counties()
wy.counties<-counties %>% filter(STATEFP %in%  c("56"))
r <- raster("Data/template_raster.tif")
wy.counties<-st_transform(wy.counties,st_crs(r))
wy.counties$NAME <- toupper(wy.counties$NAME)
wy.counties <- rename(wy.counties, "county_name" = "NAME")

votes.wy <- votes2000.2020 %>% filter(state %in% c("WYOMING"))
votes.wy <- votes.wy %>% filter(party %in% c("REPUBLICAN"))
wy.rep <- votes.wy %>% mutate(., percentrepub= (candidatevotes/totalvotes)*100)
wy.rep.avg <- wy.rep %>%
  group_by(., county_name) %>%
  summarise(avgrep = mean(percentrepub))
wy.rep.avg.join <- left_join(wy.counties, wy.rep.avg) 
wy.rep.avg.join <- st_as_sf(wy.rep.avg.join)
wy.rep.avg.rast<-fasterize::fasterize(wy.rep.avg.join, r, field = 'avgrep')
plot(wy.rep.avg.rast)
####################################################
# Montana voting
mt.counties<-counties %>% filter(STATEFP %in%  c("30"))
r <- raster("Data/template_raster.tif")
mt.counties<-st_transform(mt.counties,st_crs(r))
mt.counties$NAME <- toupper(mt.counties$NAME)
mt.counties <- rename(mt.counties, "county_name" = "NAME")

votes.mt <- votes2000.2020 %>% filter(state %in% c("MONTANA"))
votes.mt <- votes.mt %>% filter(party %in% c("REPUBLICAN"))
mt.rep <- votes.mt %>% mutate(., percentrepub= (candidatevotes/totalvotes)*100)
mt.rep.avg <- mt.rep %>%
  group_by(., county_name) %>%
  summarise(avgrep = mean(percentrepub))
mt.rep.avg.join <- left_join(mt.counties, mt.rep.avg) 
mt.rep.avg.join <- st_as_sf(mt.rep.avg.join)
mt.rep.avg.rast<-fasterize::fasterize(mt.rep.avg.join, r, field = 'avgrep')
plot(mt.rep.avg.rast)

##############################################################################
# merge, rescale and save
mt.rep.avg.rast
wy.rep.avg.rast
mtwy.republican <- merge(mt.rep.avg.rast, wy.rep.avg.rast)
plot(mtwy.republican)
rescale <- rescale01(mtwy.republican)
plot(rescale)
writeRaster(rescale, "data/raster_layers/repub_vote_layer.tif", overwrite = TRUE)


# Land Value Layer from PNAS ----------------------------------------------
landval <- raster("data/original/places_fmv_pnas_dryad/places_fmv_all.tif")
landval <- projectRaster(from = landval, to= r)
landval.mask <- mask(landval, r)
plot(landval.mask)

# need to deal with NAs where there are large bodies of water
spat <- as(landval.mask, "SpatRaster")
new <- terra::focal(spat, w = 33, fun = "modal", na.policy="only", na.rm=TRUE)
plot(new, colNA="red")
new <- as(new, "Raster")
new <- mask(new, r)

rescale <- rescale01(new)
plot(rescale) # fixed the nas!
writeRaster(rescale, "data/Raster_Layers/landval_layer.tif", overwrite = TRUE)

##############################################################################
# there is one more social dataset to analyze and because of the time it takes to run that is in script 02_01_parcel_density.R script

