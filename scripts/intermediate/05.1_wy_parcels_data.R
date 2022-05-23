# wy parcels clean

library(tigris)
library(ggplot2)
library(tidyverse)
library(sf)
library(sp)
library(raster)
library(terra)
library(dplyr)
library(rgdal)

# bring in hsi and temp raster
r <- raster("data/template_raster.tif")
#bring in counties
counties <- tigris::counties()
# make columns match to caddat
#counties$NAME <- toupper(counties$NAME)
counties <- rename(counties, State.ANSI = STATEFP)
counties <- rename(counties, County.ANSI = COUNTYFP)
counties$State.ANSI <- as.numeric(counties$State.ANSI)
counties$County.ANSI <- as.numeric(counties$County.ANSI)
wy.counties<-counties %>% filter(State.ANSI %in%  c("56"))
wy.counties<-st_transform(wy.counties,st_crs(r))
cty <- wy.counties %>% st_transform(., st_crs(r))

##### PA Area #####
wy.padus <- st_read("data/original/PADUS2_1_StateWY_Shapefile/PADUS2_1Designation_StateWY.shp")
wy.pas <- st_transform(wy.padus, crs=st_crs(r)) %>% 
  st_make_valid(.)
#get the total number of protected acres for each county
s12 = lapply(1:nrow(cty), function(i){st_intersection(cty[i,],wy.pas)})
tst <- lapply(1:length(s12), function(x){
  s12[[x]] %>% 
    group_by(GEOID) %>% 
    summarise(., PAarea = sum(st_area(.))) %>% 
    st_drop_geometry(.)
})
pas.comb <- do.call(rbind, tst)
# join back to geometries from the county data
pa.cty.area <- cty %>% 
  left_join(., pas.comb, by = "GEOID") 
# fix the NAs
pa.cty.area$NAME <- toupper(pa.cty.area$NAME)
pa.cty.area$PAarea <- as.numeric(pa.cty.area$PAarea)
pa.cty.area$PAarea[is.na(pa.cty.area$PAarea)] = 0
# make a new column for the total number of non protected land area
pa.cty.area <- mutate(pa.cty.area, nonPAarea = ALAND - as.numeric(PAarea))
head(pa.cty.area) # this all looks good!

##### Parcel Density #####
#bring in wy parcel data
wy.parcels <- st_read("data/original/Wyoming_Parcels/Wyoming_Parcels.shp") %>% 
  st_make_valid()
st_is_valid(wy.parcels)
wy.parcels <- st_transform(wy.parcels,st_crs(r))
wy.parcels <- rename(wy.parcels,NAME = jurisdicti)
wy.parcels.drop <- st_drop_geometry(wy.parcels)
# get the total number in each jurisdiction
wy.county.parcels <- wy.parcels.drop%>%
  group_by(NAME) %>%
  summarise(parcelnb = n())    
wy.county.parcels <- rename(wy.county.parcels, TotalParcels= parcelnb)
wy.county.parcels$NAME <- gsub("BIGHORN", "BIG HORN", wy.county.parcels$NAME)
wy.county.parcels$NAME <- gsub("HOTSPRINGS", "HOT SPRINGS", wy.county.parcels$NAME)
head(wy.county.parcels)
wy.parcl.dens <- wy.county.parcels %>% 
  left_join(., pa.cty.area) %>% 
  mutate(., parceldensity = TotalParcels/nonPAarea*10000*100) # PD= patches/area then converted to 100 hectares) %>% 
head(wy.parcl.dens)
wy.parcl.dens <- st_as_sf(wy.parcl.dens)  
wy.parcl.dens.rast<-fasterize::fasterize(wy.parcl.dens, r, field = 'parceldensity')
plot(wy.parcl.dens.rast)
writeRaster(wy.parcl.dens.rast, "data/processed/wy_parcel_density.tif", overwrite = TRUE)

# currently not using size ratio so can stop here
##### Size Ratio #####
colnames(wy.parcels)
wy.parcels <- rename(wy.parcels, TotalParcels= parcelnb)
wy.parcels$NAME <- gsub("BIGHORN", "BIG HORN", wy.parcels$NAME)
wy.parcels$NAME <- gsub("HOTSPRINGS", "HOT SPRINGS", wy.parcels$NAME)
wy.parcels.area <- wy.parcels %>% mutate(., parcel.area = st_area(wy.parcels)) %>% 
  st_drop_geometry()

wy.parcl.jn.stats <- wy.parcels.area%>% 
  left_join(., pa.cty.area) %>% 
  mutate(., parcelratio = parcel.area/nonPAarea) %>% 
  group_by(GEOID) %>% 
  summarize(., medratio = median(parcelratio),
            maxratio = max(parcelratio),
            minratio = min(parcelratio),
            sdratio = sd(parcelratio))
wy.parcl.jn.stats <- left_join(cty, wy.parcl.jn.stats)
wy.parcl.stats <- st_as_sf(wy.parcl.jn.stats)

parcl.max.rast<-fasterize::fasterize(wy.parcl.stats, r, field = 'maxratio')
plot(log(parcl.max.rast))
parcl.med.rast<-fasterize::fasterize(wy.parcl.stats, r, field = 'medratio')
plot(log(parcl.med.rast))
parcl.min.rast<-fasterize::fasterize(wy.parcl.stats, r, field = 'minratio')
plot(log(parcl.min.rast))
parcl.sd.rast<-fasterize::fasterize(wy.parcl.stats, r, field = 'sdratio')
plot(log(parcl.sd.rast))


