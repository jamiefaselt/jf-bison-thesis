# Cadastral Parcel Data

library(tigris)
library(ggplot2)
library(tidyverse)
library(sf)
library(sp)
library(raster)
library(terra)
library(dplyr)
library(rgdal)

rescale01 <- function(r1) {
  r.rescale <- (r1 - cellStats(r1, min))/(cellStats(r1, max) - cellStats(r1, min))
}

# bring in hsi and temp raster
r <- raster("data/template_raster.tif")
#bring in counties
counties <- tigris::counties()
# make columns match to caddat
counties <- rename(counties, State.ANSI = STATEFP)
counties <- rename(counties, County.ANSI = COUNTYFP)
counties$State.ANSI <- as.numeric(counties$State.ANSI)
counties$County.ANSI <- as.numeric(counties$County.ANSI)
mt.counties<-counties %>% filter(State.ANSI %in%  c("30"))
mt.counties<-st_transform(mt.counties,st_crs(r))
cty <- mt.counties %>% st_transform(., st_crs(r))
cty$cty.area <- st_area(cty)
cty$cty.area <- as.numeric(cty$cty.area)

#bring in protected areas
mt.desig <- st_read("data/original/PADUS2_1_StateMT_Shapefile/PADUS2_1Designation_StateMT.shp") %>% 
  st_make_valid() %>% 
  st_transform(., st_crs(r)) %>% 
  st_as_sf()
mt.proc <- st_read("data/original/PADUS2_1_StateMT_Shapefile/PADUS2_1Proclamation_StateMT.shp") %>% 
  st_make_valid() %>% 
  st_transform(., st_crs(r)) %>% 
  st_as_sf()
# drop irrelevant designations
mt.proc<-subset(mt.proc, Loc_Ds!="WMD" & Loc_Ds!="D1" & Loc_Ds!="D2")

# join them
mt.join <- bind_rows(mt.proc, mt.desig)

# find out where they don't overlap with one of the pa datasets
diffPoly <- st_difference(mt.join, st_union(mt.proc))

plot(st_geometry(diffPoly))

# join that with the padus dataset -- should now have both pas with no overlap
mt.join.new <- bind_rows(diffPoly, mt.proc)

  
#get the total number of protected acres for each county
s12 = lapply(1:nrow(cty), function(i){st_intersection(cty[i,],mt.join.new)})
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
pa.cty.area$PAarea[is.na(pa.cty.area$PAarea)] = 0
# make a new column for the total number of non protected land area
pa.cty.area <- mutate(pa.cty.area, nonPAarea = cty.area - as.numeric(PAarea))
head(pa.cty.area)

# bring in data from parcels and calculate the total number
mt.parcels <-st_read("Data/original/Montana_Cadastral/OWNERPARCEL.shp")
mt.county.parcels <- mt.parcels%>%
  st_drop_geometry() %>% 
  group_by(CountyName) %>%
  summarise(PARCELID = n())    
mt.county.parcels <- rename(mt.county.parcels, TotalParcels= PARCELID)

parcel.dens<- mt.county.parcels %>% 
  left_join(., pa.cty.area, by = c("CountyName"= "NAME")) %>% 
  mutate(., parceldensity = TotalParcels/nonPAarea*10000*100) # PD= patches/area then converted to 100 hectares) %>% 
parcel.dens <- st_as_sf(parcel.dens)  

#st_write(parcl.jn, "parcel.density.mt.shp")
mt.pd.rast<-fasterize::fasterize(parcel.dens, r, field = 'parceldensity')
plot(mt.pd.rast)
writeRaster(mt.pd.rast, "data/processed/montana_pd.tif", overwrite = TRUE)


# bring back in wyoming parcel data to join and save as one tif
wy.pd.rast <- raster("data/processed/wy_parcel_density.tif")
plot(wy.parcl.rast)
mtwy.pd <- merge(mt.pd.rast, wy.pd.rast)
plot(mtwy.pd)
mtwy.pd <- rescale01(mtwy.pd)
writeRaster(mtwy.pd, "data/raster_layers/parcel_density_layer.tif", overwrite = TRUE)

mtwy.pd <- raster("data/raster_layers/parcel_density_layer.tif")
plot(mtwy.pd)
# by the end of 05.1 and 05.2 parcel scripts should have a raster layer showing parcel density for each county in study area. did this by taking the protected area total for each county and subtracting it from the total county area to get "parcelable land area" -- then bringing in the parcel data, calculating the number of parcels in each county, then calculating parcel density by total parcels/non pa area then converting it to hectares since that is the standard metric for parcel density 

