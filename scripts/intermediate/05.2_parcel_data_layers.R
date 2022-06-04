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
#counties$NAME <- toupper(counties$NAME)
counties <- rename(counties, State.ANSI = STATEFP)
counties <- rename(counties, County.ANSI = COUNTYFP)
counties$State.ANSI <- as.numeric(counties$State.ANSI)
counties$County.ANSI <- as.numeric(counties$County.ANSI)
mt.counties<-counties %>% filter(State.ANSI %in%  c("30"))
mt.counties<-st_transform(mt.counties,st_crs(r))
cty <- mt.counties %>% st_transform(., st_crs(r))

#bring in pas
mt.padus <- st_read("data/original/PADUS2_1_StateMT_Shapefile/PADUS2_1Designation_StateMT.shp")
mt.pas <- st_transform(mt.padus, crs=st_crs(r)) %>% 
  st_make_valid(.)
#get the total number of protected acres for each county
s12 = lapply(1:nrow(cty), function(i){st_intersection(cty[i,],mt.pas)})
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
pa.cty.area <- mutate(pa.cty.area, nonPAarea = ALAND - as.numeric(PAarea))
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
mt.pd <- rescale01(mt.pd.rast)
plot(mt.pd)


# bring back in wyoming parcel data to join and save as one tif
wy.parcl.rast <- raster("data/processed/wy_parcel_density.tif")
plot(wy.parcl.rast)
wy.pd <- rescale01(wy.parcl.rast)
mtwy.pd <- merge(mt.pd, wy.pd)
plot(mtwy.pd)
writeRaster(mtwy.pd, "data/raster_layers/parcel_density_layer.tif", overwrite = TRUE)

# by the end of 05.1 and 05.2 parcel scripts should have a raster layer showing parcel density for each county in study area. did this by taking the protected area total for each county and subtracting it from the total county area to get "parcelable land area" -- then bringing in the parcel data, calculating the number of parcels in each county, then calculating parcel density by total parcels/non pa area then converting it to hectares since that is the standard metric for parcel density 










############## can stop here unless I opt for including stats on parcels
############## need to keep the Wyoming script run for the objects to work below (did not want to save each individually before the merge)

parcl.jn <- st_drop_geometry(mt.parcels)%>% 
  left_join(., pa.cty.area, by = c("CountyName"= "NAME")) %>% 
  mutate(., parcelratio = TotalAcres/nonPAarea) %>% 
  group_by(GEOID) %>% 
  summarize(., medratio = median(parcelratio),
            maxratio = max(parcelratio),
            minratio = min(parcelratio),
            sdratio = sd(parcelratio))
parcl.jn.stats <- left_join(cty, parcl.jn)
parcl.stats <- st_as_sf(parcl.jn.stats)

parcl.max.rast<-fasterize::fasterize(parcl.stats, r, field = 'maxratio')
plot(parcl.max.rast)
parcl.med.rast<-fasterize::fasterize(parcl.stats, r, field = 'medratio')
plot(parcl.med.rast)
parcl.min.rast<-fasterize::fasterize(parcl.stats, r, field = 'minratio')
plot(log(parcl.min.rast))
parcl.sd.rast<-fasterize::fasterize(parcl.stats, r, field = 'sdratio')
plot(parcl.sd.rast)

writeRaster(parcl.max.rast, "Raster_Layers/singlestate/mt.parcel.maxratio.tif")
writeRaster(parcl.med.rast, "Raster_Layers/singlestate/mt.parcel.medratio.tif")
writeRaster(parcl.sd.rast, "Raster_Layers/singlestate/mt.parcel.sdratio.tif")

mt.med.ratio <- rescale01(parcl.med.rast)
plot(mt.med.ratio)
wy.med.ratio <- raster("/users/jamiefaselt/jf_resist/Raster_Layers/singlestate/wy.parcel.medratio.tif")
plot(wy.med.ratio)
wy.med.ratio <- rescale01(wy.med.ratio)

mtwy.med.ratio <- merge(mt.med.ratio, wy.med.ratio)
plot(mtwy.med.ratio)
mtwy.med.ratio.inverse <- 1/mtwy.med.ratio
plot(mtwy.med.ratio.inverse)
