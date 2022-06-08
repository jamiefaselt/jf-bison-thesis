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
counties <- rename(counties, State.ANSI = STATEFP)
counties <- rename(counties, County.ANSI = COUNTYFP)
counties$State.ANSI <- as.numeric(counties$State.ANSI)
counties$County.ANSI <- as.numeric(counties$County.ANSI)
wy.counties<-counties %>% filter(State.ANSI %in%  c("56"))
cty<-st_transform(wy.counties,st_crs(r))
cty$cty.area <- st_area(cty)
cty$cty.area <- as.numeric(cty$cty.area) 
 

##### PA Area #####
# need to bring both of these datasets in 
wy.desig <- st_read("data/original/PADUS2_1_StateWY_Shapefile/PADUS2_1Designation_StateWY.shp") %>% 
  st_make_valid() %>% 
  st_transform(., st_crs(r))
wy.proc <- st_read("data/original/PADUS2_1_StateWY_Shapefile/PADUS2_1Proclamation_StateWY.shp") %>% 
  st_make_valid() %>% 
  st_transform(., st_crs(r))
wy.proc <- wy.proc[!(wy.proc$Des_Tp=="TRIBL" ),]

# joining to compare with just one of them to find out where they don't overlap
wy.join <- bind_rows(wy.proc, wy.desig)

diffPoly.wy <- st_difference(wy.join, st_union(wy.proc))

# join the areas that don't overlap with one of the PADUS datasets
wy.join.new <- bind_rows(diffPoly.wy, wy.proc)
wy.pas <- wy.join.new


#get the total area of protected acres for each county
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

# fix any NAs
pa.cty.area$NAME <- toupper(pa.cty.area$NAME)
pa.cty.area$PAarea <- as.numeric(pa.cty.area$PAarea)
pa.cty.area$PAarea[is.na(pa.cty.area$PAarea)] = 0


# make a new column for the total number of non protected land area
pa.cty.area <- mutate(pa.cty.area, nonPAarea = cty.area - as.numeric(PAarea))
head(pa.cty.area)

##### Parcel Density #####
#bring in wy parcel data
wy.parcels <- st_read("data/original/Wyoming_Parcels/Wyoming_Parcels.shp") %>% 
  st_make_valid() %>% 
  st_transform(.,st_crs(r))
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

wy <- raster("data/processed/wy_parcel_density.tif")

