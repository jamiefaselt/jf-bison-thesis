library(tigris)
library(ggplot2)
library(tidyverse)
library(sf)
library(sp)
library(raster)
library(terra)
library(dplyr)
library(rgdal)
library(gdalUtilities)
ensure_multipolygons <- function(X) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  st_write(X, tmp1)
  ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- st_read(tmp2)
  st_sf(st_drop_geometry(X), geom = st_geometry(Y))
}
# bring in hsi and temp raster
r <- raster("data/template_raster.tif")
#bring in counties
counties <- tigris::counties(state=c("WY", "MT"))
# make columns match to caddat
counties <- rename(counties, State.ANSI = STATEFP)
counties <- rename(counties, County.ANSI = COUNTYFP)
#counties$State.ANSI <- as.numeric(counties$State.ANSI)#don't do this; these should stay characters, it may not cause errors here but it will if there are leading 0's
#counties$County.ANSI <- as.numeric(counties$County.ANSI)
#wy.counties<-counties %>% filter(State.ANSI %in%  c("56"))
cty<-st_transform(counties,st_crs(r))
cty$cty.area <- st_area(cty)
cty$cty.area <- as.numeric(cty$cty.area) 


##### PA Area #####
# Function to fix PADUS geometries ----------------------------------------

# need to bring both of these datasets in 
wy.desig <- st_read("data/original/PADUS2_1_StateWY_Shapefile/PADUS2_1Designation_StateWY.shp") %>% 
  st_make_valid() 
wy.proc <- st_read("data/original/PADUS2_1_StateWY_Shapefile/PADUS2_1Proclamation_StateWY.shp") %>% 
  st_make_valid() 
mt.desig <- st_read("data/original/PADUS2_1_StateMT_Shapefile/PADUS2_1Designation_StateMT.shp") %>% 
  st_make_valid()
mt.proc <- st_read("data/original/PADUS2_1_StateMT_Shapefile/PADUS2_1Proclamation_StateMT.shp") %>% 
  st_make_valid()

#drop proclamation polys that can contain parcelable land
procs <- bind_rows(wy.proc, mt.proc) %>% 
  filter(., Des_Tp != "TRIBL" | is.na(Des_Tp)) %>% 
  filter(., Loc_Ds != "FSA" | is.na(Loc_Ds)) %>% 
  filter(., Loc_Ds != "WMD" | is.na(Loc_Ds))

# no polys in des that could include parcealable land so not dropping any
des <- bind_rows(wy.desig, mt.desig)



int <- st_intersection(des, procs) #get intersection of procs with des
int.b <- st_buffer(int, 0)
dif <- st_difference(des, st_union(st_geometry(int.b)), s2_snap_distance(400)) #remove that intersection from des.
dif.v <- st_make_valid(dif)
#join back to the procs dataset
pas <- bind_rows(procs, dif.v)

pas <- st_union(pas, by_feature = TRUE) %>%  #clean up geomtries
  st_transform(.,st_crs(r))
pas.b <- st_buffer(pas, dist=0) #fix some weird geometries that come from the st_dif





#get the total area of protected acres for each county
s12 = lapply(1:nrow(cty), function(i){st_intersection(cty[i,],pas.b)})

pa.area <- lapply(1:length(s12), function(x){
  s12[[x]] %>% 
    group_by(GEOID) %>% 
    summarise(., PAarea = sum(st_area(.))) %>% 
    st_drop_geometry(.)
})
pas.comb <- do.call(rbind, pa.area) #missing 3 counties need to make sure these are counties without public land (Dawson, Treasure, and Daniels Cty MT) This seems correct

# join back to geometries from the county data
pa.cty.area <- cty %>% 
  left_join(., pas.comb, by = "GEOID") 

# fix any NAs
#pa.cty.area$NAME <- toupper(pa.cty.area$NAME)
pa.cty.area$PAarea <- as.numeric(pa.cty.area$PAarea)
pa.cty.area$PAarea[is.na(pa.cty.area$PAarea)] <- 0


# make a new column for the total number of non protected land area
pa.cty.area <- mutate(pa.cty.area, nonPAarea = cty.area - PAarea)


##### Parcel Density #####
#bring in wy parcel data
wy.parcels <- st_read("data/original/Wyoming_Parcels/Wyoming_Parcels.shp") %>% 
  st_make_valid() %>% 
  st_transform(.,st_crs(r))
mt.parcels <-st_read("Data/original/Montana_Cadastral/OWNERPARCEL.shp")

# Need to fix geometry of MT parcels --------------------------------------

mt.parcels.corrected <-  ensure_multipolygons(mt.parcels)
a <- which(is.na(st_is_valid(mt.parcels.corrected)))
mt.parcels.fail <- mt.parcels.corrected[a,]

#figure out how many pts is in each feature that throws an NA in st_is_valid
wrong.pts <- lapply(1:nrow(mt.parcels.fail), function(x) sapply(st_geometry(mt.parcels.fail)[[x]], function(y) nrow(y[[1]])))
#make sure the number of pts is the reason
few.pts <- lapply(1:length(wrong.pts), function(x) any(wrong.pts[[x]] < 4))

g <- st_geometry(mt.parcels.corrected)
g[[a]][1] = NULL #this is the feature with less than 4 pts
st_geometry(mt.parcels.corrected) = g
mt.parcels.valid <- st_make_valid(mt.parcels.corrected)
wy.parcel.join <- wy.parcels %>% 
  dplyr::select(. , parcelnb, jurisdicti, geometry) %>% 
  rename(ID = parcelnb, county = jurisdicti)

mt.parcel.join <- mt.parcels.valid %>% 
  st_transform(st_crs(r)) %>% 
  dplyr::select(., PARCELID, CountyName, geom) %>% 
  rename(ID = PARCELID, county = CountyName, geometry = geom)

parcels <- bind_rows(mt.parcel.join, wy.parcel.join) %>% st_buffer(., 0)
#using the spatial version of parcels in the event that parcels span multiple counties
parcel.int = lapply(1:nrow(cty), function(i){st_intersection(cty[i,],parcels)})

parcel.num <- lapply(1:length(parcel.int), function(x){
  parcel.int[[x]] %>% 
    group_by(GEOID) %>% 
    summarise(., parcelNum = n()) %>% 
    st_drop_geometry(.)
})
parcel.num.comb <- do.call(rbind, parcel.num) #

county.pa.parcels <- left_join(pa.cty.area, parcel.num.comb, by="GEOID") %>% 
  mutate(., parceldens = parcelNum/((nonPAarea/(10000*100))))

cty.parcel.rst <- terra::rasterize(vect(county.pa.parcels), rast(r), field = "parceldens")


writeRaster(cty.parcel.rst, "data/processed/parcel_dens_update.tif", overwrite = TRUE)


