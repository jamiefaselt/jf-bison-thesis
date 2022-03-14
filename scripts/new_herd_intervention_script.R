library(raster)
library(terra)
library(dplyr)
library(sf)
library(cowplot)
library(ggplot2)
library(viridis)

r <- raster("data/template_raster.tif")
#load the PADUS
mt.padus <- st_read("data/original/PADUS2_1_StateMT_Shapefile/PADUS2_1Designation_StateMT.shp") 
plot(st_geometry(mt.padus))
st_make_valid(mt.padus)
st_is_valid(mt.padus)
head(mt.padus)

# reproject
reproj <- st_transform(mt.padus, crs = st_crs(r))
plot(st_geometry(reproj))
head(reproj)

# filter and buffer
large.pas <- mt.padus %>% 
  filter(., GIS_Acres > 100000) 
plot(st_geometry(large.pas))

# bring in the social layer
bison.inc <- raster("data/raster_layers/bis_inc.tif")
plot(bison.inc)
bison.inc

#bring in habitat suitability
hab <- raster("data/processed/hsi_resample.tif")

#bring in tribal governance preferences
tribal.gov <- raster("data/raster_layers/tribal_wildlife_gov_tract.tif") %>% 
  resample(., r)

# extract data
large.pas$ID <- seq(1, nrow(large.pas))
extract <- raster::extract(bison.inc, large.pas, buffer = 50000, fun = mean, na.rm = TRUE, df = TRUE)
extract2 <- raster::extract(hab, large.pas, buffer = 50000, fun = mean, na.rm = TRUE, df = TRUE)
extract3 <- raster::extract(tribal.gov, large.pas, buffer = 50000, fun = mean, na.rm = TRUE, df = TRUE)

join <- left_join(large.pas, extract)
join <- left_join(join, extract2)
join <- left_join(join, extract3)
head(join)
df <- subset(join, select = c(Unit_Nm, ID, bis_inc, hsi_resample, tribal_wildlife_gov_tract, d_Des_Tp, geometry))

top <- Reduce(rbind,                                 # Top N highest values by group
                    by(df$hsi_resample,
                       df["Unit_Nm"],
                       head,
                       n = 5))
top    

which(summary.df$rich.mean == max(summary.df$rich.mean))
summary.df$Unit_Nm.x[698]