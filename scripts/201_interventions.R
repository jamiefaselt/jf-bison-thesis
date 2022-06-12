rescale01 <- function(r1) {
  r.rescale <- (r1 - cellStats(r1, min))/(cellStats(r1, max) - cellStats(r1, min))
}

library(raster)
library(terra)
library(dplyr)
library(sf)
library(ggmap)
library(rgdal)
library(maptools)

cattle <- raster("data/raster_layers/cattle_sales_layer.tif")
r <- raster("data/template_raster.tif") %>% 
  mask(., cattle)

herds <- raster("data/processed/all_nodes.tif")

# Economic Intervention Scenario ------------------------------------------
econ.data <- raster("data/wildlife_model_tifs/econ.map.tif") %>% 
  terra::resample(., r, mask = TRUE)
bison.inc <- raster("data/wildlife_model_tifs/bis.increase.map.tif") %>% 
  terra::resample(., r, mask = TRUE)
econ.intervention <- 1-(bison.inc+ econ.data)
cattle.sales <- raster("data/raster_layers/cattle_sales_layer.tif")
repub <- raster("data/raster_layers/repub_vote_layer.tif")
parceldensity <- raster("data/processed/parcel_dens_update.tif") %>% 
  terra::resample(., r) %>% 
  rescale01(.)
landval.pnas <- raster("data/raster_layers/landval_layer.tif")

# fuzzy sum approach
rc1.1m <- 1-econ.intervention
rc2.1m <- 1-repub
rc3.1m <- 1-parceldensity
rc4.1m <- 1-cattle.sales

fuz.sum <- 1-(rc1.1m*rc2.1m*rc3.1m*rc4.1m)
plot(fuz.sum) 

#make resistance
econ.scenario <- ((1+fuz.sum)^10 + landval.pnas/4)
plot(econ.scenario)
writeRaster(econ.scenario, "data/raster_layers/econ_scenario.tif", overwrite = TRUE)

# Tribal Governance Scenario ----------------------------------------------
tribal.wildlife <- raster("data/wildlife_model_tifs/tribal.wildlife.map.tif") %>% 
  terra::resample(., r)
bison.inc <- raster("data/wildlife_model_tifs/bis.increase.map.tif") %>% 
  terra::resample(., r)
tribal.scenario <- 1-(bison.inc+tribal.wildlife)
cattle.sales <- raster("data/raster_layers/cattle_sales_layer.tif")
repub <- raster("data/raster_layers/repub_vote_layer.tif")
parceldensity <- raster("data/processed/parcel_dens_update.tif") %>% 
  terra::resample(., r) %>% 
  rescale01(.)
landval.pnas <- raster("data/raster_layers/landval_layer.tif")

# fuzzy sum approach
rc1.1m <- 1-tribal.scenario
rc2.1m <- 1-repub
rc3.1m <- 1-parceldensity
rc4.1m <- 1-cattle.sales

fuz.sum <- 1-(rc1.1m*rc2.1m*rc3.1m*rc4.1m)
plot(fuz.sum) 

#make resistance
tribal.scenario <- ((1+fuz.sum)^10 + landval.pnas/4)
plot(tribal.scenario)
writeRaster(tribal.scenario, "data/raster_layers/tribal_scenario.tif", overwrite = TRUE)



# DOI Friendly Scenario ---------------------------------------------------

# template raster
r <- raster("data/template_raster.tif")

# herd locations
herds <- st_read("data/processed/herd_shapefile_outline.shp")

#load the PADUS
mt.padus <- st_read("data/original/PADUS2_1_StateMT_Shapefile/PADUS2_1Designation_StateMT.shp") 
#plot(st_geometry(mt.padus))
st_make_valid(mt.padus)
st_is_valid(mt.padus)
head(mt.padus)

# reproject
reproj <- st_transform(mt.padus, crs = st_crs(r)) %>% 
  st_make_valid(.)
#plot(st_geometry(reproj))
head(reproj)

# filter and buffer
large.pas <- reproj %>% 
  filter(., GIS_Acres > 50000) 
head(large.pas)

large.pas$ID <- seq(1, nrow(large.pas))


#bring in habitat suitability and social composite resistance layers
hab <- raster("data/processed/hsi_resample.tif")
social <- raster("data/raster_layers/social_resistance_layer.tif")

# extract data
large.pas$ID <- seq(1, nrow(large.pas))

bio.extract <- raster::extract(hab, large.pas, fun = mean, na.rm = TRUE, df = TRUE)
social.extract <- raster::extract(social, large.pas, fun = mean, buffer = 5000, na.rm = TRUE, df = TRUE)

#join into one df
join <- left_join(large.pas, bio.extract) %>% 
  left_join(., social.extract)

#view and subset for simplification
head(join)
df <- subset(join, select = c(Unit_Nm, ID, social_resistance_layer, hsi_resample,  d_Des_Tp, geometry))

# write in case you want to change the selection from the same extract 
st_write(df, "data/processed/pa_bio_social_extract.shp")

#find the upper 75% of habitat suitability
hab.qual<-quantile(df$hsi_resample,probs=c(0.25, 0.5,0.75, .9))
hab.qual

hab.75 <- df %>% 
  filter(., hsi_resample > 39.68)

#find the lowest 50% of social composite
resistance<-quantile(hab.75$social_resistance_layer,probs=c(0,0.25,0.5,0.75, .9))
resistance
composite <- hab.75 %>% 
  filter(., social_resistance_layer < 629.5 ) %>% 
  st_transform(., st_crs(r))


r <- raster("data/template_raster.tif")
states <- tigris::states()
mt <- states %>% filter(., NAME=="Montana", drop=TRUE) %>% st_transform(., st_crs(r))

st_write(composite, "data/processed/publand_shortcircuit.shp", append = FALSE)

new.herds <- st_read("data/processed/publand_shortcircuit.shp")
new.herds$ID <- seq(1, nrow(new.herds))

new.herds <- new.herds %>% 
  group_by(ID) %>%
  summarise(geometry = sf::st_union(geometry)) 


newherd.rast<-fasterize::fasterize(new.herds, r, field = 'ID')
plot(newherd.rast)
writeRaster(newherd.rast, "data/processed/shortcircuit.tif", overwrite= TRUE)

# the same composite fuzzy sum approach is used for a resistance layer but the shortcircuit file is added to the ini file for CS

# ini files to now run in circuitscape to use in next scripts
# /Users/jamiefaselt/jf-bison-thesis/ini_files/econ_scenario.ini
# /Users/jamiefaselt/jf-bison-thesis/ini_files/tribal_scenario.ini
# /Users/jamiefaselt/jf-bison-thesis/ini_files/newherd_shortcircuit.ini
# /Users/jamiefaselt/jf-bison-thesis/ini_files/null.ini