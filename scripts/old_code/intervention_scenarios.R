# intervention scenarios
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

herds <- st_read("data/processed/all_nodes.tif")

#to compare later
social.composite <- raster("data/raster_layers/social_composite_layer.tif") #or?
social.resistance <- raster("data/raster_layers/social_resistance_layer.tif")
r <- raster("data/template_raster.tif")

# Economic Incentive ------------------------------------------------------
econ.incentive <- raster("data/raster_layers/econ_incentive_layer.tif")
# going to subtract this from cattle layer- do another fuzzy sum and convert back to resistance
bis.dec <- raster("data/raster_layers/bison_decrease_layer.tif") 
cattle.sales <- raster("data/raster_layers/cattle_sales_layer.tif")
econ.intervention <- cattle.sales-econ.incentive
repub <- raster("data/raster_layers/repub_vote_layer.tif")
landval.pnas <- raster("data/raster_layers/landval_layer.tif")
parceldensity <- raster("data/raster_layers/parcel_density_layer.tif")

# fuzzy sum approach
rc1.1m <- 1-bis.dec
rc2.1m <- 1-econ.intervention
rc3.1m <- 1-repub
rc4.1m <- 
rc5.1m <- 1-parceldensity

fuz.sum <- 1-(rc1.1m*rc2.1m*rc3.1m*rc5.1m)
plot(fuz.sum) 

#make resistance
econ.scenario <- ((1+fuz.sum)^10 + landval.pnas/4)
plot(econ.scenario)
plot(st_geometry(herds), add= TRUE)
writeRaster(econ.scenario, "data/raster_layers/econ_scenario_rl.tif", overwrite = TRUE)


# Tribal Governance Scenario ----------------------------------------------
tribal.wildlife <- raster("data/raster_layers/tribal_wildlife_gov_tract.tif") 
plot(tribal.wildlife)
# need to think through this more philosophically
tribal.scenario <- social.composite-tribal.wildlife
plot(tribal.scenario)
tribal.resistance.scenario <- ((1+tribal.scenario)^10 + landval.pnas/4) %>% 
  rescale01(.)
tribal.resistance.scenario[tribal.resistance.scenario==0] <- .0001
writeRaster(tribal.resistance.scenario, "data/raster_layers/tribal_scenario.tif", overwrite = TRUE)
plot(tribal.resistance.scenario)
plot(st_geometry(herds), add= TRUE)

plot(social.resistance)
plot(st_geometry(herds), add= TRUE)

# Public Lands Scenario ---------------------------------------------------

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
reproj <- st_transform(mt.padus, crs = st_crs(r))
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

#want to compare with social composite
comp <- raster("data/raster_layers/social_composite_layer.tif")

# extract data
large.pas$ID <- seq(1, nrow(large.pas))

bio.extract <- raster::extract(biophys, large.pas, fun = mean, na.rm = TRUE, df = TRUE)
social.extract <- raster::extract(social, large.pas, fun = mean, na.rm = TRUE, df = TRUE)

#join into one df
join <- left_join(large.pas, bio.extract) %>% 
  left_join(., social.extract)

#view and subset for simplification
head(join)
df <- subset(join, select = c(Unit_Nm, ID, social_resistance_layer, hsi_resample,  d_Des_Tp, geometry))

st_write(df, "data/processed/pa_bio_social_extract.shp")

#find the upper 75% of habitat suitability
hab.qual<-quantile(df$hsi_resample,probs=c(0.25, 0.5,0.75, .9))
hab.qual

hab.75 <- df %>% 
  filter(., hsi_resample > 24.6 )

#find the lowest 50% of social composite
resistance<-quantile(hab.75$social_resistance_layer,probs=c(0,0.25,0.5,0.75, .9))
resistance
composite <- hab.75 %>% 
  filter(., social_resistance_layer < 672.3 ) 
composite <- st_transform(composite, st_crs(r))

r <- raster("data/template_raster.tif")
states <- tigris::states()
mt <- states %>% filter(., NAME=="Montana", drop=TRUE) %>% st_transform(., st_crs(r))

plot(st_geometry(mt))
plot(st_geometry(composite), add = TRUE, col = "red", outline = "red")
plot(st_geometry(herds), add = TRUE)

st_write(composite, "data/processed/publand_shortcircuit.shp", append = FALSE)

centroids <- st_centroid(composite)
plot(centroids)
plot(st_geometry(mt.counties))
plot(st_geometry(centroids), add = TRUE)

st_write(composite, "data/processed/new_pa_herds.shp")


