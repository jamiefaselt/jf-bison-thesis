library(raster)
library(dplyr)
library(sf)
library(cowplot)
library(ggplot2)
library(viridis)

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





