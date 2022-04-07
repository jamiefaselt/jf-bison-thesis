library(raster)
library(dplyr)
library(sf)
library(cowplot)
library(ggplot2)
library(viridis)

r <- raster("data/template_raster.tif")
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
  filter(., GIS_Acres > 25000) 
#plot(st_geometry(large.pas))

# bring in the social layer
bison.inc <- raster("data/raster_layers/bis_inc.tif")
plot(bison.inc)
bison.inc

# check out population estimate for bison increase
bis.inc.pop <- raster("data/raster_layers/bis_inc_pop_est.tif")

#bring in habitat suitability
hab <- raster("data/processed/hsi_resample.tif")

#bring in tribal governance preferences
tribal.gov <- raster("data/raster_layers/tribal_wildlife_gov_tract.tif") %>% 
  resample(., r)

#want to compare with social composite
comp <- raster("data/raster_layers/social_composite_layer.tif")

# extract data
large.pas$ID <- seq(1, nrow(large.pas))
extract <- raster::extract(bison.inc, large.pas, buffer = 50000, fun = mean, na.rm = TRUE, df = TRUE)
extract2 <- raster::extract(hab, large.pas, fun = mean, na.rm = TRUE, df = TRUE)
extract3 <- raster::extract(tribal.gov, large.pas, buffer = 50000, fun = mean, na.rm = TRUE, df = TRUE)
extract4 <- raster::extract(comp, large.pas, buffer =50000, fun = mean, na.rm = TRUE, df = TRUE)

#join into one df
join <- left_join(large.pas, extract) %>% 
  left_join(., extract2) %>% 
  left_join(., extract3) %>% 
  left_join(., extract4)

#view and subset for simplification
head(join)
df <- subset(join, select = c(Unit_Nm, ID, bis_inc, social_composite_layer, hsi_resample, tribal_wildlife_gov_tract, d_Des_Tp,  geometry))

#find the upper 75% of habitat suitabillity
hab.qual<-quantile(df$hsi_resample,probs=c(0.5,0.75, .8))
hab.qual
# 75% = 39.61886 
hab.90 <- df %>% 
  filter(., hsi_resample > 40.82832 )

#find the lowest 25% of social composite
resistance<-quantile(df$social_composite_layer,probs=c(0,0.25,0.5,0.75))
resistance
composite <- hab.90 %>% 
  filter(., social_composite_layer < 0.8623746 ) %>% 
  filter(., hsi_resample>42.3)

centroids <- st_centroid(composite)
plot(centroids)
plot(st_geometry(mt.counties))
plot(st_geometry(centroids), add = TRUE)

st_write(composite, "data/processed/new_pa_herds.shp")

# bring in all herds
herds <- st_read("data/processed/all_nodes_correct.shp")













#bring in herd shapefils for visual aid
nodes <- st_read("data/processed/all_nodes_correct.shp")
plot(st_geometry(nodes), add = TRUE)
herds <- st_read("data/processed/herd_shapefile_outline.shp") %>% 
  st_simplify(.)


# plot to see where the overlaps aren for the three categories
hab <- ggplot()+
  geom_sf(data=hab.simp, color = "blue")+
  geom_sf(data=herds, color = "black")
hab + geom_text_repel(data = hab.centroids, aes(lon, lat, label = Unit_Nm), size = 3)


new_data <- df%>% 
  filter(Unit_Nm == "Bob Marshall Wilderness" | Unit_Nm == "Absaroka-Beartooth Wilderness"| Unit_Nm == "Selway-Bitterroot Wilderness" | Unit_Nm == "Upper Missouri River Breaks National Monument"
)

new_data$ID <- seq(1, nrow(new_data))
hab.centroids <- new_data %>% 
  st_centroid(.) %>% 
  st_buffer(., 10000) %>% 
  st_as_sf(.)# buffer of 10 km        
plot(hab.centroids)
newnode.rast<-fasterize::fasterize(hab.centroids, r, field = 'ID')
plot(newnode.rast)
writeRaster(newnode.rast, "data/processed/new_nodes.tif", overwrite = TRUE)
