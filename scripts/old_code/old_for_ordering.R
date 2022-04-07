# old code for oderign

# get the top ten values, take the centroids and add coordinate columns to use later
hab.order<- df %>%   
  arrange(desc(hsi_resample)) %>% 
  slice(1:5)
hab.simp <- st_simplify(hab.order, preserveTopology = FALSE, dTolerance = 1000)
hab.centroids <- st_centroid(hab.simp) %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])

tribal.order<- df %>%   
  arrange(desc(tribal_wildlife_gov_tract)) %>% 
  slice(1:10)
tribal.order
tribal.simp <- st_simplify(tribal.order)
tribal.centroids <- st_centroid(tribal.simp) %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])

social.order<- df %>%   
  arrange(desc(bis_inc)) %>% 
  slice(1:10)
social.simp <- st_simplify(social.order)
social.centroids <- st_centroid(social.simp) %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])

composite.order<- df %>%   
  arrange((social_composite_layer)) %>% 
  slice(1:10)
composite.simp <- st_simplify(composite.order, preserveTopology = FALSE, dTolerance = 1000)
composite.centroid <- st_centroid(composite.simp) %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])

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

#social
social <- ggplot()+
  geom_sf(data=social.simp, color = "green")+
  geom_sf(data=herds, color = "black")
social + geom_text_repel(data = social.centroids, aes(lon, lat, label = Unit_Nm), size = 3,  max.overlaps = Inf)

#tribal preference
tribal <- ggplot()+
  geom_sf(data=tribal.simp, color = "purple")+
  geom_sf(data=herds, color = "black")
tribal + geom_text_repel(data = tribal.centroids, aes(lon, lat, label = Unit_Nm), size = 3)

#social composite 
composite <- ggplot()+
  geom_sf(data=composite.simp, color = "orange")+
  geom_sf(data=herds, color = "black")
composite + geom_text_repel(data = composite.centroid, aes(lon, lat, label = Unit_Nm), size = 3)
composite.simp

ggplot()+
  geom_sf(data=herds, color = "black")+
  geom_sf(data=rmf, color = "orange")+
  geom_sf(data=composite.simp[4,], color = "blue") #Rocky Mountain Front Conservation Management Area

bob <- st_centroid(hab.simp[5,])
selway <- st_centroid(hab.simp[4,])
absaroka <- st_centroid(composite.simp[10,])
upperbreaks <- st_centroid(hab.simp[2,])

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