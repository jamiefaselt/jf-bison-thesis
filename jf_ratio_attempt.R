library(raster)
library(gdistance)
library(rgdal)
library(sf)
library(magrittr)
library(ggplot2)
library(rasterVis)
library(patchwork)
library(classInt)
#library(cowplot)
library(ggsci)
library(stringr)
library(egg)

# Load transition layers --------------------------------------------------
social.tr1 <- readRDS(here::here('data/Processed/TransitionLayers/socialtrans1.rds'))
biophys.tr <- readRDS(here::here('data/Processed/TransitionLayers/biophystrans.rds'))
# Load Resistance surfaces ------------------------------------------------
biophys.resist <- raster(here::here("data/raster_layers/biophys_resistance_layer.tif"))
biophys.resist[is.na(biophys.resist[])] <- 2* cellStats(biophys.resist, max)## drop NAs for costodistance
implementation.resist1 <- raster(here::here("data/raster_layers/social_resistance_layer.tif"))
implementation.resist1[is.na(implementation.resist1[])] <- 5* cellStats(implementation.resist1, max)## drop NAs for costodistance


social1 <- readRDS(here::here('Data/Processed/TransitionLayers/social_ms_tree.rds'))
biophys <- readRDS(here::here('Data/Processed/TransitionLayers/ms_tree.rds'))
biophys.lst <- biophys[[1]]
# Calculate the length of each LCC ----------------------------------------

biophys <- readRDS(here::here('Data/Processed/TransitionLayers/ms_tree.rds'))
biophys.lst <- biophys[[1]]

path1 <- biophys.lst[[1]] %>% 
  rasterToPolygons(., na.rm = TRUE, dissolve=TRUE) %>% 
  st_as_sf(.)
path1.area <- st_area(path1)/4000

path2<- biophys.lst[[2]] %>% 
  rasterToPolygons(., na.rm = TRUE, dissolve = TRUE) %>% 
  st_as_sf(.)
path2.area <- st_area(path2)/4000

path3 <- biophys.lst[[3]] %>% 
  rasterToPolygons(., na.rm = TRUE, dissolve = TRUE) %>% 
  st_as_sf(.)
path3.area <- st_area(path3)/4000

social1.lst <- social1[[1]]
#social2.lst <- social2[[1]]

path1 <- biophys.lst[[1]] %>% 
  rasterToPolygons(., na.rm = TRUE)
plot(path1)
path2<- biophys.lst[[2]] %>% 
  rasterToPolygons(., na.rm = TRUE)
plot(path2)
path3 <- biophys.lst[[3]] %>% 
  rasterToPolygons(., na.rm = TRUE)
plot(path3)


# social extract ----------------------------------------------------------
path1.extract <- raster::extract(implementation.resist1, path1)
df <- data.frame(matrix(unlist(path1.extract), nrow=length(path1.extract), byrow=TRUE))
path1.acc <- sum(df$matrix.unlist.path1.extract...nrow...length.path1.extract...byrow...TRUE.)

path2.extract <- raster::extract(implementation.resist1, path2)
df2 <- data.frame(matrix(unlist(path2.extract), nrow=length(path2.extract), byrow=TRUE))
path2.acc <- sum(df2$matrix.unlist.path2.extract...nrow...length.path2.extract...byrow...TRUE.)

path3.extract <- raster::extract(implementation.resist1, path3)
df3 <- data.frame(matrix(unlist(path3.extract), nrow=length(path3.extract), byrow=TRUE))
path3.acc <- sum(df3$matrix.unlist.path3.extract...nrow...length.path3.extract...byrow...TRUE.)

# bio extract -------------------------------------------------------------
bio1.extract <- raster::extract(biophys.resist, path1)
df <- data.frame(matrix(unlist(bio1.extract), nrow=length(bio1.extract), byrow=TRUE))
bio1.acc <- sum(df$matrix.unlist.bio1.extract...nrow...length.bio1.extract...byrow...TRUE.)

bio2.extract <- raster::extract(biophys.resist, path2)
df2 <- data.frame(matrix(unlist(bio2.extract), nrow=length(bio2.extract), byrow=TRUE))
bio2.acc <- sum(df2$matrix.unlist.bio2.extract...nrow...length.bio2.extract...byrow...TRUE.)

bio3.extract <- raster::extract(biophys.resist, path3)
df3 <- data.frame(matrix(unlist(bio3.extract), nrow=length(bio3.extract), byrow=TRUE))
bio3.acc <- sum(df3$matrix.unlist.bio3.extract...nrow...length.bio3.extract...byrow...TRUE.)



# master dataframe for plotting -------------------------------------------
master.df <- data.frame(label = c(1, 2, 3),
                        biophys.cost.km = c(bio1.acc/(path1.area/1000), bio2.acc/(path2.area/1000), bio3.acc/(path3.area/1000)),
                        social.cost.km = c(path1.acc/(path1.area/1000), path2.acc/(path2.area/1000), path3.acc/(path3.area/1000)),
                         distance = c(path1.area/1000, path2.area/1000, path3.area/1000))
                        
par(mfrow = c(1, 2))
colors = c("#990000","#FF6633", "#336300")
units(master.df$biophys.cost.km) <- NULL
units(master.df$social.cost.km) <- NULL
units(master.df$distance) <- NULL

barplot(master.df$biophys.cost, main = "Biophys Cost / km", col = colors)
barplot(master.df$social.cost, main23aesd = "Social Cost / km", col = colors)
# calculate the distance

d <- readRDS("data/processed/TransitionLayers/ms_tree.rds")
plot(ms_tree)

mst_cost <- function(pathset, basetr, startpt){
  mn_func <- function(x){mean(x, na.rm=TRUE)}
  cost.mask <- mask(raster(basetr), pathset)
  cost.trans <- transition(cost.mask, transitionFunction = mn_func, 16)
  acost <- accCost(cost.trans, pts[pts@data$NAME == startpt,])
}

bio.cost.list <- lapply(1:length(ms_tree[[1]]), function(x)mst_cost(pathset = d[[1]][[x]], basetr = biophys.tr, startpt = "Yellowstone National Park"))

path1.dist <- bio.cost.list[[1]] %>% 
  rasterToPolygons(., na.rm = TRUE)# %>% 
  st_as_sf(.) #%>% 
  #st_length(.)
dist1 <- distance(path1.dist)

path2.dist <- bio.cost.list[[2]] %>% 
  rasterToPolygons(., na.rm = TRUE) #%>% 
  st_as_sf(.) #%>% 
 # st_length(.)
dist2 <- st_length(path2.dist)

path3.dist <- bio.cost.list[[3]] %>% 
  rasterToPolygons(., na.rm = TRUE) %>% 
  st_as_sf(.) #%>% 
  #st_length(.)
dist3 <- st_length(path3.dist)

#calculate the ratio
path1.ratio <- path1.acc/bio1.acc
path2.ratio <- path2.acc/bio2.acc
path3.ratio <- path3.acc/bio3.acc

plot(path1.ratio)
plot(path2.ratio, add = TRUE)
plot(path3.ratio)
