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
biophys.lst <- ms_tree_nobuf[[1]]
path1 <- biophys.lst[[1]]


social1.lst <- social1[[1]]
#social2.lst <- social2[[1]]
biophys.lst <- biophys[[1]]

path1 <- biophys.lst[[1]] %>% 
  rasterToPolygons(., na.rm = TRUE)
path2<- biophys.lst[[2]] %>% 
  rasterToPolygons(., na.rm = TRUE)
path3 <- biophys.lst[[3]] %>% 
  rasterToPolygons(., na.rm = TRUE)


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

# calculate the distance

d <- readRDS("data/processed/TransitionLayers/ms_tree_nobuf.rds")
plot(ms_tree_nobuf)

mst_cost <- function(pathset, basetr, startpt){
  mn_func <- function(x){mean(x, na.rm=TRUE)}
  cost.mask <- mask(raster(basetr), pathset)
  cost.trans <- transition(cost.mask, transitionFunction = mn_func, 16)
  acost <- accCost(cost.trans, pts[pts@data$NAME == startpt,])
}

bio.cost.list <- lapply(1:length(ms_tree_nobuf[[1]]), function(x)mst_cost(pathset = d[[1]][[x]], basetr = biophys.tr, startpt = "Yellowstone National Park"))

path1.dist <- bio.cost.list[[1]] %>% 
  rasterToPolygons(., na.rm = TRUE) %>% 
  st_as_sf(.) #%>% 
  #st_length(.)
dist1 <- st_length(path1.dist)

path2.dist <- bio.cost.list[[2]] %>% 
  rasterToPolygons(., na.rm = TRUE) %>% 
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
