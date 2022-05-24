library(raster)
library(gdistance)
library(rgdal)
library(sf)
library(magrittr)
library(ggplot2)
library(purrr)
library(dplyr)
library(rasterVis)
library(patchwork)
library(classInt)
library(cowplot)
library(ggsci)
library(stringr)
library(egg)

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

# Load the data -----------------------------------------------------------
d <- readRDS("data/processed/TransitionLayers/ms_tree.rds")
biophys.tr <- readRDS("data/processed/TransitionLayers/biophystrans.rds")
social.tr1 <- readRDS(here::here('Data/Processed/TransitionLayers/socialtrans1.rds'))
pts <- st_read("data/processed/herd_centroids.shp") %>% 
  st_centroid(.) %>% 
  as(. , "Spatial")

# Calculate the costs of the MST ------------------------------------------
# run this function for the environment -- it is inputted into lapply below
mst_cost <- function(pathset, basetr, startpt){
  mn_func <- function(x){mean(x, na.rm=TRUE)}
  cost.mask <- mask(raster(basetr), pathset)
  cost.trans <- transition(cost.mask, transitionFunction = mn_func, 16)
  acost <- accCost(cost.trans, pts[pts@data$NAME == startpt,])
}

bio.cost.list <- lapply(1:length(d[[1]]), function(x)mst_cost(pathset = d[[1]][[x]], basetr = biophys.tr, startpt = "Yellowstone National Park"))
plot(stack(bio.cost.list))

social.cost.list <- lapply(1:length(d[[1]]), function(x)mst_cost(pathset = d[[1]][[x]], basetr = social.tr1, startpt = "Yellowstone National Park"))
plot(stack(social.cost.list))

# extract values ----------------------------------------------------------
mst1 <- bio.cost.list[[1]]
mst1[is.infinite(mst1)] <- NA
mst1.sum <- cellStats(mst1, sum, na.rm = TRUE)
mst1.max <- cellStats(mst1, max, na.rm = TRUE)
mst1.mean <- cellStats(mst1, mean, na.rm = TRUE)
hist(mst1)
quant.mst1 <- quantile(mst1)

mst2 <- bio.cost.list[[2]]
mst2[is.infinite(mst2)] <- NA
mst2.sum <- cellStats(mst2, sum, na.rm = TRUE)
mst2.max <- cellStats(mst2, max, na.rm = TRUE)
mst2.mean <- cellStats(mst2, mean, na.rm = TRUE)
hist(mst2)
quant.mst2 <- quantile(mst2)

mst3 <- bio.cost.list[[3]]
mst3[is.infinite(mst3)] <- NA
mst3.sum <- cellStats(mst3, sum, na.rm = TRUE)
mst3.max <- cellStats(mst3, max, na.rm = TRUE)
mst3.mean <- cellStats(mst3, mean, na.rm = TRUE)
hist(mst3)
quant.mst3 <- quantile(mst3)


social.mst1 <- social.cost.list[[1]]
social.mst1[is.infinite(social.mst1)] <- NA
social.mst1.sum <- cellStats(social.mst1, sum, na.rm = TRUE)
social.mst1.max <- cellStats(social.mst1, max, na.rm = TRUE)
social.mst1.mean <- cellStats(social.mst1, mean, na.rm = TRUE)
hist(social.mst1)
quant.soc.mst1 <- (quantile(social.mst1))

social.mst2 <- social.cost.list[[2]]
social.mst2[is.infinite(social.mst2)] <- NA
social.mst2.sum <- cellStats(social.mst2, sum, na.rm = TRUE)
social.mst2.max <- cellStats(social.mst2, max, na.rm = TRUE)
social.mst2.mean <- cellStats(social.mst2, mean, na.rm = TRUE)
hist(social.mst2)
quant.soc.mst2 <- (quantile(social.mst2))

social.mst3 <- social.cost.list[[3]]
social.mst3[is.infinite(social.mst3)] <- NA
social.mst3.sum <- cellStats(social.mst3, sum, na.rm = TRUE)
social.mst3.max <- cellStats(social.mst3, max, na.rm = TRUE)
social.mst3.mean <- cellStats(social.mst3, mean, na.rm = TRUE)
hist(social.mst3)
quant.soc.mst3 <- quantile(social.mst3)

df <- data.frame (label = c(1, 2, 3),
                  biophys.cost.km = c(mst1.sum/(path1.area/1000), mst2.sum/(path2.area/1000), mst3.sum/(path3.area/1000)),
                  social.cost.km = c(social.mst1.sum/(path1.area/1000), social.mst2.sum/(path2.area/1000), social.mst3.sum/(path3.area/1000)),
                  biophys.max =c(mst1.max, mst2.max, mst2.mean),
                  social.max = c(social.mst1.max, social.mst2.max, social.mst3.max),
                  biophys.mean = c(mst1.mean, mst2.mean, mst3.mean),
                  social.mean = c(social.mst1.mean, social.mst2.mean, social.mst3.mean),
                  distance = c(path1.area/1000, path2.area/1000, path3.area/1000)
)

units(df$biophys.cost.km) <- NULL
units(df$social.cost.km) <- NULL
units(df$distance) <- NULL
df <- df %>% mutate(ratio= social.cost.km/biophys.cost.km)
df      


par(mfrow = c(2, 2))
colors = c("#990000","#FF6633", "#336300")

barplot(df$biophys.cost.km, main = "Biophys Cost Total / km", col = colors)
barplot(df$social.cost.km, main = "Social Cost Total / km", col = colors)
barplot(df$distance, main = "Distance (km)", col = colors)
plot(df$ratio, main = "Cost Ratio", col = colors)
barplot(df$biophys.max, main = "Max Biophys Cost", col = colors)
barplot(df$social.max, main = "Max Social Cost", col = colors)
barplot(df$biophys.mean, main = "Mean Biophys Cost", col = colors)
barplot(df$social.mean, main = "Mean Social Cost", col = colors)



quantiles <- data.frame(quant.mst1 = c(quant.mst1),
                        quant.mst2 = c(quant.mst2),
                        quant.mst3 = c(quant.mst3),
                        quant.soc.mst1 = c(quant.soc.mst1),
                        quant.soc.mst2 = c(quant.soc.mst2),
                        quant.soc.mst3 = c(quant.soc.mst3))

