library(raster)
library(gdistance)
library(rgdal)
library(sf)
library(magrittr)
library(ggplot2)
library(purrr)
library(dplyr)
library(viridis)
# Calculate the length of each LCC ----------------------------------------

biophys <- readRDS(here::here('Data/Processed/TransitionLayers/ms_tree.rds'))
biophys.lst <- biophys[[1]]

path1 <- biophys.lst[[1]] %>% 
  rasterToPolygons(., na.rm = TRUE, dissolve=TRUE) %>% 
  st_as_sf(.)
path1.distance <- st_area(path1)/4000

path2<- biophys.lst[[2]] %>% 
  rasterToPolygons(., na.rm = TRUE, dissolve = TRUE) %>% 
  st_as_sf(.)
path2.distance <- st_area(path2)/4000

path3 <- biophys.lst[[3]] %>% 
  rasterToPolygons(., na.rm = TRUE, dissolve = TRUE) %>% 
  st_as_sf(.)
path3.distance <- st_area(path3)/4000

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
  mn_func <- function(x){median(x, na.rm=TRUE)}
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
mst1.median <- cellStats(mst1, median, na.rm = TRUE)
hist(mst1)
quant.mst1 <- quantile(mst1)

mst2 <- bio.cost.list[[2]]
mst2[is.infinite(mst2)] <- NA
mst2.sum <- cellStats(mst2, sum, na.rm = TRUE)
mst2.max <- cellStats(mst2, max, na.rm = TRUE)
mst2.median <- cellStats(mst2, median, na.rm = TRUE)
hist(mst2)
quant.mst2 <- quantile(mst2)

mst3 <- bio.cost.list[[3]]
mst3[is.infinite(mst3)] <- NA
mst3.sum <- cellStats(mst3, sum, na.rm = TRUE)
mst3.max <- cellStats(mst3, max, na.rm = TRUE)
mst3.median <- cellStats(mst3, median, na.rm = TRUE)
hist(mst3)
quant.mst3 <- quantile(mst3)


social.mst1 <- social.cost.list[[1]]
social.mst1[is.infinite(social.mst1)] <- NA
social.mst1.sum <- cellStats(social.mst1, sum, na.rm = TRUE)
social.mst1.max <- cellStats(social.mst1, max, na.rm = TRUE)
social.mst1.median <- cellStats(social.mst1, median, na.rm = TRUE)
hist(social.mst1)
quant.soc.mst1 <- (quantile(social.mst1))

social.mst2 <- social.cost.list[[2]]
social.mst2[is.infinite(social.mst2)] <- NA
social.mst2.sum <- cellStats(social.mst2, sum, na.rm = TRUE)
social.mst2.max <- cellStats(social.mst2, max, na.rm = TRUE)
social.mst2.median <- cellStats(social.mst2, median, na.rm = TRUE)
hist(social.mst2)
quant.soc.mst2 <- (quantile(social.mst2))

social.mst3 <- social.cost.list[[3]]
social.mst3[is.infinite(social.mst3)] <- NA
social.mst3.sum <- cellStats(social.mst3, sum, na.rm = TRUE)
social.mst3.max <- cellStats(social.mst3, max, na.rm = TRUE)
social.mst3.median <- cellStats(social.mst3, median, na.rm = TRUE)
hist(social.mst3)
quant.soc.mst3 <- quantile(social.mst3)

df <- data.frame (label = c(1, 2, 3),
                  biophys.cost.km = c(mst1.sum/(path1.distance/1000), mst2.sum/(path2.distance/1000), mst3.sum/(path3.distance/1000)),
                  social.cost.km = c(social.mst1.sum/(path1.distance/1000), social.mst2.sum/(path2.distance/1000), social.mst3.sum/(path3.distance/1000)),
                  biophys.sum = c(mst1.sum, mst2.sum, mst3.sum),
                  social.sum = c(social.mst1.sum, social.mst2.sum, social.mst3.sum),
                  biophys.max =c(mst1.max, mst2.max, mst2.median),
                  social.max = c(social.mst1.max, social.mst2.max, social.mst3.max),
                  biophys.median = c(mst1.median, mst2.median, mst3.median),
                  social.median = c(social.mst1.median, social.mst2.median, social.mst3.median),
                  distance = c(path1.distance/1000, path2.distance/1000, path3.distance/1000)
)

units(df$biophys.cost.km) <- NULL
units(df$social.cost.km) <- NULL
units(df$distance) <- NULL
df <- df %>% mutate(ratio= social.cost.km/biophys.cost.km)
df      

m <- c("Path 1", "Path 2", "Path 3")

par(mfrow = c(3, 2))
colors = c("#CC3333","#FF9900", "#669900")

#barplot(df$biophys.cost.km, main = "Biophys Cost Total / km", col = colors, names.arg = m, ylab = "Cost")
#barplot(df$social.cost.km, main = "Social Cost Total / km", col = colors, names.arg = m, ylab = "Cost")
barplot(df$biophys.sum, main = "Cumulative Biophys Cost", col = colors, names.arg = m, ylab = "Cost", cex.names=1.5, cex.main=1.5)
barplot(df$social.sum, main = "Cumulative Implementation Cost", col = colors, names.arg = m, ylab = "Cost", cex.names=1.5, cex.main=1.5)
#barplot(df$distance, main = "Distance (km)", col = colors, names.arg = m, ylab = "Cost", cex.names=1.5, cex.main=1.5)
#plot(df$ratio, main = "Cost Ratio", col = colors, names.arg = m, ylab = "Cost", cex.names=1.5, cex.main=1.5)
barplot(df$biophys.max, main = "Max Biophys Cost", col = colors, names.arg = m, ylab = "Cost", cex.names=1.5, cex.main=1.5)
barplot(df$social.max, main = "Max Implementation Cost", col = colors, names.arg = m, ylab = "Cost", cex.names=1.5, cex.main=1.5)
barplot(df$biophys.median, main = "Median Biophys Cost", col = colors, names.arg = m, ylab = "Cost", cex.names=1.5, cex.main=1.5)
barplot(df$social.median, main = "Median Implementation Cost", col = colors, names.arg = m, ylab = "Cost", cex.names=1.5, cex.main=1.5)



quantiles <- data.frame(quant.mst1 = c(quant.mst1),
                        quant.mst2 = c(quant.mst2),
                        quant.mst3 = c(quant.mst3),
                        quant.soc.mst1 = c(quant.soc.mst1),
                        quant.soc.mst2 = c(quant.soc.mst2),
                        quant.soc.mst3 = c(quant.soc.mst3))


# Extract Biophys Current Flow --------------------------------------------


