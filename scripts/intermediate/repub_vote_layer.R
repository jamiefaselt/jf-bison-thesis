# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ

library(sf)
library(tidyverse)
library(tidycensus)
library(tmap)
library(tigris)
library(car)
library(rgdal)
library(raster)
library(dplyr)

votes2000.2020 <- read_csv("data/original/countypres_2000-2020.csv")
colnames(votes2000.2020)
head(votes2000.2020)
counties <- tigris::counties()
wy.counties<-counties %>% filter(STATEFP %in%  c("56"))
r <- raster("Data/template_raster.tif")
wy.counties<-st_transform(wy.counties,st_crs(r))
wy.counties$NAME <- toupper(wy.counties$NAME)
wy.counties <- rename(wy.counties, "county_name" = "NAME")

votes.wy <- votes2000.2020 %>% filter(state %in% c("WYOMING"))
votes.wy <- votes.wy %>% filter(party %in% c("REPUBLICAN"))
wy.rep <- votes.wy %>% mutate(., percentrepub= (candidatevotes/totalvotes)*100)
wy.rep.avg <- wy.rep %>%
  group_by(., county_name) %>%
  summarise(avgrep = mean(percentrepub))
wy.rep.avg.join <- left_join(wy.counties, wy.rep.avg) 
wy.rep.avg.join <- st_as_sf(wy.rep.avg.join)
wy.rep.avg.rast<-fasterize::fasterize(wy.rep.avg.join, r, field = 'avgrep')
plot(wy.rep.avg.rast)



####################################################
# Montana voting
mt.counties<-counties %>% filter(STATEFP %in%  c("30"))
r <- raster("Data/template_raster.tif")
mt.counties<-st_transform(mt.counties,st_crs(r))
mt.counties$NAME <- toupper(mt.counties$NAME)
mt.counties <- rename(mt.counties, "county_name" = "NAME")

votes.mt <- votes2000.2020 %>% filter(state %in% c("MONTANA"))
votes.mt <- votes.mt %>% filter(party %in% c("REPUBLICAN"))
mt.rep <- votes.mt %>% mutate(., percentrepub= (candidatevotes/totalvotes)*100)
mt.rep.avg <- mt.rep %>%
  group_by(., county_name) %>%
  summarise(avgrep = mean(percentrepub))
mt.rep.avg.join <- left_join(mt.counties, mt.rep.avg) 
mt.rep.avg.join <- st_as_sf(mt.rep.avg.join)
mt.rep.avg.rast<-fasterize::fasterize(mt.rep.avg.join, r, field = 'avgrep')
plot(mt.rep.avg.rast)

##############################################################################
# merge, rescale and save
mt.rep.avg.rast
wy.rep.avg.rast
mtwy.republican <- merge(mt.rep.avg.rast, wy.rep.avg.rast)
plot(mtwy.republican)
rescale <- rescale01(mtwy.republican)
plot(rescale)
writeRaster(rescale, "data/raster_layers/repub_vote_layer.tif")
##############################################################################
#let's see if 2020 is different than the average 
wy.2020 <- votes.wy %>% filter(year %in% c("2020"))
wy.2020.rep <- wy.2020 %>% filter(party %in% c("REPUBLICAN")) %>% 
  mutate(., percentrepub = (candidatevotes/totalvotes)*100)
wy.2020.rep.spat <- left_join(wy.counties, wy.2020.rep) %>% 
  st_as_sf()
wy.2020.rast<-fasterize::fasterize(wy.2020.rep.spat, r, field = 'percentrepub')
plot(wy.2020.rast)

wy.diff <- wy.2020.rast - wy.rep.avg.rast
plot(wy.diff)
#let's see if 2020 is different than the average 
mt.2020 <- votes.mt %>% filter(year %in% c("2020"))
mt.2020.rep <- mt.2020 %>% filter(party %in% c("REPUBLICAN")) %>% 
  mutate(., percentrepub = (candidatevotes/totalvotes)*100)
mt.2020.rep.spat <- left_join(mt.counties, mt.2020.rep) %>% 
  st_as_sf()
mt.2020.rast<-fasterize::fasterize(mt.2020.rep.spat, r, field = 'percentrepub')
plot(mt.2020.rast)

mt.diff <- mt.2020.rast - mt.rep.avg.rast
plot(mt.diff)



##############################################
# curious to loook at us as a whole
states <- tigris::states()
#<-terra::rast( xmin=-6500000, xmax= 3500000, ymin=-3000000, ymax=5000000,crs='ESRI:102008', resolution=5000, vals=0)
#r1 <- raster(crs= albers, ext=raster::extent(as(counties, "Spatial")), resolution= 5000)
#r1 <- raster(crs='ESRI:102008', ext=raster::extent(as(counties, "Spatial")), resolution= 1000)
albers<- st_crs('ESRI:102008') 

r2d2 <- raster(crs= 'ESRI:102008', ext=raster::extent(as(states, "Spatial")), resolution= 90)
extent(states)
plot(states)

votes2000.2020 <- read_csv("Data/countypres_2000-2020.csv")
colnames(votes2000.2020)
head(votes2000.2020)
counties <- tigris::counties()

counties$NAME <- toupper(counties$NAME)
counties <- rename(counties, "county_name" = "NAME")

votes <- votes2000.2020 %>% filter(party %in% c("REPUBLICAN")) %>% 
  mutate(., percentrepub= (candidatevotes/totalvotes)*100) %>% 
  group_by(., county_name) %>% 
  summarise(avgrep = mean(percentrepub))

votes.join <- left_join(counties, votes) %>% 
  st_as_sf()

votesavg<-fasterize::fasterize(votes.join, r2d2, field = 'avgrep')
plot(votesavg)

#let's see if 2020 is different than the average 
v.2020 <- votes %>% filter(year %in% c("2020"))
v.2020.rep <- v.2020 %>% filter(party %in% c("REPUBLICAN")) %>% 
  mutate(., percentrepub = (candidatevotes/totalvotes)*100)
v.2020.jn <- left_join(counties, v.2020.rep) %>% 
  st_as_sf()
v.2020.rast<-fasterize::fasterize(v.2020.jn, r1, field = 'percentrepub')
plot(v.2020.rast)

wy.diff <- wy.2020.rast - wy.rep.avg.rast
plot(wy.diff)