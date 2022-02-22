# APR/ Private Conservation Easements in MT
# https://mslservices.mt.gov/geographic_information/data/datalist/datalist_MetadataDetail.aspx?did=%7B2757ACE4-10F2-47E5-B3D6-C7C6A84011FD%7D

library(fasterize)
library(raster)
library(sp)
library(sf)
library(rgeos)
library(rgdal)
library(tidyverse)
library(tigris)
library(dplyr)

r <- raster("data/template_raster.tif")
states <- tigris::states()
mt <- states %>% filter(., NAME=="Montana", drop=TRUE)
counties <- tigris::counties()
mt.counties<-counties %>% filter(STATEFP %in%  c("30"))
mt.counties<-st_transform(mt.counties,st_crs(r))
all.nodes <- st_read("data/processed/all_nodes_correct.shp")
private <- st_read("/Users/jamiefaselt/Downloads/LandMan_PrvtCons/LandMan_PrvtCons.shp") %>% 
  st_as_sf(.) %>% 
  st_transform(.,st_crs(r))

apr <-filter(private, OwnerName=="American Prairie Reserve",  drop=TRUE) %>% 
  st_make_valid() %>% 
  st_union(., cmr) %>% 
  st_combine(.)
apr.node <- st_centroid(apr)

plot(st_geometry(mt.counties))
plot(st_geometry(apr), add = TRUE)
plot(st_geometry(cmr.nodes), add =TRUE)
plot(st_geometry(apr.node), add = TRUE)
plot(st_geometry(cmr), add = TRUE)
