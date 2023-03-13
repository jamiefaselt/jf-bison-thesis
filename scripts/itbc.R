# ITBC member tribes
library(fasterize)
library(raster)
library(sp)
library(sf)
library(rgeos)
library(rgdal)
library(tidyverse)
library(tigris)
library(dplyr)
library(terra)

#bring in the hsi layer to match the crs
hsi <- rast("/Users/jamiefaselt/Research/Data/DRAFT_Bison_Summer_EVI_8bit.tif")
hmi <- rast("data/original/Human_Modification_Index/dblbnd.adf")

hsi.resamp <- terra::resample(hsi, hmi)
hsi.crop <- terra::mask(hsi.resamp, hmi)
plot(hsi.crop)

full.rez <- st_read("data/tl_2018_us_aiannh/tl_2018_us_aiannh.shp")%>% 
  st_transform(.,st_crs(hmi)) %>% 
  st_make_valid()
head(full.rez)

itbc <- full.rez[full.rez$NAME %in% c("Old Harbor", "Blackfeet", "Cherokee", "Cheyenne-Arapaho", "Cheyenne River", "Rocky Boy's
", "Flathead", "Umatilla", "Crow Creek", "Crow","Wind River","Flandreau","Forest County Potawatomi",
"Fort Belknap",
"Fort Peck","Ho-Chunk Nation",
"Iowa",
"Jicarilla Apache Nation",
"Kalispel",
"Leech Lake",
"Lower Brule",
"Mesa Grande", "Sac and Fox",
"Fort Berthold",
"Modoc",
"Nambe",
"Ruby",
"Northern Cheyenne",
"Pine Ridge",
"Ohkay Owingeh",
"Omaha",
"Oneida Nation",
"Osage","Picuris",
"Pit River",
"Ponca (NE)",
"Prairie Band of Potawatomi Nation",
"Prairie Island",
"Cochiti",
"Pojoaque",
"Sandia",
"Tesuque",
"Quapaw",
"Red Lake",
"Rosebud",
"Round Valley",
"Sac and Fox Nation",
"Salt River",
"Santee",
"Seminole",
"Seneca-Cayuga",
"Shakopee Mdewakanton Sioux",
"Fort Hall",
"Lake Traverse",
"Skull Valley",
"Southern Ute",
"Spirit Lake",
"Standing Rock",
"Stevens Village",
"Stillaguamish",
"Taos",
"Tonkawa",
"Turtle Mountain",
"Ute Mountain",
"White Earth",
"Winnebago",
"Yakama Nation",
"Yankton"),] 

itbc.conus <- full.rez[full.rez$NAME %in% c("Blackfeet", "Cherokee", "Cheyenne-Arapaho", "Cheyenne River", "Rocky Boy's", "Flathead", "Umatilla", "Crow Creek", "Crow","Wind River","Flandreau","Forest County Potawatomi",
                                      "Fort Belknap",
                                      "Fort Peck","Ho-Chunk Nation",
                                      "Iowa",
                                      "Jicarilla Apache Nation",
                                      "Kalispel",
                                      "Leech Lake",
                                      "Lower Brule",
                                      "Mesa Grande", "Sac and Fox",
                                      "Fort Berthold",
                                      "Modoc",
                                      "Nambe",
                                      "Northern Cheyenne",
                                      "Pine Ridge",
                                      "Ohkay Owingeh",
                                      "Omaha",
                                      "Oneida Nation",
                                      "Osage","Picuris",
                                      "Pit River",
                                      "Ponca (NE)",
                                      "Prairie Band of Potawatomi Nation",
                                      "Prairie Island",
                                      "Cochiti",
                                      "Pojoaque",
                                      "Sandia",
                                      "Tesuque",
                                      "Quapaw",
                                      "Red Lake",
                                      "Rosebud",
                                      "Round Valley",
                                      "Sac and Fox Nation",
                                      "Salt River",
                                      "Santee",
                                      "Seminole",
                                      "Seneca-Cayuga",
                                      "Shakopee Mdewakanton Sioux",
                                      "Fort Hall",
                                      "Lake Traverse",
                                      "Skull Valley",
                                      "Southern Ute",
                                      "Spirit Lake",
                                      "Standing Rock",
                                      "Stillaguamish",
                                      "Taos",
                                      "Tonkawa",
                                      "Turtle Mountain",
                                      "Ute Mountain",
                                      "White Earth",
                                      "Winnebago",
                                      "Yakama Nation",
                                      "Yankton"),] 



simp <- st_simplify(itbc.conus)
st_write(itbc.conus, "data/processed/itbc_conus.shp")
plot(st_geometry(simp))

# crop to just lower 48-- sorry Alaska
states <- tigris::states() 
states.con<-states[!(states$STUSPS=="AK" | states$STUSPS=="HI" | states$STUSPS=="VI" |states$STUSPS=="GU" |states$STUSPS=="PR"),] %>% 
  st_transform(crs(hmi)) %>% 
  st_make_valid(.)
st_write(states, "data/processed/conus.shp")
