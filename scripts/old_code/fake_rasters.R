library("ggplot2")

counties <- tigris::counties()
nj.counties<-counties %>% filter(STATEFP %in%  c("34"))
nj.counties<-st_transform(nj.counties,st_crs(r))

sussex <- nj.counties %>% 
  filter(., grepl('Sussex',  NAME))

nj.tracts <- get_acs(geography = "tract",
                     variables = c(pop = "B01003_001"),
                     state = "NJ",
                     year = 2019,
                     geometry = TRUE) %>% 
  st_transform(.,st_crs(r))
plot(nj.tracts)

sussex.tracts <- st_join(sussex, nj.tracts)
plot(sussex.tracts)
p = ggplot(nj.tracts, aes(fill = estimate)) + geom_sf(color = NA) + coord_sf(crs = st_crs(r)) 
p

hmi <- raster("data/processed/hmi.crop.tif")
plot(hmi, col=plasma(256), axes = TRUE, main = "Social Composite Circuitscape Output")
plot(st_geometry(mt.counties), add = TRUE)


r <- raster("data/template_raster.tif")
plot(r)
r <- raster(xmn=0, xmx=7, ymn=0, ymx=7, ncol=50, nrow=50)
s <- sample(ncell(r), replace=TRUE)
s[1:8]
## [1] -1 -1 -1 -1  1 -1  1 -1
R <- setValues(r, s)
plot(R)
R

states <- tigris::states()
mt <- states %>% filter(., NAME=="Montana", drop=TRUE)
counties <- tigris::counties()
mt.counties<-counties %>% filter(STATEFP %in%  c("30"))
mt.counties<-st_transform(mt.counties,st_crs(r)) 

sheridan <- mt.counties %>% 
  filter(., grepl('Sheridan',  NAME))
ext <- st_bbox(sheridan)
r <- raster(crs= proj4string(as(poly, "Spatial")), ext=raster::extent(as(poly, "Spatial")), resolution= 540)
extent(r)

crop <- raster::crop(hmi, sheridan)
plot(crop)
sher.hmi <- raster::mask(hmi, sheridan) %>% 
  resample(., r)

ggplot()+
  geom_raster(hmi)+
  scale_color_brewer(palette = "Dark2")

ggplot() +
  geom_raster(data = hmi , aes(x = x, y = y)+
  scale_color_brewer(palette = "Dark2")) + 
  coord_quickmap()

library(RColorBrewer)
display.brewer.all()
my.palette <- brewer.pal(n = 10, name = "RdBu")
grn <- brewer.pal(n=9, name = "YlGn")

plot(r, col=my.palette, axes = FALSE)

ra <- focal(r, w=matrix(1/9, nc=3, nr=3))
plot(ra)

trib.gov <- raster("data/raster_layers/tribal_wildlife_gov_tract.tif")
plot(log(trib.gov), scale_color_brewer(palette = "Dark2"), axes = TRUE, main = "Social Composite Circuitscape Output")

plot(log(sher.hmi), scale_color_brewer(palette = "Dark2"))

plot(st_geometry(mt.counties))
plot(st_geometry(poly), add = TRUE)
plot(st_geometry(poly))
ext <- st_bbox(sher.hmi)
poly <- st_as_sfc(st_bbox(c(xmin = st_bbox(sheridan)[[1]]+500000, xmax = st_bbox(sheridan)[[3]]+500000, ymax = st_bbox(sheridan)[[4]]-50050, ymin = st_bbox(sheridan)[[2]]-50000), crs = st_crs(hmi)))
poly <- st_as_sf(poly) %>% 
  st_transform(crs=st_crs(hmi)) %>% 
  st_make_valid()
r <- raster::raster(crs= proj4string(as(poly, "Spatial")), ext=raster::extent(as(poly, "Spatial")), resolution= 540)
plot(r)
cr

scale_color_gradient2(low = ("red"), high = ("blue"))
scale_colour_gradient(
  low = muted("red"),
  high = muted("blue"),
  space = "Lab",
  na.value = "grey50",
  guide = "colourbar",
  aesthetics = "colour")


library(brewc)
install.packages("MetBrewer")
library("MetBrewer")
met.brewer("Troy",n=15,type="continuous")
ggplot() +
  geom_sf(data = mt.highways$osm_lines,
          inherit.aes = FALSE,
          color = "black")

plot(crop, col= met.brewer::("Troy",n=15,type="continuous"), axes = TRUE, main = "Social Composite Circuitscape Output")


plot(crop, col=plasma(256), axes = TRUE, main = "Social Composite Circuitscape Output")
