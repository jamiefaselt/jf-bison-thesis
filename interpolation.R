library(tigris)
library(ggplot2)
library(tidyverse)
library(sf)
library(sp)
library(raster)
library(dplyr)
library(rgdal)
library(ggmap)
library(usmap)
library(fasterize)

rescale01 <- function(r1) {
  r.rescale <- (r1 - cellStats(r1, min))/(cellStats(r1, max) - cellStats(r1, min))
}


# bring in hsi and temp raster
r <- raster("data/template_raster.tif")
hsi <- raster("data/original/SUMMER_HSI_clip/SUMMER_HSI_clip.tif")
cattle_sales <- read.csv("data/original/NASS_data/cattle_sales_MTWY.csv")

#bring in counties to make the NASS data spatial
counties <- tigris::counties() %>% 
  filter(STATEFP %in% c("30", "56")) %>% 
  st_transform(., st_crs(hsi))

# make columns match to nass data
counties$NAME <- toupper(counties$NAME)
counties <- rename(counties, State.ANSI = STATEFP)
counties <- rename(counties, County.ANSI = COUNTYFP)
counties$State.ANSI <- as.numeric(counties$State.ANSI)
counties$County.ANSI <- as.numeric(counties$County.ANSI)

#plot(counties) #checking and this doesn't have weird gaps yet
cattle_sales$Value <- gsub(",","",cattle_sales$Value)
cattle_sales$Value <- as.numeric(cattle_sales$Value)
# join
cattlesales.spatial <- left_join(counties, cattle_sales)
cattlesales.spatial<- subset(cattlesales.spatial, select = c(Value, geometry))

#cattlesales.spatial<- subset(cattlesales.spatial, select = c(Value, geometry))
median <- median(cattlesales.spatial$Value, na.rm = TRUE)



library(rgdal)
library(tmap)

# Load precipitation data
P <- cattlesales.spatial  %>% 
  st_transform(. , crs(r))  %>% 
  st_centroid(.) %>% 
  as(. , "Spatial")

# Load Texas boudary map
W <- counties  %>% 
  st_transform(. , crs(r))  %>% 
  st_centroid(.) %>% 
  as(. , "Spatial")

# Replace point boundary extent with that of Texas
P@bbox <- W@bbox

# have one county in study area with an NA value-- using spatial interpolation to fill 
library(spatstat)  # Used for the dirichlet tessellation function
library(maptools)  # Used for conversion from SPDF to ppp
library(raster)    # Used to clip out thiessen polygons

# Create a tessellated surface
th  <-  as(dirichlet(as.ppp(P)), "SpatialPolygons")

# The dirichlet function does not carry over projection information
# requiring that this information be added manually
proj4string(th) <- proj4string(P)

# The tessellated surface does not store attribute information
# from the point data layer. We'll use the over() function (from the sp
# package) to join the point attributes to the tesselated surface via
# a spatial join. The over() function creates a dataframe that will need to
# be added to the `th` object thus creating a SpatialPolygonsDataFrame object
th.z     <- over(th, P, fn=mean)
th.spdf  <-  SpatialPolygonsDataFrame(th, th.z)
# Finally, we'll clip the tessellated  surface to the Texas boundaries
th.clp   <- raster::intersect(P,th.spdf)

library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function

# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(P, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(P) <- proj4string(P) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(P)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(Value ~ 1, P, newdata=grd, idp=2.0)

# Convert to raster object then clip to Texas
r       <- raster(P.idw)
r.m     <- mask(r, W)

# Plot
tm_shape(r.m) + 
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Predicted precipitation \n(in inches)") + 
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)


