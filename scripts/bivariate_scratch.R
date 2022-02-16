# Custom map theme --------------------------------------------------------

theme_map <- function(...) {
  theme_minimal() +
    theme(
      #text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_line(color = "black", size=0.02),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "white", size = 0.002),
      panel.grid.minor = element_line(color = "white", size = 0.002),
      plot.background = element_rect(fill = "white", color = NA), 
      panel.background = element_rect(fill = "white", color = NA), 
      legend.background = element_rect(fill = "white", color = NA),
      #panel.border = element_rect(fill=NA, color = "black"),
      ...
    )
}
# Load Elev data and calc hillshade --------------------------------------- 
#not necessary, purely aesthetic
elev <- getData('alt', country = 'USA')
elev.proj <- projectRaster(elev[[1]], dist.stack)


elev.crop <- crop(elev.proj, dist.stack)
elev.mod <- elev.crop *10
slope <- terrain(elev.mod, opt='slope')
aspect <- terrain(elev.mod, opt='aspect')
hill = hillShade(slope, aspect, 40, 270)
hill2 <- aggregate(hill , fact = 5 , method = "bilinear" )
hills3 <- focal(hill2, w=matrix(1/9, nc=3, nr=3), mean)


# Get vectors for maps ----------------------------------------------------

PAs <- st_read(here::here("Data/ProcessedData/studyPAs.shp")) %>% 
  dplyr::filter(. , Unit_Nm == "Yellowstone National Park" | Unit_Nm == "Weminuche Wilderness") %>% 
  st_transform(. , crs = crs(dist.stack))

sts <- tigris::states() %>% 
  dplyr::filter(., STUSPS %in% c("ID", "MT", "UT", "WY", "CO", "AZ", "NM")) %>% 
  st_transform(., crs(dist.stack)) %>% 
  as(., "Spatial")

conus <-  tigris::states() %>%
  dplyr::filter(,, !STUSPS %in% c("AK","AS", "HI", "GU", "VI", "DC", "PR", "MP")) %>% 
  st_transform(., crs(dist.stack))

sts.crop <- crop(sts, dist.stack)

pa.cents <- rbind(as(origin.proj,"sf"), as(goals.proj, "sf"))
pa.cents$lab <- c("Weminche \n Wilderness Area", "Yellowstone \n National Park")

# Plot Circuitscape results with lcps-----------------------------------------------
library(purrr)
library(dplyr)

b_df <- biophys.cs %>%
  projectRaster(., res=300, crs = crs(dist.stack)) %>%
  rasterToPoints %>%
  as.data.frame() %>%
  `colnames<-`(c("x", "y", "biophys"))

biophys.lcps <- lapply(biophys.lst, function (x) rasterToPolygons(x=x, n=8, dissolve = TRUE))

biophys.lcp.sf <- list(biophys.lcps, makeUniqueIDs = T) %>% 
  flatten() %>% 
  do.call(rbind, .) %>% 
  as(., "sf") %>% 
  dplyr::mutate(., rank = row_number())



p.cs <- RStoolbox::ggR(hills3) +
  geom_raster(
    data = b_df,
    aes(
      x=x,
      y=y,
      fill=biophys
    ),
    alpha=0.8,
    interpolate = TRUE
  ) +
  scale_fill_viridis_c(option="B", 
                       limits=c(-0.000000001,0.005), 
                       breaks = c(0,0.005),
                       labels = c("Low","High")) +
  geom_sf(data=biophys.lcp.sf, aes(color=as.character(rank)), fill=NA, lwd=1.15) +
  scale_color_locuszoom() +
  geom_sf(data = PAs, fill = "forestgreen") +
  geom_sf(data = as(sts.crop, "sf"), fill = NA, color="white")+
  ggrepel::geom_text_repel(data = pa.cents, aes(x = st_coordinates(pa.cents)[,1], y = st_coordinates(pa.cents)[,2], label=lab), nudge_x = -40000 , nudge_y = c(80000,90000), fontface="bold", color = "white")+
  guides(fill = guide_colorbar(nbin = 8, direction = "horizontal", title.position = "top", 
                               label.position="bottom", title = "Current flow"), 
         color = guide_legend(direction = "horizontal", title.position = "top", 
                              label.position="bottom", title = "Cost rank"))+
  theme_map() + theme(legend.position = "bottom")

inset <- ggplot()+
  geom_sf(data=conus, fill="white") +
  geom_sf(data =st_as_sfc(st_bbox(dist.stack)), fill=NA, color="red") +
  theme_map() +
  theme(panel.background = element_rect(fill = "gray", color = "black"),
        plot.background = element_rect(fill = NA, color = NA))  


# Plot bivariate raster ---------------------------------------------------
#normalize (i.e. 0,1) your CS outputs first

# convert gridded raster dato dataframe
b_df <- biophys.norm %>%
  projectRaster(., res=300, crs = crs(dist.stack)) %>%
  rasterToPoints %>%
  as.data.frame() %>%
  `colnames<-`(c("x", "y", "biophys"))

s_df <- implement.norm %>%
  projectRaster(., res=300, crs = crs(dist.stack)) %>%
  rasterToPoints %>%
  as.data.frame() %>%
  `colnames<-`(c("x", "y", "implement"))

b_reclass <- b_df %>% 
  mutate(PCT = ntile(biophys, 5))  %>%  data.frame()

s_reclass <- s_df %>% 
  mutate(PCT = ntile(implement, 5))  %>%  data.frame()

df <- left_join(b_reclass, s_reclass, by = c("x","y")) %>%
  mutate(
    group = paste(PCT.y, PCT.x, sep = " - ")
  ) %>%
  dplyr::select(-c(PCT.x, PCT.y))



# Build a palette ---------------------------------------------------------
colmat<-function(nquantiles=10, upperleft=rgb(0,150,235, maxColorValue=255), upperright=rgb(130,0,80, maxColorValue=255), bottomleft="grey", bottomright=rgb(255,230,15, maxColorValue=255), xlab="x label", ylab="y label"){
  my.data<-seq(0,1,.01)
  my.class<-classIntervals(my.data,n=nquantiles,style="quantile")
  my.pal.1<-findColours(my.class,c(upperleft,bottomleft))
  my.pal.2<-findColours(my.class,c(upperright, bottomright))
  col.matrix<-matrix(nrow = 101, ncol = 101, NA)
  for(i in 1:101){
    my.col<-c(paste(my.pal.1[i]),paste(my.pal.2[i]))
    col.matrix[102-i,]<-findColours(my.class,my.col)}
  plot(c(1,1),pch=19,col=my.pal.1, cex=0.5,xlim=c(0,1),ylim=c(0,1),frame.plot=F, xlab=xlab, ylab=ylab,cex.lab=1.3)
  for(i in 1:101){
    col.temp<-col.matrix[i-1,]
    points(my.data,rep((i-1)/100,101),pch=15,col=col.temp, cex=1)}
  seqs<-seq(0,100,(100/nquantiles))
  seqs[1]<-1
  col.matrix<-col.matrix[c(seqs), c(seqs)]}


col.matrix<-colmat(nquantiles=9)




legend_5 <- tibble(
  "5 - 5" = "#820050",
  "4 - 5" = "#FF05BF",
  "3 - 5" = "#FF0580", 
  "2 - 5" = "#FF0540",
  "1 - 5" = "#FF0500",
  "5 - 4" = "#BF05FF",
  "4 - 4" = "#BF05BF",
  "3 - 4" = "#BF0580", 
  "2 - 4" = "#BF0540",
  "1 - 4" = "#BF0500",
  "5 - 3" = "#8005FF",
  "4 - 3" = "#8005BF",
  "3 - 3" = "#800580", 
  "2 - 3" = "#800540",
  "1 - 3" = "#800500",
  "5 - 2" = "#4005FF",
  "4 - 2" = "#4005BF",
  "3 - 2" = "#400580", 
  "2 - 2" = "#400540",
  "1 - 2" = "#400500",
  "5 - 1" = "#0005FF",
  "4 - 1" = "#0005BF",
  "3 - 1" = "#000580", 
  "2 - 1" = "#000540",
  "1 - 1" = "#000500"
) %>%
  gather("group", "fill")

legend_5$fill[1:5] <- col.matrix[10,seq(10, 0, -2)]
legend_5$fill[6:10] <- col.matrix[8,seq(10, 0, -2)]
legend_5$fill[11:15] <- col.matrix[6,seq(10, 0, -2)]
legend_5$fill[16:20] <- col.matrix[4,seq(10, 0, -2)]
legend_5$fill[21:25] <- col.matrix[2,seq(10, 0, -2)]
df <- left_join(df, legend_5)



p1 <- RStoolbox::ggR(hills3) +
  geom_raster(
    data = df,
    aes(
      x=x,
      y=y,
      fill=fill
    ),
    alpha=0.8,
    interpolate = TRUE
  ) +
  scale_fill_identity() +
  geom_sf(data = PAs, fill = "forestgreen") +
  geom_sf(data = as(sts.crop, "sf"), fill = NA, color="black")+
  ggrepel::geom_text_repel(data = pa.cents, aes(x = st_coordinates(pa.cents)[,1], y = st_coordinates(pa.cents)[,2], label=lab), nudge_x = -40000 , nudge_y = c(80000,90000), fontface="bold", color = "white")+
  theme_map() +
  theme(legend.position = 'none',
        plot.subtitle = element_text(
          color = "grey30",
          size = 40,
          hjust = 0.1),
        plot.title = element_text(
          color = "grey30",
          size = 70,
          hjust = 0.1),
        plot.caption = element_text(
          color = "grey30",
          size = 25,
          lineheight = 0.3)
  )

p2 <- legend_5 %>%
  separate(group,
           into = c("biophys", "implement"),
           sep = " - ") %>%
  mutate(biophys = as.integer(biophys),
         implement = as.integer(implement)) %>%
  ggplot() +
  geom_tile(mapping = aes(
    x = biophys,
    y = implement,
    fill = fill)) +
  scale_fill_identity() +
  labs(x = "Biophysical →\n probability",
       y = "Implementation →\n probability") +
  theme_void() +
  theme(
    axis.title = element_text(
      size = 8,
    ),
    axis.title.y = element_text(angle = 90),
    plot.background = element_rect(fill="white", color = "black")) +
  coord_fixed()

p3 <- ggplot()+
  geom_sf(data=conus, fill="white") +
  geom_sf(data =st_as_sfc(st_bbox(dist.stack)), fill=NA, color="red") +
  theme_map() +
  theme(panel.background = element_rect(fill = "gray", color = "black"),
        plot.background = element_rect(fill = NA, color = NA))

# create final layout
p <- ggdraw(p1)  +
  draw_plot(p2, x = 0.74, y = 0.73, 
            width = 0.26, height = 0.26) +
  draw_plot(p3, x = 0.72, y = 0,  
            width = 0.3, height = 0.2)