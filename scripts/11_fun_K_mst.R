library(raster)
library(gdistance)
library(yenpathy)
library(rgdal)
library(sf)
library(magrittr)
library(matrixStats)


# download data -----------------------------------------------------------
folder_url <- "https://drive.google.com/drive/u/0/folders/16bUzSKoT75gAnue0p1gaXXtD3S6psOAi" # temp data for herd centroids and biophys resistance layer
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/temp/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))


resist <- raster("data/temp/biophys_resistance_layer.tif")
resist[is.na(resist[])] <- 5* cellStats(resist, max)## drop NAs for costodistance
tr <- transition(1/resist, transitionFunction = mean, 16)
tr <- geoCorrection(tr, "c")

numpath <- 3
bufdist <- 4000
pts <- st_read("data/temp/herd_centroids.shp") %>% 
  st_transform(. , crs(resist))  %>% 
  st_centroid(.) %>% 
  as(. , "Spatial")

gen_top_tree <- function(tr, resist,  numpath, bufdist, pts){
  
  combos <- expand.grid(1:nrow(pts), 1:nrow(pts))
  #remove self connections
  keep <- apply(combos[1:2], 1, function(x) length(unique(x[!is.na(x)])) != 1)
  combos <- combos[keep,]
  
  originCells <- raster::cellFromXY(tr, pts)
  goalCells <- raster::cellFromXY(tr, pts)
  indexOrigin <- originCells 
  indexGoal <- goalCells 
  result.list <- vector("list",numpath)
  update.res.list <- vector("list",numpath)
  
  for(z in 1:numpath){
    if(z == 1){
  z <- 1
          y <- transitionMatrix(tr)
      if(isSymmetric(y)) {
        mode <- "undirected"
      }else{
        mode <- "directed"
      }
      adjacencyGraph <- igraph::graph.adjacency(y, mode=mode, weighted=TRUE)
      E(adjacencyGraph)$weight <- 1/E(adjacencyGraph)$weight
      dst <- distances(adjacencyGraph, indexOrigin, indexGoal, mode="out", weights =E(adjacencyGraph)$weight)
      diag(dst) <- NA
      dst <- as.data.frame(dst)
      dst$minIwant <- rowMins(as.matrix(dst), na.rm = TRUE)
      pos <- which(as.matrix(dst[,1:3]) == dst$minIwant, arr.ind = TRUE)
      path.list <- vector("list",nrow(pos))
      for (i in 1:nrow(pos)){
        sp <- shortest_paths(adjacencyGraph, from = indexOrigin[pos[i,1]], to = indexGoal[pos[i,2]], mode = "out")
        path.list[i] <-sp$vpath 
      }
      #path.vect <- unlist(path.list)
      result <- tr
      transitionMatrix(result) <- Matrix::Matrix(0, ncol=ncell(tr), nrow=ncell(tr))
      for(i in 1:length(path.list)){
        sPVector <- path.list[[i]]
        adj <- cbind(sPVector[-(length(sPVector))], sPVector[-1])
        adj <- rbind(adj,cbind(adj[,2], adj[,1]))
        transitionMatrix(result)[adj] <- 1/length(path.list) + transitionMatrix(result)[adj]
        }
      result.rast <- raster(result)
      result.buf <- raster::buffer(result.rast, width = bufdist, doEdge=TRUE)
      result.list[[z]] <- result.buf
      result.buf.2 <- raster::buffer(result.rast, width = 2 * bufdist, doEdge=TRUE)
      update <- tr
      adj <- raster::adjacent(result.buf.2, Which(!is.na(result.buf.2), cells = TRUE), directions=16)
      transitionMatrix(update)[adj] <- cellStats(raster(tr), min)
      update.res.list[[z]] <- update
    }else{
      y <- transitionMatrix(update.res.list[[z-1]])
      if(isSymmetric(y)) {
        mode <- "undirected"
      }else{
        mode <- "directed"
      }
      adjacencyGraph <- igraph::graph.adjacency(y, mode=mode, weighted=TRUE)
      E(adjacencyGraph)$weight <- 1/E(adjacencyGraph)$weight
      dst <- distances(adjacencyGraph, indexOrigin, indexGoal, mode="out", weights =E(adjacencyGraph)$weight)
      diag(dst) <- NA
      dst <- as.data.frame(dst)
      dst$minIwant <- rowMins(as.matrix(dst), na.rm = TRUE)
      pos <- which(as.matrix(dst[,1:3]) == dst$minIwant, arr.ind = TRUE)
      path.list <- vector("list",nrow(pos))
      for (i in 1:nrow(pos)){
        sp <- shortest_paths(adjacencyGraph, from = indexOrigin[pos[i,1]], to = indexGoal[pos[i,2]], mode = "out")
        path.list[i] <-sp$vpath 
      }
      result <- tr
      transitionMatrix(result) <- Matrix::Matrix(0, ncol=ncell(tr), nrow=ncell(tr))
      for(i in 1:length(path.list)){
        sPVector <- path.list[[i]]
        adj <- cbind(sPVector[-(length(sPVector))], sPVector[-1])
        adj <- rbind(adj,cbind(adj[,2], adj[,1]))
        transitionMatrix(result)[adj] <- 1/length(path.list) + transitionMatrix(result)[adj]
        }
      result.rast <- raster(result)
      result.buf <- raster::buffer(result.rast, width = bufdist, doEdge=TRUE)
      result.list[[z]] <- result.buf
      result.buf.2 <- raster::buffer(result.rast, width = 2 * bufdist, doEdge=TRUE)
      update <- update.res.list[[z-1]] 
      adj <- raster::adjacent(result.buf.2, Which(!is.na(result.buf.2), cells = TRUE), directions=16)
      transitionMatrix(update)[adj] <- cellStats(raster(tr), min)
      update.res.list[[z]] <- update
    }
  }
  all.res <- list(result.list, update.res.list)
  return(all.res)
}


gen_top_paths <- function(tr, resist,  numpath, bufdist, pts){
  originCells <- raster::cellFromXY(tr, orig)
  goalCells <- raster::cellFromXY(tr, goal)
  indexOrigin <- originCells 
  indexGoal <- goalCells 
  result.list <- vector("list",numpath)
  update.res.list <- vector("list",numpath)
  for(z in 1:numpath){
    if(z == 1){
      y <- transitionMatrix(tr)
      if(isSymmetric(y)) {
        mode <- "undirected"
      }else{
        mode <- "directed"
      }
      adjacencyGraph <- igraph::graph.adjacency(y, mode=mode, weighted=TRUE)
      E(adjacencyGraph)$weight <- 1/E(adjacencyGraph)$weight
      shortestPaths <- get.shortest.paths(adjacencyGraph,
                                          indexOrigin, indexGoal)$vpath
      result <- tr
      transitionMatrix(result) <- Matrix::Matrix(0, ncol=ncell(tr), nrow=ncell(tr))
      for(i in 1:length(shortestPaths)){
        sPVector <- shortestPaths[[i]]
        adj <- cbind(sPVector[-(length(sPVector))], sPVector[-1])
        adj <- rbind(adj,cbind(adj[,2], adj[,1]))
        transitionMatrix(result)[adj] <- 1/length(shortestPaths) + transitionMatrix(result)[adj]}
      result.rast <- raster(result)
      result.buf <- raster::buffer(result.rast, width = bufdist, doEdge=TRUE)
      result.list[[z]] <- result.buf
      result.buf.2 <- raster::buffer(result.rast, width = 2 * bufdist, doEdge=TRUE)
      update <- tr
      adj <- raster::adjacent(result.buf.2, Which(!is.na(result.buf.2), cells = TRUE), directions=16)
      transitionMatrix(update)[adj] <- cellStats(raster(tr), min)
      update.res.list[[z]] <- update
    }else{
      y <- transitionMatrix(update.res.list[[z-1]])
      if(isSymmetric(y)) {
        mode <- "undirected"
      }else{
        mode <- "directed"
      }
      adjacencyGraph <- igraph::graph.adjacency(y, mode=mode, weighted=TRUE)
      E(adjacencyGraph)$weight <- 1/E(adjacencyGraph)$weight
      shortestPaths <- get.shortest.paths(adjacencyGraph,
                                          indexOrigin, indexGoal)$vpath
      result <- tr
      transitionMatrix(result) <- Matrix::Matrix(0, ncol=ncell(tr), nrow=ncell(tr))
      for(i in 1:length(shortestPaths)){
        sPVector <- shortestPaths[[i]]
        adj <- cbind(sPVector[-(length(sPVector))], sPVector[-1])
        adj <- rbind(adj,cbind(adj[,2], adj[,1]))
        transitionMatrix(result)[adj] <- 1/length(shortestPaths) + transitionMatrix(result)[adj]}
      result.rast <- raster(result)
      result.buf <- raster::buffer(result.rast, width = bufdist, doEdge=TRUE)
      result.list[[z]] <- result.buf
      result.buf.2 <- raster::buffer(result.rast, width = 2 * bufdist, doEdge=TRUE)
      update <- update.res.list[[z-1]] 
      adj <- raster::adjacent(result.buf.2, Which(!is.na(result.buf.2), cells = TRUE), directions=16)
      transitionMatrix(update)[adj] <- cellStats(raster(tr), min)
      update.res.list[[z]] <- update
    }
  }
  all.res <- list(result.list, update.res.list)
  return(all.res)
}
