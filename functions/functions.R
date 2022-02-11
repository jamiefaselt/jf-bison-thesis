# Functions 

rescale01 <- function(r1) {
  r.rescale <- (r1 - cellStats(r1, min))/(cellStats(r1, max) - cellStats(r1, min))
}

# fuzzy sum approach to combine them from Theobald 2013
fuzzysum <- function(r1, r2) {
  rc1.1m <- (1-r1)
  rc2.1m <- (1-r2)
  fuz.sum <- 1-(rc1.1m*rc2.1m)
}

rc1.1m <- 1-hsi.rescale
rc2.1m <- 1-hmi
fuz.sum <- 1-(rc1.1m*rc2.1m)
plot(fuz.sum) 
fuz.sum


