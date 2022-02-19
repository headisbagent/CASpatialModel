library(data.table)
library(ggplot2)
library(gdxtools)
library(reshape2)
library(sf)
library(foreign)
library(ggmap)
library(maps)
library(mapdata)
library(gganimate)
library(dplyr)

igdx(gams.executable.location)

base.dir <- getwd()

print.lst.status <- function(file) {
	lst.file <- readLines(file)
	print(grep('SOLVER STATUS',lst.file,value=TRUE))
	print(grep('MODEL STATUS',lst.file,value=TRUE))
}

st_line_midpoints <- function(sf_lines = NULL) {

  g <- st_geometry(sf_lines)

  g_mids <- lapply(g, function(x) {

    coords <- as.matrix(x)

    # this is just a copypaste of View(maptools:::getMidpoints):
    get_mids <- function (coords) {
      dist <- sqrt((diff(coords[, 1])^2 + (diff(coords[, 2]))^2))
      dist_mid <- sum(dist)/2
      dist_cum <- c(0, cumsum(dist))
      end_index <- which(dist_cum > dist_mid)[1]
      start_index <- end_index - 1
      start <- coords[start_index, ]
      end <- coords[end_index, ]
      dist_remaining <- dist_mid - dist_cum[start_index]
      mid <- start + (end - start) * (dist_remaining/dist[start_index])
      return(mid)
    }

    mids <- st_point(get_mids(coords))
    })

  out <- st_sfc(g_mids, crs = st_crs(sf_lines))
  out <- st_sf(out)
}