library(data.table)
library(ggplot2)
library(gdxtools)
library(reshape2)
library(sf)
library(foreign)
library(dplyr)
library(RColorBrewer)
library(cowplot)
library(spatstat)
library(broom)
library(maptools)
library(scales)

igdx(gams.executable.location)

base.dir <- getwd()

print.lst.status <- function(file) {
	lst.file <- readLines(file)
	print(grep('SOLVER STATUS',lst.file,value=TRUE))
	print(grep('MODEL STATUS',lst.file,value=TRUE))
}