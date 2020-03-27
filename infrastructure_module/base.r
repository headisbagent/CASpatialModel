library(data.table)
library(ggplot2)
library(gdxtools)
library(reshape2)

igdx(gams.executable.location)

base.dir <- getwd()

print.lst.status <- function(file) {
	lst.file <- readLines(file)
	print(grep('SOLVER STATUS',lst.file,value=TRUE))
	print(grep('MODEL STATUS',lst.file,value=TRUE))
}