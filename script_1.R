#setwd("~Desktop/AidData/WestBank")
setwd("~/GitHub/WestBank")

# install.packages("rgdal", dependencies = TRUE)
# install.packages("sp", dependencies = TRUE)
# install.packages("rgeos", dependencies = TRUE)
# install.packages("raster", dependencies = TRUE)
# install.packages("spatstat", dependencies = TRUE)
# install.packages("ggplot2", dependencies = TRUE)
# install.packages("ggmap", dependencies = TRUE)

library(rgdal)
library(sp)
library(rgeos)
library(raster)
library(spatstat)
<<<<<<< HEAD
library(maptools)
library(sp)
library(foreign)
library(knitr)
library(mapproj)
library(maps)
=======
library(ggplot2)
library(ggmap)

#library(dplyr)
#library(maptools)
#library(foreign)
#library(knitr)
>>>>>>> 93b25dddf25e83e35c32fd65545b50db46e52bfc

roads <- readOGR(dsn = "shapefiles/described", layer = "roads", stringsAsFactors=FALSE, verbose=FALSE)
proj4string(roads)
roads <- spTransform(roads, CRS("+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(roads)
roads_f <- fortify(roads)

governorates <- readOGR(dsn = "shapefiles/described", layer = "Governorates", stringsAsFactors=FALSE, verbose=FALSE)
proj4string(governorates)
governorates <- spTransform(governorates, CRS("+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(governorates)
governorates_f <- fortify(governorates)

roads_proj <- readOGR(dsn = "shapefiles/selected", layer = "INPIIRoadsProjects_Line_modified", stringsAsFactors = FALSE, verbose = FALSE)
proj4string(roads_proj)
roads_proj <- spTransform(roads_proj, CRS("+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(roads_proj)
roads_proj_f <- fortify(roads_proj)

rdbuff <- readOGR(dsn = "shapefiles/selected", layer = "5km_road_buffer_reprj", stringsAsFactors = FALSE, verbose = FALSE)
proj4string(rdbuff)
rdbuff <- spTransform(rdbuff, CRS("+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(rdbuff)
rdbuff_f <- fortify(rdbuff)
#rdbuff_f1 <- subset(rdbuff_f, id == 0)

rdntwrk <- ggplot() + geom_map(data = governorates_f, aes(x=long, y=lat, map_id = id), map = governorates_f, alpha = .5, col = "black")
#rdntwrk <- rdntwrk + geom_map(data = roads_f, aes(x=long, y=lat, map_id = id), map = roads_f, alpha = 0, col= "blue")
rdntwrk <- rdntwrk + geom_map(data = roads_proj_f, aes(x=long, y=lat, map_id = id), map = roads_proj_f, alpha = 0, col= "red")
rdntwrk <- rdntwrk + geom_map(data = rdbuff_f, aes(x=long, y=lat, map_id = id), map = rdbuff_f, alpha = 0, col= "yellow")
rdntwrk <- rdntwrk + geom_point(size = .5, shape = 18, aes(x = 716245.1, y = 3514522))
rdntwrk <- rdntwrk + geom_point(size = .5, shape = 17, aes(x = 716271.4, y = 3514513))
rdntwrk



popdata <- raster("~/Google Drive/LiberiaProject/gpw-v4-population-count_2015.tif")
buffpop <- extract(popdata, rdbuff, df = TRUE)
buffpopdf <- data.frame(buffpop)
buffpop

buffpop_sum <- aggregate(. ~ ID, buffpop, sum)
buffpopLT13_sum <- aggregate(. ~ ID, buffpopLT, sum)
buffpopLT13 <- buffpopLT ## .RData file copied from DMSP data ##

#remove("rdbuff_f1")

#unionSpatialPolygons(rdbuff, id)
