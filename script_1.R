#setwd("~Desktop/AidData/WestBank")

library(rgdal)
library(dplyr)
library(rgeos)
library(ggmap)
library(raster)
library(spatstat)
library(maptools)
library(sp)
library(foreign)
library(knitr)
library(mapproj)
library(maps)

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
roads_proj <- spTransform(roads_proj, CRS("+proj=cass +lat_0=31.73409694444445 +lon_0=35.21208055555556 +x_0=170251.555 +y_0=126867.909 +a=6378300.789 +b=6356566.435 +units=m +no_defs"))
plot(roads_proj)
roads_proj_f <- fortify(roads_proj)

rdbuff <- readOGR(dsn = "shapefiles/selected", layer = "5km_road_buffer", stringsAsFactors = FALSE, verbose = FALSE)
proj4string(rdbuff)
rdbuff <- spTransform(rdbuff, CRS("+proj=cass +lat_0=31.73409694444445 +lon_0=35.21208055555556 +x_0=170251.555 +y_0=126867.909 +a=6378300.789 +b=6356566.435 +units=m +no_defs"))
plot(rdbuff)
rdbuff_f <- fortify(rdbuff)

rdntwrk <- ggplot() + geom_map(data = governorates_f, aes(x=long, y=lat, map_id = id), map = governorates_f, alpha = .5, col = "black")
rdntwrk <- rdntwrk + geom_map(data = roads_f, aes(x=long, y=lat, map_id = id), map = roads_f, col = "blue" )
rdntwrk



