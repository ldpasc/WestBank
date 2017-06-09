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
library(ggplot2)
library(ggmap)

#library(dplyr)
#library(maptools)
#library(foreign)
#library(knitr)

roads <- readOGR(dsn = "shapefiles/described", layer = "roads", stringsAsFactors=FALSE, verbose=FALSE)
proj4string(roads)
roads <- spTransform(roads, CRS("+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
roads_f <- fortify(roads)
plot(roads)

governorates <- readOGR(dsn = "shapefiles/described", layer = "Governorates", stringsAsFactors=FALSE, verbose=FALSE)
proj4string(governorates)
governorates <- spTransform(roads, CRS("+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
governorates_f <- fortify(governorates)

roads_proj <- readOGR(dsn = "shapefiles/selected", layer = "INPIIRoadsProjects_Line_modified", stringsAsFactors = FALSE, verbose = FALSE)
proj4string(roads_proj)
roads_proj <- spTransform(roads_proj, CRS("+proj=cass +lat_0=31.73409694444445 +lon_0=35.21208055555556 +x_0=170251.555 +y_0=126867.909 +a=6378300.789 +b=6356566.435 +units=m +no_defs"))
plot(roads_proj)


