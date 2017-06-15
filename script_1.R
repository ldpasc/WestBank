
rm(list=ls(all=TRUE))
#setwd("~Desktop/AidData/WestBank")
setwd("~/GitHub/WestBank")

library(rgdal)
library(sp)
library(rgeos)
library(raster)
library(spatstat)
library(maptools)
library(sp)
library(foreign)
library(knitr)
library(mapproj)
library(maps)
library(ggplot2)
library(ggmap)
library(rasterVis)
library(latticeExtra)
library(gridExtra)

####Kathryn's Code####
lightdmsp <- raster("~/GitHub/WestBank/F182013.v4c_web.stable_lights.avg_vis.tif")
# buffpopLT <- extract(lightdmsp, rdbuff, df=T)
# save(buffpopLT,file = "buffpopLT.RData")
load("buffpopLT.RData")
buffpopLT_sum <- aggregate(. ~ ID, buffpopLT, sum)

proj4string(lightdmsp)
governorates <- readOGR(dsn = "shapefiles/described", layer = "Governorates", stringsAsFactors=FALSE, verbose=FALSE)
governorates <- spTransform(governorates, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

rdbuff <- readOGR(dsn = "shapefiles/selected", layer = "5km_road_buffer_reprj", stringsAsFactors = FALSE, verbose = FALSE)
rdbuff <- spTransform(rdbuff, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

roads_proj <- readOGR(dsn = "shapefiles/selected", layer = "INPIIRoadsProjects_Line_modified", stringsAsFactors = FALSE, verbose = FALSE)
roads_proj <- spTransform(roads_proj, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

roads <- readOGR(dsn = "shapefiles/described", layer = "roads", stringsAsFactors=FALSE, verbose=FALSE)
roads <- spTransform(roads, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

buffer_dis <- readOGR(dsn = "shapefiles/selected", layer = "dissolve", stringsAsFactors = FALSE, verbose = FALSE)
buffer_dis <- spTransform(buffer_dis, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#For pixels
cds1 <- rbind(buffer_dis) 
cds2 <- rbind(buffer_dis) 

polys <- SpatialPolygonsDataFrame(SpatialPolygons(list(Polygons(list(Polygon(cds1)), 1), 
                                                       Polygons(list(Polygon(cds2)), 2))),data.frame(ID=c(1,2)))
#Combines the polygon with the raster layer to gain the data# 
( v <- extract(lightdmsp, buffer_dis) )

sum(lengths(v))
sum(v[[6]])
# compareCRS(lightdmsp, governorates)
plot(governorates)
####level plot####
    lightdmsp<- crop(lightdmsp, extent(34.21877, 35.57319, 31.22437, 32.55239))
    plot(lightdmsp)
    blueblue<-colorRampPalette(c("gray","black"))(63)
    
    (levelplot(lightdmsp, col.regions=blueblue, main = "West Bank Avg Nighttime Light") + layer(sp.polygons(governorates, col = "black", lwd=2)) 
      + layer(sp.polygons(rdbuff, col = "red")) + layer(sp.polygons(roads_proj, col = "#FF9100")) + layer(sp.polygons(roads, col = "black", alpha = 0.3)))
    #1
    A1 <- (levelplot(lightdmsp, col.regions=blueblue, main = "West Bank Avg Nighttime Light") + layer(sp.polygons(governorates, col = "white", lwd=2)) 
     + layer(sp.polygons(roads_proj, col = "yellow"))) + layer(sp.polygons(rdbuff, col = "red")) 
    #2
    A2 <- (levelplot(lightdmsp, col.regions=blueblue, main = "West Bank Avg Nighttime Light") + layer(sp.polygons(governorates, col = "white", lwd=2)) 
      + layer(sp.polygons(roads_proj, col = "yellow"))) + layer(sp.polygons(roads, col = "white", alpha = .2))
    #3
    A3 <- (levelplot(lightdmsp, col.regions=blueblue, main = "West Bank Avg Nighttime Light") + layer(sp.polygons(governorates, col = "white", lwd=2)) 
      + layer(sp.polygons(buffer_dis, col = "red"))) 
    A3
    #4
    A4 <- (levelplot(lightdmsp, col.regions=blueblue, main = "West Bank Avg Nighttime Light") + layer(sp.polygons(governorates, col = "white", lwd=2)) 
      + layer(sp.polygons(roads_proj, col = "yellow"))) 
    
    MAPS <- arrangeGrob(A1,A2,A3,A4,nrow=2,ncol=2)
    ggsave("WB_Temp_Maps.png",MAPS,width = 26, height = 24, dpi = 150) 
    
####Governorate Plots#####
    #Overall
    lightdmsp <- raster("~/GitHub/WestBank/F182013.v4c_web.stable_lights.avg_vis.tif")
    blueblue<-colorRampPalette(c("gray","black"))(63)
    lightdmsp_all<- crop(lightdmsp, extent(34.21877, 35.57319, 31.22437, 32.55239))    
    
    Master <- (levelplot(lightdmsp_all, col.regions=blueblue, main = "West Bank \n Avg Nighttime Light") + layer(sp.polygons(governorates, col = "white", lwd=2)) 
                 + layer(sp.polygons(roads_proj, col = "yellow")) + layer(sp.polygons(rdbuff, col = "red"))
                 + layer(panel.text(35.05, 32.52, "Jenin", col="white", cex = .8))
                 + layer(panel.text(34.88, 32.3, "Tulkarm", col="white", cex = .8))
                 + layer(panel.text(34.85, 32.18, "Qalqilya", col="white", cex = .8))
                 + layer(panel.text(35.3, 32.18, "Nablus", col="white", cex = .8))
                 + layer(panel.text(34.88, 32.09, "Salfit", col="white", cex = .8))
                 + layer(panel.text(34.68, 31.95, "Ramallah and Al Bireh", col="white", cex = .8))
                 + layer(panel.text(35.48, 32, "Jericho", col="white", cex = .8))
                 + layer(panel.text(35, 31.79, "Jerusalem", col="white", cex = .8))
                 + layer(panel.text(34.95, 31.72, "Bethlehem", col="white", cex = .8))
                 + layer(panel.text(34.8, 31.5, "Hebron", col="white", cex = .8))
               ) 
    Master  
    
    #Overall
    lightdmsp <- raster("~/GitHub/WestBank/F182013.v4c_web.stable_lights.avg_vis.tif")
    blueblue<-colorRampPalette(c("gray","black"))(63)
    lightdmsp_all<- crop(lightdmsp, extent(34.21877, 35.57319, 31.22437, 32.55239))    
    
    Masterv2 <- (levelplot(lightdmsp_all, col.regions=blueblue, main = "West Bank \n Avg Nighttime Light") + layer(sp.polygons(governorates, col = "white", lwd=2)) 
               + layer(panel.text(35.05, 32.52, "Jenin", col="white", cex = .8))
               + layer(panel.text(34.88, 32.3, "Tulkarm", col="white", cex = .8))
               + layer(panel.text(34.85, 32.18, "Qalqilya", col="white", cex = .8))
               + layer(panel.text(35.3, 32.18, "Nablus", col="white", cex = .8))
               + layer(panel.text(34.88, 32.09, "Salfit", col="white", cex = .8))
               + layer(panel.text(34.68, 31.95, "Ramallah and Al Bireh", col="white", cex = .8))
               + layer(panel.text(35.48, 31.82, "Jericho", col="white", cex = .8))
               + layer(panel.text(35, 31.79, "Jerusalem", col="white", cex = .8))
               + layer(panel.text(34.95, 31.72, "Bethlehem", col="white", cex = .8))
               + layer(panel.text(34.8, 31.5, "Hebron", col="white", cex = .8))
    ) 
    Masterv2  
    
  #Jenin
  lightdmsp <- raster("~/GitHub/WestBank/F182013.v4c_web.stable_lights.avg_vis.tif")
  blueblue<-colorRampPalette(c("gray","black"))(63)
  lightdmsp_je<- crop(lightdmsp, extent(35.05, 35.44, 32.3, 32.56239))
  
  Jenin <- (levelplot(lightdmsp_je, col.regions=blueblue, main = "Jenin \n Avg Nighttime Light") + layer(sp.polygons(governorates, col = "white", lwd=2)) 
         + layer(sp.polygons(roads_proj, col = "yellow"))) + layer(sp.polygons(rdbuff, col = "red")) 
  Jenin  
    
  #Tulkarm
  lightdmsp <- raster("~/GitHub/WestBank/F182013.v4c_web.stable_lights.avg_vis.tif")
  blueblue<-colorRampPalette(c("gray","black"))(63)
  lightdmsp_tul<- crop(lightdmsp, extent(34.95, 35.25, 32.2, 32.46))
  
  Tulkarm <- (levelplot(lightdmsp_tul, col.regions=blueblue, main = "Tulkarm \n Avg Nighttime Light") + layer(sp.polygons(governorates, col = "white", lwd=2)) 
            + layer(sp.polygons(roads_proj, col = "yellow"))) + layer(sp.polygons(rdbuff, col = "red")) 
  Tulkarm  
  
  #Qalqilya
  lightdmsp <- raster("~/GitHub/WestBank/F182013.v4c_web.stable_lights.avg_vis.tif")
  blueblue<-colorRampPalette(c("gray","black"))(63)
  lightdmsp_qa<- crop(lightdmsp, extent(34.9, 35.21, 32.105, 32.26))
  
  Qalqilya <- (levelplot(lightdmsp_qa, col.regions=blueblue, main = "Qalqilya \n Avg Nighttime Light") + layer(sp.polygons(governorates, col = "white", lwd=2)) 
              + layer(sp.polygons(roads_proj, col = "yellow"))) + layer(sp.polygons(rdbuff, col = "red")) 
  Qalqilya  
  
  #Nablus
  lightdmsp <- raster("~/GitHub/WestBank/F182013.v4c_web.stable_lights.avg_vis.tif")
  blueblue<-colorRampPalette(c("gray","black"))(63)
  lightdmsp_na<- crop(lightdmsp, extent(35.1, 35.48, 32.03, 32.35))
  
  Nablus <- (levelplot(lightdmsp_na, col.regions=blueblue, main = "Nablus \n Avg Nighttime Light") + layer(sp.polygons(governorates, col = "white", lwd=2)) 
               + layer(sp.polygons(roads_proj, col = "yellow"))) + layer(sp.polygons(rdbuff, col = "red")) 
  Nablus 
  
  
  #Salfit
  lightdmsp <- raster("~/GitHub/WestBank/F182013.v4c_web.stable_lights.avg_vis.tif")
  blueblue<-colorRampPalette(c("gray","black"))(63)
  lightdmsp_sa<- crop(lightdmsp, extent(34.97, 35.26, 32, 32.19))
  
  Salfit <- (levelplot(lightdmsp_sa, col.regions=blueblue, main = "Salfit \n Avg Nighttime Light") + layer(sp.polygons(governorates, col = "white", lwd=2)) 
             + layer(sp.polygons(roads_proj, col = "yellow"))) + layer(sp.polygons(rdbuff, col = "red")) 
  Salfit 
  
  #Ramallah and Al Bireh
  lightdmsp <- raster("~/GitHub/WestBank/F182013.v4c_web.stable_lights.avg_vis.tif")
  blueblue<-colorRampPalette(c("gray","black"))(63)
  lightdmsp_ra<- crop(lightdmsp, extent(34.9, 35.44, 31.81, 32.1))
  
  Ramallah_Bireh <- (levelplot(lightdmsp_ra, col.regions=blueblue, main = "Ramallah and Al Bireh \n Avg Nighttime Light") + layer(sp.polygons(governorates, col = "white", lwd=2)) 
             + layer(sp.polygons(roads_proj, col = "yellow"))) + layer(sp.polygons(rdbuff, col = "red")) 
  Ramallah_Bireh  
  
  #Jericho
  lightdmsp <- raster("~/GitHub/WestBank/F182013.v4c_web.stable_lights.avg_vis.tif")
  blueblue<-colorRampPalette(c("gray","black"))(63)
  lightdmsp_je<- crop(lightdmsp, extent(35.3, 35.57319, 31.75, 32.2))
  
  Jericho <- (levelplot(lightdmsp_je, col.regions=blueblue, main = "Jericho \n Avg Nighttime Light") + layer(sp.polygons(governorates, col = "white", lwd=2)) 
                     + layer(sp.polygons(roads_proj, col = "yellow"))) + layer(sp.polygons(rdbuff, col = "red")) 
  Jericho  
  
  #Jerusalem
  lightdmsp <- raster("~/GitHub/WestBank/F182013.v4c_web.stable_lights.avg_vis.tif")
  blueblue<-colorRampPalette(c("gray","black"))(63)
  lightdmsp_js<- crop(lightdmsp, extent(35, 35.5, 31.69, 31.95))
  
  Jerusalem <- (levelplot(lightdmsp_js, col.regions=blueblue, main = "Jerusalem \n Avg Nighttime Light") + layer(sp.polygons(governorates, col = "white", lwd=2)) 
              + layer(sp.polygons(roads_proj, col = "yellow"))) + layer(sp.polygons(rdbuff, col = "red")) 
  Jerusalem  
  
  #Bethlehem
  lightdmsp <- raster("~/GitHub/WestBank/F182013.v4c_web.stable_lights.avg_vis.tif")
  blueblue<-colorRampPalette(c("gray","black"))(63)
  lightdmsp_be<- crop(lightdmsp, extent(35.05, 35.49, 31.46, 31.8))
  
  Bethlehem <- (levelplot(lightdmsp_be, col.regions=blueblue, main = "Bethlehem \n Avg Nighttime Light") + layer(sp.polygons(governorates, col = "white", lwd=2)) 
                + layer(sp.polygons(roads_proj, col = "yellow"))) + layer(sp.polygons(rdbuff, col = "red")) 
  Bethlehem  
  
  #Hebron
  lightdmsp <- raster("~/GitHub/WestBank/F182013.v4c_web.stable_lights.avg_vis.tif")
  blueblue<-colorRampPalette(c("gray","black"))(63)
  lightdmsp_he<- crop(lightdmsp, extent(34.83, 35.35, 31.32, 31.7))
  
  Hebron <- (levelplot(lightdmsp_he, col.regions=blueblue, main = "Hebron \n Avg Nighttime Light") + layer(sp.polygons(governorates, col = "white", lwd=2)) 
                + layer(sp.polygons(roads_proj, col = "yellow"))) + layer(sp.polygons(rdbuff, col = "red")) 
  Hebron  

  #Comparison Maps#
  MAPS1 <- arrangeGrob(Jenin,Tulkarm,nrow=2,ncol=1)
  ggsave("WB_jn_tu.png",MAPS1,width = 28, height = 26, dpi = 500) 
  
  MAPS1.5 <- arrangeGrob(Qalqilya, Nablus, nrow = 2, ncol=1)
  ggsave("WB_qa_na.png",MAPS1.5,width = 28, height = 26, dpi = 500) 
  
  MAPS2 <- arrangeGrob(Salfit,Ramallah_Bireh,nrow=2,ncol=1)
  ggsave("WB_sa_ra.png",MAPS2,width = 28, height = 26, dpi = 500) 
  
  MAPS2.5 <- arrangeGrob(Jericho,Jerusalem,nrow=2,ncol=1)
  ggsave("WB_je_js.png",MAPS2.5,width = 28, height = 26, dpi = 500)
  
  MAPS3 <- arrangeGrob(Bethlehem,Hebron,nrow=2,ncol=1)
  ggsave("WB_be_he.png",MAPS3,width = 28, height = 26, dpi = 500) 
  
####
    cds1 <- rbind(rdbuff) 
    cds2 <- rbind(rdbuff) 
    
    polys <- SpatialPolygonsDataFrame(SpatialPolygons(list(Polygons(list(Polygon(cds1)), 1), 
                                                           Polygons(list(Polygon(cds2)), 2))),data.frame(ID=c(1,2)))
    #Combines the polygon with the raster layer to gain the data# 
    ( v <- extract(lightdmsp, rdbuff) )
    
    sum(lengths(v))
    sum(v[[6]])
    #I can tell you the total lumens per circle. I think I can also figure out the # of pixels 
    v2 <- as.matrix(v)

#All together
    lightdmsp <- raster("~/GitHub/WestBank/F182013.v4c_web.stable_lights.avg_vis.tif")
    lightdmsp<- crop(lightdmsp, extent(34.21877, 35.57319, 31.22437, 32.55239))
    rdbuff <- readOGR(dsn = "shapefiles/selected", layer = "5km_road_buffer_reprj", stringsAsFactors = FALSE, verbose = FALSE)
    rdbuff <- spTransform(rdbuff, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    ( v <- extract(lightdmsp, rdbuff) )
    lengths(v)
    #on "rdbuff" the ID's go 0 to 58 and in the raster extract "v" the IDs are 1-59 thereby the 0 became the 1 and so on
    #The length of the column should be the number of pixels while the sum of the column is total nighttime light. Divide 
    #the total of the column by the number of pixels (length) to obtain the average across the polygon 

####ggplot testing 

wb <- get_map(location = c(34.21877, 35.57319, 31.22437, 32.55239), zoom = 4, maptype = "watercolor")
wb <- ggmap(wb)
wb <- wb + geom_map(data=africa_f, map=africa_f, aes(x=long, y=lat, map_id=id), color ="white", fill ="orangered4", alpha = .4, size = .3)
wb <- wb + geom_map(data=nigeria1_main_f, map=nigeria1_main_f, aes(x=long, y=lat, map_id=id), color ="white", fill ="orangered4", alpha = .4, size = .3)
wb <- wb + geom_map(data=nigeria1_main_f, map=nigeria1_main_f, aes(x=long, y=lat, map_id=id, fill = maize), alpha = .4)
wb <- wb + scale_fill_gradient(low="black", high="green", space="Lab", name="Maize")
wb <- wb + annotate('text', x = labels$x, y = labels$y, label = labels$country, size = 3, color = "white")
wb <- wb + annotate('text', x = nigeria_1$x, y = nigeria_1$y, label =  nigeria_1$country, size = 2, color = "white")
wb <- wb + ggtitle("Maize Harvest Area in Nigeria") + 
  theme(plot.title=element_text(family="Times", size=18), legend.background = element_rect(fill = "transparent"),  
        legend.position = c(0.94, 0.14), legend.title = (element_text(size = 12, color = "black") ))
maize 





#####Luke's Old Code for Reference using other plotting methods####
#>>>>>>> 93b25dddf25e83e35c32fd65545b50db46e52bfc

# roads <- readOGR(dsn = "shapefiles/described", layer = "roads", stringsAsFactors=FALSE, verbose=FALSE)
# proj4string(roads)
# roads <- spTransform(roads, CRS("+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# plot(roads)
# roads_f <- fortify(roads)

# governorates <- readOGR(dsn = "shapefiles/described", layer = "Governorates", stringsAsFactors=FALSE, verbose=FALSE)
# proj4string(governorates)
# governorates <- spTransform(governorates, CRS("+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# plot(governorates)
# governorates_f <- fortify(governorates)

# roads_proj <- readOGR(dsn = "shapefiles/selected", layer = "INPIIRoadsProjects_Line_modified", stringsAsFactors = FALSE, verbose = FALSE)
# proj4string(roads_proj)
# roads_proj <- spTransform(roads_proj, CRS("+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# plot(roads_proj)
# roads_proj_f <- fortify(roads_proj)

# rdbuff <- readOGR(dsn = "shapefiles/selected", layer = "5km_road_buffer_reprj", stringsAsFactors = FALSE, verbose = FALSE)
# proj4string(rdbuff)
# rdbuff <- spTransform(rdbuff, CRS("+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# plot(rdbuff)
# rdbuff_f <- fortify(rdbuff)

# ###Map####
# rdntwrk <- get_map(location = c(34.21877, 35.57319, 31.22437, 32.55239), zoom = 4, maptype = "watercolor")
# rdntwrk <- ggmap(rdntwrk)
# rdntwrk <- ggplot() + geom_map(data = governorates_f, aes(x=long, y=lat, map_id = id), map = governorates_f, alpha = .5, col = "black")
# # rdntwrk <- rdntwrk + geom_map(data = roads_f, aes(x=long, y=lat, map_id = id), map = roads_f, alpha = 0, col= "blue")
# rdntwrk <- rdntwrk + geom_map(data = roads_proj_f, aes(x=long, y=lat, map_id = id), map = roads_proj_f, alpha = 0, col= "red")
# rdntwrk <- rdntwrk + geom_map(data = rdbuff_f, aes(x=long, y=lat, map_id = id), map = rdbuff_f, alpha = 0, col= "yellow")
# rdntwrk
######
# test_spdf <- as(lightdmsp, "SpatialPixelsDataFrame")
# test_df <- as.data.frame(test_spdf)
# colnames(test_df) <- c("value", "x", "y")
# 
# map4<-ggplot()+  
#   geom_tile(data=test_df, aes(fill = value), alpha=0.8) + 
#   geom_map(data=governorates_f,map=governorates_f,aes(x=long,y=lat,map_id=id))+
#   scale_fill_gradientn(colours = c("grey100","red2"),guide=guide_colourbar(title=NULL),space="Lab",na.value = "grey60")+
#   geom_map(data=roads_proj_f,map=roads_proj_f,aes(x=long,y=lat,map_id=id),color="grey45",alpha=0,size=.2)+
#   geom_map(data=rdbuff_f,map=rdbuff_f,aes(x=long,y=lat,map_id=id),color="red",alpha=0,size=.2)+
#   theme(text = element_text(color = "white"),
#         rect = element_rect(fill = "grey35", color = "grey35"),
#         plot.background = element_rect(fill = "grey35", color = "grey35"),
#         panel.background = element_rect(fill = "grey35", color = "grey35"),
#         plot.title = element_text(),
#         panel.grid = element_blank(),
#         panel.border = element_blank(),
#         axis.text = element_blank(),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         legend.position = c(.1,.2))+
#   ggtitle("West Bank Avg Nighttime Light")
# map4
# 
# gplot(lightdmsp)+geom_map(data=governorates_f,map=governorates_f,aes(x=long,y=lat,map_id=id))+geom_tile(aes(fill = value)) +
#   geom_map(data=governorates_f,map=governorates_f,aes(x=long,y=lat,map_id=id, alpha = 0.5)) +
#   geom_map(data=rdbuff_f,map=rdbuff_f,aes(x=long,y=lat,map_id=id),color="red",alpha=0,size=.2)
# 
# 
# 
# popdata <-raster("~/GitHub/Liberia/gpw-v4-population-count_2010.tif")
# # buffpop <- extract(popdata, rdbuff, df=T)
# # save(buffpop,file = "buffpop_gpw42010.RData")
# 
# load("buffpop_gpw42010.RData")
# 
# 
# sum(buffpop$gpw.v4.population.count_2010[1:128])
# 
# buffpop_sum <- aggregate(. ~ ID, buffpop, sum)
