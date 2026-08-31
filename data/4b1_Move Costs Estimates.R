install.packages("elevatr")
install.packages("movecost")
install.packages("sf")
install.packages("rgdal")
install.packages("leastcostpath")
install.packages("sp")
install.packages("raster")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("terra")

library(elevatr)
library(movecost)
library(sf)
library(rgdal)
library(leastcostpath)
library(sp)
library(raster)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(terra)

##### Polities Group 1: No Waterbodies or Rivers in vicinity
setwd("~/Library/CloudStorage/Dropbox/Papers/Murdock Paper/Data Release/Atlas of African Centralized Polities/4_Data Construction Replication/4b_Cost_Allocation Models/4b2_Polities_Grp1_(no water)")

#set up file loop
all.files <- list.files(pattern="\\.csv$")

for(i in 1:length(all.files)) {
  d <- read.csv(all.files[i], stringsAsFactors = FALSE)
  sp <- st_as_sf(d, coords = c("XCOORD", "YCOORD"), crs = 4326)
  sp2 <- as(sp, "Spatial")
  elevation <- get_elev_raster(sp, z = 8, expand = 0.4)
  SixHours <- movebound(dtm=elevation, origin=sp2,  funct="mp",  move=16,  time="h", cont.value = 6)
    d$isolines$Polity <- paste(sp$Polity,"6",sep="_")
    writeOGR(obj=SixHours$isolines, dsn="6h", layer=paste(sp$Polity, '_',"6"),  driver='ESRI Shapefile', overwrite_layer=TRUE)
  EightHours <- movebound(dtm=elevation, origin=sp2,  funct="mp", move=16,  time="h", cont.value = 8)
    d$isolines$Polity <- paste(sp$Polity,"8",sep="_")
    writeOGR(obj=EightHours$isolines, dsn="8h", layer=paste(sp$Polity, '_',"8"),  driver='ESRI Shapefile', overwrite_layer=TRUE)
  TenHours <- movebound(dtm=elevation, origin=sp2,  funct="mp",  move=16,  time="h", cont.value = 10)
    d$isolines$Polity <- paste(sp$Polity,"10",sep="_")
    writeOGR(obj=TenHours$isolines, dsn="10h", layer=paste(sp$Polity, '_',"10"),  driver='ESRI Shapefile', overwrite_layer=TRUE)
}


##### Polities Group 2: Waterbodies, no rivers in vicinity
setwd("~/Library/CloudStorage/Dropbox/Papers/Murdock Paper/Data Release/Atlas of African Centralized Polities/4_Data Construction Replication/4b_Cost_Allocation Models/4b3_Polities_Grp2_(water no river)")

#import water raster
AfricaWater <- raster("Africa_Waterbodies.tif")
plot(AfricaWater)

#set up file loop
all.files <- list.files(pattern="\\.csv$")

for(i in 1:length(all.files)) {
  d <- read.csv(all.files[i], stringsAsFactors = FALSE)
  sp <- st_as_sf(d, coords = c("XCOORD", "YCOORD"), crs = 4326)
  sp2 <- as(sp, "Spatial")
  elevation <- get_elev_raster(sp, z = 8, expand = 0.5)
  Water<-crop(AfricaWater, elevation)
  extent(elevation) <- extent(Water)
  Water2 <- projectRaster(Water, elevation)
  Elevation2 <- overlay(elevation, Water2, fun=function(x,y) {
    x[!is.na(y[])] <-NA
    return(x)})
  plot(Elevation2)
  SixHours <- movebound(dtm=elevation, origin=sp2,  funct="mp",  move=16,  time="h", cont.value = 6)
    d$isolines$Polity <- paste(sp$Polity,"6",sep="_")
    writeOGR(obj=SixHours$isolines, dsn="6h", layer=paste(sp$Polity, '_',"6"),  driver='ESRI Shapefile', overwrite_layer=TRUE)
  EightHours <- movebound(dtm=elevation, origin=sp2,  funct="mp", move=16,  time="h", cont.value = 8)
    d$isolines$Polity <- paste(sp$Polity,"8",sep="_")
    writeOGR(obj=EightHours$isolines, dsn="8h", layer=paste(sp$Polity, '_',"8"),  driver='ESRI Shapefile', overwrite_layer=TRUE)
  TenHours <- movebound(dtm=elevation, origin=sp2,  funct="mp",  move=16,  time="h", cont.value = 10)
    d$isolines$Polity <- paste(sp$Polity,"10",sep="_")
    writeOGR(obj=TenHours$isolines, dsn="10h", layer=paste(sp$Polity, '_',"10"),  driver='ESRI Shapefile', overwrite_layer=TRUE)
}



##### Polities Group 3: Rivers, no waterbodies in vicinity
setwd("~/Library/CloudStorage/Dropbox/Papers/Murdock Paper/Data Release/Atlas of African Centralized Polities/4_Data Construction Replication/4b_Cost_Allocation Models/4b4_Polities_Grp3_(rivers no lakes)")

#import barriers
MajorRivers <- shapefile("Major_Rivers_007_Final.shp")

#set up file loop
all.files <- list.files(pattern="\\.csv$")

for(i in 1:length(all.files)) {
  d <- read.csv(all.files[i], stringsAsFactors = FALSE)
  sp <- st_as_sf(d, coords = c("XCOORD", "YCOORD"), crs = 4326)
  sp2 <- as(sp, "Spatial")
  elevation <- get_elev_raster(sp, z = 8, expand = 0.4)
  SixHours <- movebound(dtm=elevation, origin=sp2,  funct="mp", barrier=MajorRivers, field=0.05, move=16,  time="h", cont.value = 6)
    d$isolines$Polity <- paste(sp$Polity,"6",sep="_")
    writeOGR(obj=SixHours$isolines, dsn="6h", layer=paste(sp$Polity, '_',"6",'_',"0_05"),  driver='ESRI Shapefile', overwrite_layer=TRUE)
  EightHours <- movebound(dtm=elevation, origin=sp2,  funct="mp", barrier=MajorRivers, field=0.05, move=16,  time="h", cont.value = 8)
    d$isolines$Polity <- paste(sp$Polity,"8",sep="_")
    writeOGR(obj=EightHours$isolines, dsn="8h", layer=paste(sp$Polity, '_',"8",'_',"0_05"),  driver='ESRI Shapefile', overwrite_layer=TRUE)
  TenHours <- movebound(dtm=elevation, origin=sp2,  funct="mp", barrier=MajorRivers,  field=0.05, move=16,  time="h", cont.value = 10)
    d$isolines$Polity <- paste(sp$Polity,"10",sep="_")
    writeOGR(obj=TenHours$isolines, dsn="10h", layer=paste(sp$Polity, '_',"10",'_',"0_05"),  driver='ESRI Shapefile', overwrite_layer=TRUE)
}


##### Polities Group 4: Rivers and waterbodies in vicinity
setwd("~/Library/CloudStorage/Dropbox/Papers/Murdock Paper/Data Release/Atlas of African Centralized Polities/4_Data Construction Replication/4b_Cost_Allocation Models/4b4_Polities_Grp4_(rivers and lakes)")

#import barriers
MajorRivers <- shapefile("Major_Rivers_007_Final.shp")

#import water raster
AfricaWater <- raster("Africa_Waterbodies.tif")

#set up file loop
all.files <- list.files(pattern="\\.csv$")

for(i in 1:length(all.files)) {
  d <- read.csv(all.files[i], stringsAsFactors = FALSE)
  sp <- st_as_sf(d, coords = c("XCOORD", "YCOORD"), crs = 4326)
  sp2 <- as(sp, "Spatial")
  elevation <- get_elev_raster(sp, z = 8, expand = 0.4)
  Water<-crop(AfricaWater, elevation)
  extent(elevation) <- extent(Water)
  Water2 <- projectRaster(Water, elevation)
  Elevation2 <- overlay(elevation, Water2, fun=function(x,y) {
    x[!is.na(y[])] <-NA
    return(x)})
  plot(Elevation2)
  SixHours <- movebound(dtm=Elevation2, origin=sp2,  funct="mp", barrier=MajorRivers, field=0.05, move=16,  time="h", cont.value = 6)
    d$isolines$Polity <- paste(sp$Polity,"6",sep="_")
    writeOGR(obj=SixHours$isolines, dsn="6h", layer=paste(sp$Polity, '_',"6",'_',"0_05"),  driver='ESRI Shapefile', overwrite_layer=TRUE)
  EightHours <- movebound(dtm=Elevation2, origin=sp2,  funct="mp", barrier=MajorRivers, field=0.05, move=16,  time="h", cont.value = 8)
    d$isolines$Polity <- paste(sp$Polity,"8",sep="_")
    writeOGR(obj=EightHours$isolines, dsn="8h", layer=paste(sp$Polity, '_',"8",'_',"0_05"),  driver='ESRI Shapefile', overwrite_layer=TRUE)
  TenHours <- movebound(dtm=Elevation2, origin=sp2,  funct="mp", barrier=MajorRivers,  field=0.05, move=16,  time="h", cont.value = 10)
    d$isolines$Polity <- paste(sp$Polity,"10",sep="_")
    writeOGR(obj=TenHours$isolines, dsn="10h", layer=paste(sp$Polity, '_',"10",'_',"0_05"),  driver='ESRI Shapefile', overwrite_layer=TRUE)
}


