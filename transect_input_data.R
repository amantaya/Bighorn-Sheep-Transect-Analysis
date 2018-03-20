# USMX sheep transect data cleanup

# clear the enviroment
rm(list=ls(all=TRUE))

setwd('C:/Users/Andrew/Dropbox/antaya')
getwd()

library(raster)
library(lubridate)
library(openxlsx)
library(ggplot2)


## path options
davespc<-"D:/Dropbox/"
andrewspc<-"C:/Users/Andrew/"

#set current path
mypath<-davespc

# read in data from Excel
sheepraw<-read.xlsx(paste0(mypath,"antaya/sheepdatabase.xlsx"),
                  sheet=1,
                  startRow=1)






#NA's in location must be removed
sheepraw<-sheepraw[!is.na(sheepraw$obslong),]

# make it a spatialpoints object
sheeprawsp<-SpatialPointsDataFrame(cbind(sheepraw$obslong,sheepraw$obslat),data=sheepraw,proj4string= CRS("+init=epsg:4326"))

#save as shapefile
shapefile(sheeprawsp,paste0(mypath,"antaya/spatialsheepraw.shp"),overwrite=T)


#identify columns of interest






