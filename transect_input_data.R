# USMX sheep transect data cleanup

# clear the enviroment
rm(list=ls(all=TRUE))

getwd()

library(raster)
library(lubridate)
library(openxlsx)
library(ggplot2)

# read in data from Excel
sheepraw<-read.xlsx("C:/Users/Andrew/Dropbox/antaya/sheepdatabase.xlsx",
                  sheet=1,
                  startRow=1)

#NA's in location must be removed
sheepraw<-sheepraw[!is.na(sheepraw$obslong),]

# make it a spatialpoints object
sheeprawsp<-SpatialPointsDataFrame(cbind(sheepraw$obslong,sheepraw$obslat),data=sheepraw,proj4string= CRS("+init=epsg:4326"))

#save as shapefile
shapefile(sheeprawsp,"D:/Dropbox/antaya/spatialsheep.shp")


#identify columns of interest






