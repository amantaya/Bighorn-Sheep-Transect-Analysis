# USMX sheep transect data cleanup

# clear the enviroment
rm(list=ls(all=TRUE))

setwd('C:/Users/Andrew/Dropbox/antaya')
getwd()

library(raster)
library(lubridate)
library(openxlsx)
library(ggplot2)
library(gdistance)
library(rgeos)


## path options
davespc<-"D:/Dropbox/"
andrewspc<-"C:/Users/Andrew/"

#set current path
mypath<-davespc


# ### this code created the original shapefile from Excel, for editing in ArcMap
# #read in data from Excel
# sheepraw<-read.xlsx(paste0(mypath,"antaya/sheepdatabase.xlsx"),
#                   sheet=1,
#                   startRow=1)
# 
# 
# 
# 
# #NA's in location must be removed
# sheepraw<-sheepraw[!is.na(sheepraw$obslong),]
# 
# # make it a spatialpoints object
# sheeprawsp<-SpatialPointsDataFrame(cbind(sheepraw$obslong,sheepraw$obslat),data=sheepraw,proj4string= CRS("+init=epsg:4326"))
# 
# sheeprawsp@data[(1728-33):1728,]
# 
# #save as shapefile
# #shapefile(sheeprawsp,paste0(mypath,"antaya/spatialsheep.shp"),overwrite=T)
# 
# 
# fewcols<-shapefile(paste0(mypath,"antaya/Sheep_Spatial/spatialsheepnew.shp"))
# 
# 
# #creat new segments
# library(gdistance)
# library(rgeos)
# 
# # transform data to UTM's
# sheepsp<-shapefile(paste0(mypath,"antaya/Sheep_Spatial/spatialsheep.shp"))
# sheepsp<-spTransform(sheepsp, CRS("+init=epsg:32612"))
# 
# # delete unused columns
# trimdata<-sheepsp@data[,-c(13,14,15,16,17,18)]
# names(sheeprawsp@data)<-names(trimdata)[-c(38,39)]
# 
# alldata<-rbind(trimdata,data.frame(sheeprawsp@data[(1728-33):1728,],transectna=NA,sectname=NA))
# 
# allsp<-SpatialPointsDataFrame(cbind(alldata$obslong,alldata$obslat),alldata,proj4string= CRS("+init=epsg:4326"))
# 
# View(allsp@data)
# 
# ## add transect data to last 34 obs
# 
# allsp$transectna[1540:1573]<-"Arch"
# 
# k=1
# for (i in 1:34){
#     if(!is.na(allsp$newsctn[1539+i])){
#         if(allsp$newsctn[1539+i]=="Categorize Previous Section"){
#             allsp$sectname[1539+i]<-k
#             k=k+1
#         }
#         else{
#             allsp$sectname[1539+i]<-k
#         }
#     }
#     else{
#         allsp$sectname[1539+i]<-k
#     }
# 
# }


#shapefile(allsp,paste0(mypath,"antaya/Sheep_Spatial/allsheep.shp"),overwrite=T))

allsp<-shapefile(paste0(mypath,"antaya/Sheep_Spatial/allsheep.shp"))
allsp<-spTransform(allsp, CRS("+init=epsg:32612"))

# matrix of geopgrahic Euclidean distances between each point
gdist<-gDistance(allsp,allsp,byid=T)

# threshold distance to combine segments (in meters)
m<-300
# make logical test of threshold distances between all moints
gdistl<-gdist

for(i in 1:dim(gdist)[1]){
    gdistl[i:dim(gdist)[1],i]<-gdist[i:dim(gdist)[1],i]<m
}

#empty container to hold new section names
newsect<-vector(length=dim(gdist)[1])

# group sections within threshold distance, note that the newsection names are meaningless
for(i in 1: dim(gdistl)[1]){
    if(newsect[i]==FALSE){
        newsect[gdistl[,i]==1]<-i
    }
    else {
    }
}

#append newsection names to data
allsp$occsection<-newsect

allsp$transectna[allsp$transectna=="southtillotson"]<-"tillotson"


#shapefile(sheepsp,paste0(mypath,"antaya/spatialsheepnew.shp"),overwrite=T)
    
  
# data prep  
all<-allsp@data    


# add altitiude to all points
library(elevatr)
allsp$demalt<-get_elev_point(allsp,src="epqs")


# some have no transect name
all<-all[!is.na(all$transectna),]
# some early surveys were recorded differently
all<-all[!is.na(all$newsctn),]

# change white and brown pellet columns to something more tractable
all$whtpllt[all$whtpllt=="<NA>"]<-NA
all$brwnpll[all$brwnpll=="<NA>"]<-NA
all$trshcnt[all$trshcnt=="<NA>"]<-NA



all$white<-!is.na(all$whtpllt)&(all$whtpllt!=0)
all$brown<-!is.na(all$brwnpll)&(all$brwnpll!=0)
all$trash<-1*(!is.na(all$trshcnt)&(all$trshcnt!=0))
all$trash[all$trshcnt=="More than Trace- >10 pieces"]<-2


## read in fecal data<-

samples<-read.xlsx(paste0(mypath,"antaya/Sheep_Fecal_Samples/2018_01_25_Bighorn_Sheep_Fecal_Samples.xlsx"))


## build detection matrix
all$sites<-paste0(all$transectna,all$occsection)
allocc<-data.frame(sites=unique(paste0(all$transectna,all$occsection)))

## some sites survey more than once
all$surveys<-paste(all$transectna,all$obsdate,sep="-")

surveys<-data.frame(aggregate(all$obsdate,by=list(all$transectna),FUN=unique))

surveys<-data.frame(name=as.vector(surveys$Group.1),nsections=unlist(lapply(surveys$x,length)))

# number the surveys
for (i in 1:dim(all)[1]){
    all$nsurvey[i]<-(which(unique(all$surveys[all$transectna==all$transectna[i]]) %in% all$surveys[i])) 
}

# some corrections,
# all alamo surveys occured close in time, collapse to one survey
all$nsurvey[all$transectna=="Alamo"]<-1
all$nsurvey[all$transectna=="MonHill"&all$obsdate==43083]<-2
all$nsurvey[all$transectna=="MonHill"&all$obsdate==43083]<-2
all$nsurvey[all$transectna=="puertos"&all$obsdate %in% c(43050,43051,43054)]<-2
all$nsurvey[all$transectna=="QuitoSouth"&all$obsdate %in% c(43083,43119)]<-2





# occupancy for each survey
allocc$y1<-NA
allocc$y2<-NA
allocc$y3<-NA
allocc$y4<-NA

for (i in 1:dim(allocc)[1]){ 
    for(j in 1:4){
        if(j==1&max(all$nsurvey[all$site==allocc$sites[i]])>=j){
            allocc$y1[i]<-as.numeric(sum(all$white[all$site==allocc$sites[i]&all$nsurvey==j])>0)
            allocc$y2[i]<-as.numeric(sum(all$brown[all$site==allocc$sites[i]&all$nsurvey==j])>0) 
        }
        if(j==2&max(all$nsurvey[all$site==allocc$sites[i]])>=j){
            allocc$y3[i]<-as.numeric(sum(all$white[all$site==allocc$sites[i]&all$nsurvey==j])>0)
            allocc$y4[i]<-as.numeric(sum(all$brown[all$site==allocc$sites[i]&all$nsurvey==j])>0) 
        }
        else{
            
        }
    }
}

 
## site covariates

# median of all locations at a site
allocc<-allocc[order(allocc$sites),]
allocc$obslong<-c(unlist(tapply(all$obslong,all$sites,median)))
allocc$obslat<-c(unlist(tapply(all$obslat,all$sites,median)))
allocc$alt<-c(unlist(tapply(all$obsalt,all$sites,median)))

allocc$trash<-c(unlist(tapply(all$trash,all$sites,mean)))

# if you want trash to be a binomial
#allocc$trash<-allocc$trash>0

# add nation
allocc$nation<-allocc$obslong>(-113.210)




# or you could use the first location at a site
#allocc$long<-all$obslong[match(allocc$sites,all$sites)]


 
alloccsp<-SpatialPointsDataFrame(cbind(allocc$obslong,allocc$obslat),allocc[,c(1,5:10)], proj4string= CRS("+init=epsg:4326"))  

#shapefile(alloccsp,paste0(mypath,"antaya/Sheep_Spatial/alloccshp.shp"),overwrite=T)
#write.csv(allocc,paste0(mypath,"antaya/allocc.csv")) 
    

writeClipboard(unique(all$transectna))    



segmentcovs<-read.xlsx(paste0(mypath,"antaya/OccCov.xlsx"))
    

# add elevatoin of the start of each segment
#dem<-raster(paste0(mypath,"antaya/DEM/"))

## this code extracts elevation for all points

#allocc$altitude<-extract(dem,alloccsp)
library(elevatr)
elevs<-get_elev_point(alloccsp,src="epqs")

allocc$demalt<-elevs$elevation



allocc$transect<-all$transectna[match(allocc$sites,all$sites)]

# find height above the lowest point on each transect
for (i in 1:length(allocc$demalt)){
    allocc$height[i]<-allocc$demalt[i]-min(allocc$demalt[allocc$transect==allocc$transect[i]])
}

## add max height on transect
for (i in 1:length(allocc$demalt)){
    allocc$maxheight[i]<-max(allsp$demalt$elevation[allsp$transectna==allocc$transect[i]],na.rm=T)-min(allsp$demalt$elevation[all2$transectna==allocc$transect[i]],na.rm=T)
}

## add size of features (copy size from thesis into clipboard)
#size<-data.frame(size=readClipboard(),transect=unique(allocc$transect))
#allocc$size<-size$size[match(allocc$transect,size$transect)]




# report the number of unique segments in all data
# make a table of unique segments by transect
transectsections<-as.data.frame(aggregate(all$occsection,by=list(all$transectna),FUN=unique,simplify=T))
# make a dataframe to hold the name of each transect and the number of new sections on that transect
transects<-data.frame(name=as.vector(transectsections$Group.1),nsections=unlist(lapply(unique(transectsections$x),length)))
write.table(transects,"clipboard",sep="\t",row.names=F)


#shapefile(alloccsp,paste0(mypath,"antaya/Sheep_Spatial/alloccshp.shp"),overwrite=T)
#write.csv(allocc,paste0(mypath,"antaya/allocc.csv")) 



