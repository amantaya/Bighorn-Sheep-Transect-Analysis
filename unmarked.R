library(unmarked)

nsites<-length(allocc$sites)
nsurv<-4

# this needs to have the same number of rows as number of sites, and in the same order
sitecovs<-data.frame(nation=allocc$nation,trash=allocc$trash,elevation=allocc$alt)

# this needs to have the same number of rows as number of sites, and in the same order
# this also nees one column for each sampling period (4) for each obscovariate 
color<-data.frame(y1.col=rep("white",nsites),y2.col=rep("brown",nsites),
                  y3.col=rep("white",nsites),y4.col=rep("brown",nsites))

nobs<-matrix(1,nrow=nsites,ncol=nsurv)

time<-matrix(c(1,1,2,2),nrow=nsites,ncol=nsurv,byrow=T)




obscovs<-list(color=color,nobs=nobs,time=time)

#names(obscovs)<-c("y1.col","y2.col","y3.col","y4.col",
#                  "y1.nobs","y2.nobs","y3.nobs","y4.nobs",
#                  "y1.time","y2.time","y3.time","y4.time")



umf <- unmarkedFrameOccu(y = allocc[,2:5],obsCovs=obscovs,siteCovs=sitecovs)      # Create unmarked data frame

summary(umf)                                        # Summarize data frame


(fm1 <- occu(???color*as.factor(time) ???nationtrash+scale(elevation), data = umf))   # Fit model

backTransform(fm1,"state")
backTransform(fm1,"det")
