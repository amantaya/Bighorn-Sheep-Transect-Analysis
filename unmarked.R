
# clear the enviroment
rm(list=ls(all=TRUE))

library(unmarked)

## path options
davespc<-"D:/Dropbox/"
andrewspc<-"C:/Users/Andrew/"

#set current path
mypath<-davespc

allocc<-read.csv(paste0(mypath,"antaya/allocc.csv"))

nsites<-length(allocc$sites)
nsurv<-4


## recode size to better

# this needs to have the same number of rows as number of sites, and in the same order
sitecovs<-data.frame(nation=allocc$nation,trash=allocc$trash,relheight=allocc$height/allocc$maxheight,maxheight=allocc$maxheight)
sitecovs$relheight[is.nan(sitecovs$relheight)]<-0

## need to scale and transform before putting into unmarked frame
sitecovs$relheight<-scale(sitecovs$relheight)

sitecovs$maxheight[sitecovs$maxheight==0]<-10
sitecovs$maxheight<-scale(log(sitecovs$maxheight))

hist(sitecovs$maxheight)

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



umf <- unmarkedFrameOccu(y = allocc[,3:6],obsCovs=obscovs,siteCovs=sitecovs)      # Create unmarked data frame

summary(umf)                                        # Summarize data frame


(fm1 <- occu(~color*as.factor(time) ~nation*maxheight+trash + nation*relheight, data = umf))   # Fit model



backTransform(fm1,"state")
backTransform(fm1,"det")

lcmex <- linearComb(fm1, c(1,0,0,0,0,0,0), type="state") # Estimate abundance on the log scale when forest=0
backTransform(lcmex) 

lcusa <- linearComb(fm1, c(1,1,0,0,0,0,0), type="state") # Estimate abundance on the log scale when forest=0
backTransform(lcusa) 

## backtransformed estimates of detection 
lcdetwhite <- linearComb(fm1, c(1,1,0,0), type="det") # Estimate abundance on the log scale when forest=0
backTransform(lcdetwhite) 
lcdetbrown <- linearComb(fm1, c(1,0,0,0), type="det") # Estimate abundance on the log scale when forest=0
backTransform(lcdetbrown) 
lcdetwhite2 <- linearComb(fm1, c(1,1,1,1), type="det") # Estimate abundance on the log scale when forest=0
backTransform(lcdetwhite2) 
lcdetbrown2 <- linearComb(fm1, c(1,0,1,0), type="det") # Estimate abundance on the log scale when forest=0
backTransform(lcdetbrown2) 




just()

plogis(1*1.605+1*(-1.227)+1*(-0.288)+1*(-0.814)+1*(1.645)+1*(1.654)+1*(-1.809))

confint(fm1,type="state")

# overlapping histograms
hist(sitecovs$maxheight[sitecovs$nation==TRUE],col=rgb(1,0,0,0.5))
hist(sitecovs$maxheight[sitecovs$nation==FALSE],col=rgb(0,0,1,0.5),add=T)


## create new data to plot

newdat<-data.frame(trash=seq(0,2,by=0.1),nation=1,maxheight=0,relheight=0)
predtrash<-predict(fm1,type="state",newdata=newdat,appendData=T,level=0.90)

library(jpeg)
jpeg(paste0(mypath,"antaya/Thesis/predictedtrash.jpg"),width=6.6,height=6.5,units="in",res=600)
    par(mar=c(5,5,1,1))
    plot(x=as.numeric(as.character(predtrash$trash)),y= predtrash$Predicted,type="n",ylim=c(0,0.8),
         cex.axis=1.25,cex.lab=1.25,
         xlab="trash index",ylab="Occupancy (??)")
    lines(predtrash$trash, predtrash$Predicted,col="red")
    #lines(predtrash$trash, predtrash$lower,lty=2)
    #lines(predtrash$trash, predtrash$upper,lty=2)
    axis(1,at=c(0,1,2),labels=c("no trash","trace",">trace"),line=1,lwd=0)
    polygon(c(predtrash$trash,rev(predtrash$trash)),c(predtrash$lower,rev(predtrash$upper)),col=rgb(1,0,0,0.5),border=NA)
    
dev.off()


## create new data to plot

newdat2<-data.frame(trash=0,nation=c(rep(0,20),rep(1,20)),maxheight=0,relheight=seq(-2,2,length.out=20))
prednationrel<-predict(fm1,type="state",newdata=newdat2,appendData=T,level=0.90)

library(jpeg)
jpeg(paste0(mypath,"antaya/Thesis/predictednationrel.jpg"),width=6.6,height=6.5,units="in",res=600)
par(mar=c(5,5,1,1))
plot(x=as.numeric(as.character(prednationrel$relheight)),y= prednationrel$Predicted,type="n",ylim=c(0,1),
     cex.axis=1.25,cex.lab=1.25,
     xlab="relative height",ylab="Occupancy (??)")
with(prednationrel[prednationrel$nation==0,],lines(relheight, Predicted,col="green"))
with(prednationrel[prednationrel$nation==1,],lines(relheight, Predicted,col="blue"))
with(prednationrel[prednationrel$nation==0,],polygon(c(relheight,rev(relheight)),c(lower,rev(upper)),col=rgb(0,1,0,0.5),border=NA))
with(prednationrel[prednationrel$nation==1,],polygon(c(relheight,rev(relheight)),c(lower,rev(upper)),col=rgb(0,0,1,0.5),border=NA))
legend(0,0.2,fill=c(rgb(0,1,0,0.5),rgb(0,0,1,0.5)),lty=c(1,1),col=c("green","blue"),legend=c("Mexico","USA"),bty="n",cex=1.25,border=NA)

#lines(predtrash$trash, predtrash$lower,lty=2)
#lines(predtrash$trash, predtrash$upper,lty=2)
#axis(1,at=c(0,1,2),labels=c("0","trace",">trace"),line=1.1,lwd=0)
#polygon(c(predtrash$trash,rev(predtrash$trash)),c(predtrash$lower,rev(predtrash$upper)),col=rgb(0,0,1,0.5),border=NA)

dev.off()

## create new data to plot

newdat3<-data.frame(trash=0,nation=c(rep(0,20),rep(1,20)),relheight=0,maxheight=seq(-2,2,length.out=20))
prednationmax<-predict(fm1,type="state",newdata=newdat3,appendData=T,level=0.90)

library(jpeg)
jpeg(paste0(mypath,"antaya/Thesis/predictednationmax.jpg"),width=6.6,height=6.5,units="in",res=600)
par(mar=c(5,5,1,1))
plot(x=as.numeric(as.character(prednationmax$maxheight)),y= prednationmax$Predicted,type="n",ylim=c(0,1),
     cex.axis=1.25,cex.lab=1.25,
     xlab="maximum height",ylab="Occupancy (??)")
with(prednationmax[prednationmax$nation==0,],lines(maxheight, Predicted,col="green"))
with(prednationmax[prednationmax$nation==1,],lines(maxheight, Predicted,col="blue"))
with(prednationmax[prednationmax$nation==0,],polygon(c(maxheight,rev(maxheight)),c(lower,rev(upper)),col=rgb(0,1,0,0.5),border=NA))
with(prednationmax[prednationmax$nation==1,],polygon(c(maxheight,rev(maxheight)),c(lower,rev(upper)),col=rgb(0,0,1,0.5),border=NA))
legend(0,0.2,fill=c(rgb(0,1,0,0.5),rgb(0,0,1,0.5)),lty=c(1,1),col=c("green","blue"),legend=c("Mexico","USA"),bty="n",cex=1.25,border=NA)

#lines(predtrash$trash, predtrash$lower,lty=2)
#lines(predtrash$trash, predtrash$upper,lty=2)
#axis(1,at=c(0,1,2),labels=c("0","trace",">trace"),line=1.1,lwd=0)
#polygon(c(predtrash$trash,rev(predtrash$trash)),c(predtrash$lower,rev(predtrash$upper)),col=rgb(0,0,1,0.5),border=NA)

dev.off()
