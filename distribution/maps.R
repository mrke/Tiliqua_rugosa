library(raster)
library(rgdal)
library(maptools)
path<-'c:/git/Tiliqua_rugosa/distribution/'
files<-list.files(paste(path,'raw/',sep=""))

header<-c('site','LONG','LAT','DEVTIME','BIRTHDAY','BIRTHMASS','MONMATURE','MONREPRO','SVLREPRO','FECUNDITY','CLUTCHES','ANNUALACT','MINRESERVE','LASTFOOD','TOTFOOD','FEC1','FEC2','FEC3','FEC4','FEC5','FEC6','FEC7','FEC8','FEC9','FEC10','FEC11','FEC12','FEC13','FEC14','FEC15','FEC16','FEC17','FEC18','FEC19','FEC20','ACT1','ACT2','ACT3','ACT4','ACT5','ACT6','ACT7','ACT8','ACT9','ACT10','ACT11','ACT12','ACT13','ACT14','ACT15','ACT16','ACT17','ACT18','ACT19','ACT20','SUR1','SUR2','SUR3','SUR4','SUR5','SUR6','SUR7','SUR8','SUR9','SUR10','SUR11','SUR12','SUR13','SUR14','SUR15','SUR16','SUR17','SUR18','SUR19','SUR20','MINTB','MAXTB','Pct_Dess','LifeSpan','GenTime','R0','rmax','SVL')

P4S <- CRS("+proj=longlat +datum=WGS84")
sleepy_limit <- readShapeLines(paste(path,"Sleepy_Dist_Limit.shp",sep=""), verbose=TRUE, proj4string=P4S)
aust_bound<-readShapeLines("C:/Spatial_Data/Australia Vector Data/background/ausborder_polyline.shp", verbose=TRUE, proj4string=P4S)
T_scincoides<-read.csv(paste(path,'Tiliqua_scincoides.csv',sep=""))
T_occipitalis<-read.csv(paste(path,'Tiliqua_occipitalis.csv',sep=""))
T_multifasciata<-read.csv(paste(path,'Tiliqua_multifasciata.csv',sep=""))

order1<-c(6,1,2,3,4,5,7,9,10,11,12,8)
pctwets<-c(0,0.1,0.2,0.3,0.4,0.5,1,2,3,4,5,10)

    filename<-paste("distribution/percent_wet_maps.pdf",sep="") 
    pdf(filename,paper="A4",width=15,height=11) # doing this means you're going to make a pdf - comment this line out if you want to see them in R
    par(mfrow = c(4,3)) # set up for 5 plots in 2 columns
    par(oma = c(2,2,2,2) + 0.1) # margin spacing stuff
    par(mar=rep(0,4)) # margin spacing stuff 
    par(mgp = c(3,1,0) ) # margin spacing stuff 

for(i in 1:length(order1)){
  data<-read.csv(paste(path,'raw/',files[order1[i]],sep=""),head=FALSE)
  colnames(data)<-header
  data$longlat<-paste(data$LONG,data$LAT,sep="")
  tomap<-aggregate(data[,2:10],by=list(data$longlat),FUN=max)

lat1<-min(tomap[,3])-.025 # min latitude
lat2<-max(tomap[,3])+.025 # max latitude
lon1<-min(tomap[,2])-.025 # min longitude
lon2<-max(tomap[,2])+.025 # max longitude
quadwid<-(lon2-lon1)/.6
quadlen<-(lat2-lat1)/.6
gridout <- raster(ncol=quadwid, nrow=quadlen, xmn=lon1, xmx=lon2, ymn=lat1, ymx=lat2)

    x<-cbind(tomap$LONG,tomap$LAT) # list of co-ordinates
  grid <- rasterize(x, gridout, tomap[,10])
  grid <- projectRaster(grid, crs="+proj=longlat +datum=WGS84") # change projection here if needed
  #plot(grid,main=files[order1[i]],zlim=c(0,35),ylim=c(-45,-10),xlim=c(105,155))
  plot(grid,zlim=c(0,35),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=FALSE)
  plot(sleepy_limit, col="black", lwd=1.0,lty=2,add=TRUE)
  plot(aust_bound, col="black", lwd=1.0,add=TRUE)
  text(118,-15,paste(pctwets[i],"% wet",sep=""))
#   points(T_scincoides$Long,T_scincoides$Lat,col='black',cex=0.5,pch=16)
#   points(T_occipitalis$long,T_occipitalis$lat,col='blue',cex=0.5,pch=16)
#   points(T_multifasciata$long,T_multifasciata$lat,col='red',cex=0.5,pch=16)
}
dev.off()


order1<-c(13,18,14,16,17,19,15)
pctwets<-c('dam',5,10,15,25,50,100)

    filename<-paste("distribution/percent_desic_maps.pdf",sep="") 
    pdf(filename,paper="A4",width=15,height=11) # doing this means you're going to make a pdf - comment this line out if you want to see them in R
    par(mfrow = c(3,2)) # set up for 5 plots in 2 columns
    par(oma = c(2,2,2,2) + 0.1) # margin spacing stuff
    par(mar=rep(0,4)) # margin spacing stuff 
    par(mgp = c(3,1,0) ) # margin spacing stuff 

for(i in 1:length(order1)){
  data<-read.csv(paste(path,'raw/',files[order1[i]],sep=""),head=FALSE)
  colnames(data)<-header
  data$longlat<-paste(data$LONG,data$LAT,sep="")
  tomap<-aggregate(data[,2:10],by=list(data$longlat),FUN=max)

lat1<-min(tomap[,3])-.025 # min latitude
lat2<-max(tomap[,3])+.025 # max latitude
lon1<-min(tomap[,2])-.025 # min longitude
lon2<-max(tomap[,2])+.025 # max longitude
quadwid<-(lon2-lon1)/.6
quadlen<-(lat2-lat1)/.6
gridout <- raster(ncol=quadwid, nrow=quadlen, xmn=lon1, xmx=lon2, ymn=lat1, ymx=lat2)

    x<-cbind(tomap$LONG,tomap$LAT) # list of co-ordinates
  grid <- rasterize(x, gridout, tomap[,10])
  grid <- projectRaster(grid, crs="+proj=longlat +datum=WGS84") # change projection here if needed
  #plot(grid,main=files[order1[i]],zlim=c(0,35),ylim=c(-45,-10),xlim=c(105,155))
  plot(grid,zlim=c(0,35),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=FALSE)
  plot(sleepy_limit, col="black", lwd=1.0,lty=2,add=TRUE)
  plot(aust_bound, col="black", lwd=1.0,add=TRUE)
  if(i==1){
   text(118,-15,"no water")
   text(118,-16.5, "limit")
  }else{
   text(118,-15,paste(pctwets[i],"% desic",sep=""))
  }
#   points(T_scincoides$Long,T_scincoides$Lat,col='black',cex=0.5,pch=16)
#   points(T_occipitalis$long,T_occipitalis$lat,col='blue',cex=0.5,pch=16)
#   points(T_multifasciata$long,T_multifasciata$lat,col='red',cex=0.5,pch=16)
}
dev.off()

    filename<-paste("distribution/bluetongues.pdf",sep="") 
    pdf(filename,paper="A4",width=15,height=11) # doing this means you're going to make a pdf - comment this line out if you want to see them in R
    par(mfrow = c(2,1)) # set up for 5 plots in 2 columns
    par(mar=rep(0,4)) # margin spacing stuff 
    par(mgp = c(3,1,0) ) # margin spacing stuff 

i<-1
  data<-read.csv(paste(path,'raw/',files[i],sep=""),head=FALSE)
  colnames(data)<-header
  data$longlat<-paste(data$LONG,data$LAT,sep="")
  tomap<-aggregate(data[,2:10],by=list(data$longlat),FUN=max)

lat1<-min(tomap[,3])-.025 # min latitude
lat2<-max(tomap[,3])+.025 # max latitude
lon1<-min(tomap[,2])-.025 # min longitude
lon2<-max(tomap[,2])+.025 # max longitude
quadwid<-(lon2-lon1)/.6
quadlen<-(lat2-lat1)/.6
gridout <- raster(ncol=quadwid, nrow=quadlen, xmn=lon1, xmx=lon2, ymn=lat1, ymx=lat2)

    x<-cbind(tomap$LONG,tomap$LAT) # list of co-ordinates
  grid <- rasterize(x, gridout, tomap[,10])
  grid <- projectRaster(grid, crs="+proj=longlat +datum=WGS84") # change projection here if needed
  #plot(grid,main=files[order1[i]],zlim=c(0,35),ylim=c(-45,-10),xlim=c(105,155))
  plot(grid,zlim=c(0,35),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=FALSE)
  plot(sleepy_limit, col="black", lwd=1.0,lty=2,add=TRUE)
  plot(aust_bound, col="black", lwd=1.0,add=TRUE)

   points(T_scincoides$Long,T_scincoides$Lat,col='black',cex=0.5,pch=16)
   points(T_occipitalis$long,T_occipitalis$lat,col='blue',cex=0.5,pch=16)
   

  plot(grid,zlim=c(0,35),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=FALSE)
  plot(sleepy_limit, col="black", lwd=1.0,lty=2,add=TRUE)
  plot(aust_bound, col="black", lwd=1.0,add=TRUE)

points(T_multifasciata$long,T_multifasciata$lat,col='red',cex=0.5,pch=16)

dev.off()

