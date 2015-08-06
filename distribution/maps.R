library(raster)
library(rgdal)
library(maptools)

#install.packages("devtools")
library(devtools)
#install_github("AtlasOfLivingAustralia/ALA4R")
library(plyr) 
library(ALA4R)
ala_config(cache_directory=file.path("c:","mydata","ala_cache")) ## Windows
library(grDevices)

addTrans <- function(color,trans)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.
  
  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))
  
  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}


path<-'c:/git/Tiliqua_rugosa/distribution/'
files<-list.files(paste(path,'raw/',sep=""))

header<-c('site','LONG','LAT','DEVTIME','BIRTHDAY','BIRTHMASS','MONMATURE','MONREPRO','SVLREPRO','FECUNDITY','CLUTCHES','ANNUALACT','MINRESERVE','LASTFOOD','TOTFOOD','FEC1','FEC2','FEC3','FEC4','FEC5','FEC6','FEC7','FEC8','FEC9','FEC10','FEC11','FEC12','FEC13','FEC14','FEC15','FEC16','FEC17','FEC18','FEC19','FEC20','ACT1','ACT2','ACT3','ACT4','ACT5','ACT6','ACT7','ACT8','ACT9','ACT10','ACT11','ACT12','ACT13','ACT14','ACT15','ACT16','ACT17','ACT18','ACT19','ACT20','SUR1','SUR2','SUR3','SUR4','SUR5','SUR6','SUR7','SUR8','SUR9','SUR10','SUR11','SUR12','SUR13','SUR14','SUR15','SUR16','SUR17','SUR18','SUR19','SUR20','MINTB','MAXTB','Pct_Dess','LifeSpan','GenTime','R0','rmax','SVL')

P4S <- CRS("+proj=longlat +datum=WGS84")
sleepy_limit <- readShapeLines(paste(path,"Sleepy_Dist_Limit.shp",sep=""), verbose=TRUE, proj4string=P4S)
aust_bound<-readShapeLines("C:/Spatial_Data/Australia Vector Data/background/ausborder_polyline.shp", verbose=TRUE, proj4string=P4S)

species<-"Tiliqua rugosa"
species_dist=occurrences(taxon=species, download_reason_id=10)
species_dist<-as.data.frame(species_dist$data)


order1<-c(13,6,1,2,5,10,18,16,17)
pctwets<-c('dam',0,0.1,0.2,1,5,5,15,25)
filename<-paste("distribution/maps.pdf",sep="") 
pdf(filename,paper="A4",width=15,height=11) # doing this means you're going to make a pdf - comment this line out if you want to see them in R
par(mfrow = c(4,3)) # set up for 5 plots in 2 columns
par(oma = c(2,2,2,2) + 0.1) # margin spacing stuff
par(mar=rep(0,4)) # margin spacing stuff 
par(mgp = c(3,1,0) ) # margin spacing stuff 

for(i in 1:length(order1)){
  #for(i in 2:2){
  var<-10
  #   if(i==1){
  #   data<-read.csv(paste(path,'raw/',files[13],sep=""),head=FALSE)
  #   colnames(data)<-header
  #   data$longlat<-paste(data$LONG,data$LAT,sep="")
  #   tomap<-aggregate(data[,2:83],by=list(data$longlat),FUN=max)
  #   max<-max(tomap[,var])
  #   min<-min(tomap[,var])
  #   }
  data<-read.csv(paste(path,'raw/',files[order1[i]],sep=""),head=FALSE)
  colnames(data)<-header
  data$longlat<-paste(data$LONG,data$LAT,sep="")
  tomap<-aggregate(data[,2:83],by=list(data$longlat),FUN=max)
  max<-max(tomap[,var])
  min<-min(tomap[,var])
  
  lat1<-min(tomap[,3])-.025 # min latitude
  lat2<-max(tomap[,3])+.025 # max latitude
  lon1<-min(tomap[,2])-.025 # min longitude
  lon2<-max(tomap[,2])+.025 # max longitude
  quadwid<-(lon2-lon1)/.6
  quadlen<-(lat2-lat1)/.6
  gridout <- raster(ncol=quadwid, nrow=quadlen, xmn=lon1, xmx=lon2, ymn=lat1, ymx=lat2)
  
  x<-cbind(tomap$LONG,tomap$LAT) # list of co-ordinates
  grid <- rasterize(x, gridout, tomap[,var])
  grid <- projectRaster(grid, crs="+proj=longlat +datum=WGS84") # change projection here if needed
  #plot(grid,main=files[order1[i]],zlim=c(0,35),ylim=c(-45,-10),xlim=c(105,155))
  plot(grid,zlim=c(0,35),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=FALSE)
  #plot(grid,zlim=c(min,max),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE)
  plot(sleepy_limit, col="black", lwd=1.0,lty=2,add=TRUE)
  plot(aust_bound, col="black", lwd=1.0,add=TRUE)
  if(i==1){
    text(118,-15,"no water")
    text(118,-16.5, "limit")
  }else{
    if(i<7){
      text(118,-15,paste(pctwets[i],"%",sep=""))
    }else{
      text(118,-15,paste(pctwets[i],"%",sep=""))
    }
  }
  #  points(species_dist$longitude, species_dist$latitude, col="black", cex=0.2,pch=16)
  
}
#  mtext(species,outer = TRUE)
dev.off()


order1<-c(18,16,17)
pctwets<-c(5,15,25)

    filename<-paste("distribution/percent_desic_maps.pdf",sep="") 
    pdf(filename,paper="A4",width=15,height=11) # doing this means you're going to make a pdf - comment this line out if you want to see them in R
    par(mfrow = c(3,1)) # set up for 5 plots in 2 columns
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

   text(118,-15,paste(pctwets[i],"% desic",sep=""))

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

