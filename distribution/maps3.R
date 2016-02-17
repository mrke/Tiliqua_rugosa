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

path<-'c:/git/Tiliqua_rugosa/distribution/'
files<-list.files(paste(path,'raw/',sep=""))

header<-c('site','LONG','LAT','DEVTIME','BIRTHDAY','BIRTHMASS','MONMATURE','MONREPRO','SVLREPRO','FECUNDITY','CLUTCHES','ANNUALACT','MINRESERVE','LASTFOOD','TOTFOOD','MINTB','MAXTB','Pct_Dess','LifeSpan','GenTime','R0','rmax','SVL')

P4S <- CRS("+proj=longlat +datum=WGS84")
sleepy_limit <- readShapeLines(paste(path,"Sleepy_Dist_Limit.shp",sep=""), verbose=TRUE, proj4string=P4S)
aust_bound<-readShapeLines("C:/Spatial_Data/Australia Vector Data/background/ausborder_polyline.shp", verbose=TRUE, proj4string=P4S)

#1	n		      #26	CauseDeath
#2	LONG		  #27	tLay
#3	LAT		    #28	tEgg
#4	YEAR		  #29	tStg1
#5	MaxStg    #30	tStg2
#6	MaxWgt	  #31	tStg3
#7	MaxLen	  #32	tStg4
#8	Tmax		  #33	tStg5
#9	Tmin		  #34	tStg6
#10	MinRes	  #35	tStg7
#11	MaxDess	  #36	tStg8
#12	MinShade  #37	mStg1
#13	MaxShade	#38	mStg2
#14	MinDep		#39	mStg3
#15	MaxDep		#40	mStg4
#16	Bsk		    #41	mStg5
#17	Forage		#42	mStg6
#18	Dist	  	#43	mStg7
#19	Food	  	#44	mStg8
#20	Drink	  	#45	surviv
#21	NWaste  	#46	ovip_surviv
#22	Feces	  	#47	fitness
#23	O2		    #48	deathstage
#24	Clutch		#49	cohort
#25	Fec			

#    causedeath
#    0    no death
#    1    cold
#    2    heat
#    3    dess
#    4    starve
#    5    ageing

filename<-paste("distribution/maps.pdf",sep="") 
pdf(filename,paper="A4r",width=15,height=11) # doing this means you're going to make a pdf - comment this line out if you want to see them in R
par(mfrow = c(4,4)) # set up for 5 plots in 2 columns
par(oma = c(2,2,2,3) + 0.1) # margin spacing stuff
par(mar=rep(0,4)) # margin spacing stuff 
par(mgp = c(3,1,0) ) # margin spacing stuff 
fileorder<-c(7,6,5,8)

for(filenum in fileorder){
  # dam repro
  data2<-read.csv(paste(path,'raw/',files[filenum],sep=""),head=FALSE)
  cohort<-rep(seq(1,20),20)
  cohort<-cohort[order(cohort)]
  cohort<-rep(cohort,(nrow(data2)/400))
  data2$cohort<-cohort[1:nrow(data2)]
  yearsout.names<-c("n","LONG","LAT","YEAR","MaxStg","MaxWgt","MaxLen","Tmax","Tmin","MinRes","MaxDess","MinShade","MaxShade","MinDep","MaxDep","Bsk","Forage","Dist","Food","Drink","NWaste","Feces","O2","Clutch","Fec","CauseDeath","tLay","tEgg","tStg1","tStg2","tStg3","tStg4","tStg5","tStg6","tStg7","tStg8","mStg1","mStg2","mStg3","mStg4","mStg5","mStg6","mStg7","mStg8","surviv","ovip_surviv","fitness","deathstage","cohort")
  colnames(data2)<-yearsout.names
  data2<-subset(data2,MaxWgt>0)
  
  # age
  var<-4
  
  for(i in 1:20){
    data3<-subset(data2,data2$cohort==i)
    #data3<-subset(data3,data3$CauseDeath==0)
    data3$MaxDep<-data3$MaxDep*-1
    #data3<-subset(data3,data3$CauseDeath==0)
    data3$longlat<-paste(data3$LONG,data3$LAT,sep="")
    tomap<-cbind(aggregate(data3[,2:3],by=list(data3$longlat),FUN=max),aggregate(data3[,4:49],by=list(data3$longlat),FUN=max)[,2:45])
    #tomap<-subset(tomap,tomap$Clutch>0)
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
    if(i==1){
      grids<-grid
    }else{
      grids<-stack(grids,grid)
    }
    cat(i,' \n')
  }  
  
  # if(filenum==fileorder[1]){
  #  plot(mean(grids),zlim=c(0,21),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=TRUE) 
  # }else{
  #  plot(mean(grids),zlim=c(0,21),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=FALSE) 
  # }
  
  age<-mean(grids)
  ages<-grids
  
  # repro
  var<-25
  
  for(i in 1:20){
    data3<-subset(data2,data2$cohort==i)
    #data3<-subset(data3,data3$CauseDeath==0)
    data3$MaxDep<-data3$MaxDep*-1
    #data3<-subset(data3,data3$CauseDeath==0)
    data3$longlat<-paste(data3$LONG,data3$LAT,sep="")
    tomap<-cbind(aggregate(data3[,2:3],by=list(data3$longlat),FUN=max),aggregate(data3[,4:49],by=list(data3$longlat),FUN=sum)[,2:45])
    #tomap<-subset(tomap,tomap$Clutch>0)
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
    if(i==1){
      grids<-grid
    }else{
      grids<-stack(grids,grid)
    }
    cat(i,' \n')
  }  
  
  if(filenum==fileorder[1]){
    plot(sum(grids)/20,zlim=c(1,32),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=TRUE) 
  }else{
    plot(sum(grids)/20,zlim=c(1,32),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=FALSE) 
  }
  plot(aust_bound, col="black", lwd=1.0,add=TRUE)  
  species<-"Tiliqua rugosa"
  species_dist=occurrences(taxon=species, download_reason_id=10)
  species_dist<-as.data.frame(species_dist$data)
  points(species_dist$longitude, species_dist$latitude, col="black", cex=0.2,pch=16)
  
  # activity
  
  for(i in 1:20){
    data3<-subset(data2,data2$cohort==i)
    #data3<-subset(data3,data3$CauseDeath==0)
    data3$MaxDep<-data3$MaxDep*-1
    #data3<-subset(data3,data3$CauseDeath==0)
    data3$longlat<-paste(data3$LONG,data3$LAT,sep="")
    tomap<-cbind(aggregate(data3[,2:3],by=list(data3$longlat),FUN=max),aggregate(data3[,4:49],by=list(data3$longlat),FUN=sum)[,2:45])
    
    #tomap<-subset(tomap,tomap$Clutch>0)
    max<-max(tomap[,16])+max(tomap[,17])
    min<-min(tomap[,16])+max(tomap[,17])
    
    lat1<-min(tomap[,3])-.025 # min latitude
    lat2<-max(tomap[,3])+.025 # max latitude
    lon1<-min(tomap[,2])-.025 # min longitude
    lon2<-max(tomap[,2])+.025 # max longitude
    quadwid<-(lon2-lon1)/.6
    quadlen<-(lat2-lat1)/.6
    gridout <- raster(ncol=quadwid, nrow=quadlen, xmn=lon1, xmx=lon2, ymn=lat1, ymx=lat2)
    
    x<-cbind(tomap$LONG,tomap$LAT) # list of co-ordinates
    grid <- rasterize(x, gridout, tomap[,16]+tomap[,17])
    grid <- projectRaster(grid, crs="+proj=longlat +datum=WGS84") # change projection here if needed
    if(i==1){
      grids<-grid
    }else{
      grids<-stack(grids,grid)
    }
    cat(i,' \n')
  }  
  
  if(filenum==fileorder[1]){
    plot(mean(grids/ages),zlim=c(0,4500),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=TRUE) 
  }else{
    plot(mean(grids/ages),zlim=c(0,4500),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=FALSE) 
  }
  plot(aust_bound, col="black", lwd=1.0,add=TRUE)  
  #points(species_dist$longitude, species_dist$latitude, col="black", cex=0.2,pch=16)
  
  
  # mass
  var<-6
  
  for(i in 1:20){
    data3<-subset(data2,data2$cohort==i)
    #data3<-subset(data3,data3$CauseDeath==0)
    data3$MaxDep<-data3$MaxDep*-1
    #data3<-subset(data3,data3$CauseDeath==0)
    data3$longlat<-paste(data3$LONG,data3$LAT,sep="")
    tomap<-cbind(aggregate(data3[,2:3],by=list(data3$longlat),FUN=max),aggregate(data3[,4:49],by=list(data3$longlat),FUN=max)[,2:45])
    #tomap<-subset(tomap,tomap$Clutch>0)
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
    if(i==1){
      grids<-grid
    }else{
      grids<-stack(grids,grid)
    }
    cat(i,' \n')
  }  
  
  if(filenum==fileorder[1]){
    plot(sum(grids)/20/1000,zlim=c(.100,1.500),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=TRUE) 
  }else{
    plot(sum(grids)/20/1000,zlim=c(.100,1.500),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=FALSE) 
  }
  plot(aust_bound, col="black", lwd=1.0,add=TRUE)  
  #points(species_dist$longitude, species_dist$latitude, col="black", cex=0.2,pch=16)
  
  # depth
  var<-15
  
  for(i in 1:20){
    data3<-subset(data2,data2$cohort==i)
    #data3<-subset(data3,data3$CauseDeath==0)
    data3$MaxDep<-data3$MaxDep*-1
    #data3<-subset(data3,data3$CauseDeath==0)
    data3$longlat<-paste(data3$LONG,data3$LAT,sep="")
    tomap<-cbind(aggregate(data3[,2:3],by=list(data3$longlat),FUN=max),aggregate(data3[,4:49],by=list(data3$longlat),FUN=max)[,2:45])
    #tomap<-subset(tomap,tomap$Clutch>0)
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
    if(i==1){
      grids<-grid
    }else{
      grids<-stack(grids,grid)
    }
    cat(i,' \n')
  }  
  
  if(filenum==fileorder[1]){
    plot(sum(grids)/20/100,zlim=c(0,2),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=TRUE) 
  }else{
    plot(sum(grids)/20/100,zlim=c(0,2),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=FALSE) 
  }
  plot(aust_bound, col="black", lwd=1.0,add=TRUE)  
  #points(species_dist$longitude, species_dist$latitude, col="black", cex=0.2,pch=16)
  
  
  
}

dev.off()



# Tiliqua, Egernia, Liopholis

filenum=7

data2<-read.csv(paste(path,'raw/',files[filenum],sep=""),head=FALSE)
cohort<-rep(seq(1,20),20)
cohort<-cohort[order(cohort)]
cohort<-rep(cohort,(nrow(data2)/400))
data2$cohort<-cohort[1:nrow(data2)]
yearsout.names<-c("n","LONG","LAT","YEAR","MaxStg","MaxWgt","MaxLen","Tmax","Tmin","MinRes","MaxDess","MinShade","MaxShade","MinDep","MaxDep","Bsk","Forage","Dist","Food","Drink","NWaste","Feces","O2","Clutch","Fec","CauseDeath","tLay","tEgg","tStg1","tStg2","tStg3","tStg4","tStg5","tStg6","tStg7","tStg8","mStg1","mStg2","mStg3","mStg4","mStg5","mStg6","mStg7","mStg8","surviv","ovip_surviv","fitness","deathstage","cohort")
colnames(data2)<-yearsout.names
data2<-subset(data2,MaxWgt>0)

# repro
var<-25

for(i in 1:20){
  data3<-subset(data2,data2$cohort==i)
  #data3<-subset(data3,data3$CauseDeath==0)
  data3$MaxDep<-data3$MaxDep*-1
  #data3<-subset(data3,data3$CauseDeath==0)
  data3$longlat<-paste(data3$LONG,data3$LAT,sep="")
  tomap<-cbind(aggregate(data3[,2:3],by=list(data3$longlat),FUN=max),aggregate(data3[,4:49],by=list(data3$longlat),FUN=sum)[,2:45])
  #tomap<-subset(tomap,tomap$Clutch>0)
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
  if(i==1){
    grids<-grid
  }else{
    grids<-stack(grids,grid)
  }
  cat(i,' \n')
}  


plot(sum(grids)/20,zlim=c(1,32),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=FALSE) 

plot(aust_bound, col="black", lwd=1.0,add=TRUE)  
species<-"Tiliqua"
species_dist=occurrences(taxon=species, download_reason_id=10)
species_dist<-as.data.frame(species_dist$data)
species_dist=subset(species_dist,species!="Tiliqua multifasciata")
points(species_dist$longitude, species_dist$latitude, col="black", cex=0.2,pch=16)

plot(sum(grids)/20,zlim=c(1,30),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=FALSE) 

plot(aust_bound, col="black", lwd=1.0,add=TRUE)  
species<-"Egernia"
species_dist=occurrences(taxon=species, download_reason_id=10)
species_dist<-as.data.frame(species_dist$data)
#species_dist=subset(species_dist,species!="Tiliqua multifasciata")
points(species_dist$longitude, species_dist$latitude, col="black", cex=0.2,pch=16)


plot(sum(grids)/20,zlim=c(1,30),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=FALSE) 

plot(aust_bound, col="black", lwd=1.0,add=TRUE)  
species<-"Liopholis"
species_dist=occurrences(taxon=species, download_reason_id=10)
species_dist<-as.data.frame(species_dist$data)
species_dist=subset(species_dist,species!="Liopholis whitei")
points(species_dist$longitude, species_dist$latitude, col="black", cex=0.2,pch=16)


# Mesic taxa

filenum=8

data2<-read.csv(paste(path,'raw/',files[filenum],sep=""),head=FALSE)
cohort<-rep(seq(1,20),20)
cohort<-cohort[order(cohort)]
cohort<-rep(cohort,(nrow(data2)/400))
data2$cohort<-cohort[1:nrow(data2)]
yearsout.names<-c("n","LONG","LAT","YEAR","MaxStg","MaxWgt","MaxLen","Tmax","Tmin","MinRes","MaxDess","MinShade","MaxShade","MinDep","MaxDep","Bsk","Forage","Dist","Food","Drink","NWaste","Feces","O2","Clutch","Fec","CauseDeath","tLay","tEgg","tStg1","tStg2","tStg3","tStg4","tStg5","tStg6","tStg7","tStg8","mStg1","mStg2","mStg3","mStg4","mStg5","mStg6","mStg7","mStg8","surviv","ovip_surviv","fitness","deathstage","cohort")
colnames(data2)<-yearsout.names
data2<-subset(data2,MaxWgt>0)

# repro
var<-25

for(i in 1:20){
  data3<-subset(data2,data2$cohort==i)
  #data3<-subset(data3,data3$CauseDeath==0)
  data3$MaxDep<-data3$MaxDep*-1
  #data3<-subset(data3,data3$CauseDeath==0)
  data3$longlat<-paste(data3$LONG,data3$LAT,sep="")
  tomap<-cbind(aggregate(data3[,2:3],by=list(data3$longlat),FUN=max),aggregate(data3[,4:49],by=list(data3$longlat),FUN=sum)[,2:45])
  #tomap<-subset(tomap,tomap$Clutch>0)
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
  if(i==1){
    grids<-grid
  }else{
    grids<-stack(grids,grid)
  }
  cat(i,' \n')
}  


plot(sum(grids)/20,zlim=c(1,30),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=FALSE) 

plot(aust_bound, col="black", lwd=1.0,add=TRUE)  
species<-"Eulamprus"
species_dist=occurrences(taxon=species, download_reason_id=10)
species_dist<-as.data.frame(species_dist$data)
#species_dist=subset(species_dist,species!="Tiliqua multifasciata")
points(species_dist$longitude, species_dist$latitude, col="black", cex=0.2,pch=16)

plot(sum(grids)/20,zlim=c(1,30),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=FALSE) 

plot(aust_bound, col="black", lwd=1.0,add=TRUE)  
species<-"Niveoscincus"
species_dist=occurrences(taxon=species, download_reason_id=10)
species_dist<-as.data.frame(species_dist$data)
#species_dist=subset(species_dist,species!="Tiliqua multifasciata")
points(species_dist$longitude, species_dist$latitude, col="black", cex=0.2,pch=16)

plot(sum(grids)/20,zlim=c(1,30),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=FALSE) 

plot(aust_bound, col="black", lwd=1.0,add=TRUE)  
species<-"Saproscincus"
species_dist=occurrences(taxon=species, download_reason_id=10)
species_dist<-as.data.frame(species_dist$data)
#species_dist=subset(species_dist,species!="Tiliqua multifasciata")
points(species_dist$longitude, species_dist$latitude, col="black", cex=0.2,pch=16)

plot(sum(grids)/20,zlim=c(1,30),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=FALSE) 

plot(aust_bound, col="black", lwd=1.0,add=TRUE)  
species<-"Pseudemoia"
species_dist=occurrences(taxon=species, download_reason_id=10)
species_dist<-as.data.frame(species_dist$data)
#species_dist=subset(species_dist,species!="Tiliqua multifasciata")
points(species_dist$longitude, species_dist$latitude, col="black", cex=0.2,pch=16)


## Maxent vs NicheMapR plots

library(maptools)
table <- data.frame(readAsciiGrid("distribution/maxent/species.asc"))
library(sp)
library(rgdal)
coordinates(table)=~s1+s2
proj4string(table)=CRS("+init=epsg:4326") # set it to lat-long
pts = spTransform(table,CRS("+proj=longlat +datum=WGS84"))
gridded(pts) = TRUE
r = raster(pts)
projection(r) = CRS("+proj=longlat +datum=WGS84")
r[r<0.2]<-NA


plot(r,ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=FALSE) 

plot(aust_bound, col="black", lwd=1.0,add=TRUE)  
species<-"Tiliqua rugosa"
species_dist=occurrences(taxon=species, download_reason_id=10)
species_dist<-as.data.frame(species_dist$data)
#species_dist=subset(species_dist,species!="Tiliqua multifasciata")
points(species_dist$longitude, species_dist$latitude, col="black", cex=0.2,pch=16)







# NicheMapR

filenum=5

data2<-read.csv(paste(path,'raw/',files[filenum],sep=""),head=FALSE)
cohort<-rep(seq(1,20),20)
cohort<-cohort[order(cohort)]
cohort<-rep(cohort,(nrow(data2)/400))
data2$cohort<-cohort[1:nrow(data2)]
yearsout.names<-c("n","LONG","LAT","YEAR","MaxStg","MaxWgt","MaxLen","Tmax","Tmin","MinRes","MaxDess","MinShade","MaxShade","MinDep","MaxDep","Bsk","Forage","Dist","Food","Drink","NWaste","Feces","O2","Clutch","Fec","CauseDeath","tLay","tEgg","tStg1","tStg2","tStg3","tStg4","tStg5","tStg6","tStg7","tStg8","mStg1","mStg2","mStg3","mStg4","mStg5","mStg6","mStg7","mStg8","surviv","ovip_surviv","fitness","deathstage","cohort")
colnames(data2)<-yearsout.names
data2<-subset(data2,MaxWgt>0)

# repro
var<-25

for(i in 1:20){
  data3<-subset(data2,data2$cohort==i)
  #data3<-subset(data3,data3$CauseDeath==0)
  data3$MaxDep<-data3$MaxDep*-1
  #data3<-subset(data3,data3$CauseDeath==0)
  data3$longlat<-paste(data3$LONG,data3$LAT,sep="")
  tomap<-cbind(aggregate(data3[,2:3],by=list(data3$longlat),FUN=max),aggregate(data3[,4:49],by=list(data3$longlat),FUN=sum)[,2:45])
  #tomap<-subset(tomap,tomap$Clutch>0)
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
  if(i==1){
    grids<-grid
  }else{
    grids<-stack(grids,grid)
  }
  cat(i,' \n')
}  


plot(sum(grids)/20,zlim=c(1,32),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=FALSE) 

plot(aust_bound, col="black", lwd=1.0,add=TRUE)  
species<-"Tiliqua rugosa"
species_dist=occurrences(taxon=species, download_reason_id=10)
species_dist<-as.data.frame(species_dist$data)
species_dist=subset(species_dist,species!="Tiliqua multifasciata")
points(species_dist$longitude, species_dist$latitude, col="black", cex=0.2,pch=16)
