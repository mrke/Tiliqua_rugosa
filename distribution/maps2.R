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
 plot(sum(grids)/20,zlim=c(1,30),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=TRUE) 
}else{
 plot(sum(grids)/20,zlim=c(1,30),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=FALSE) 
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

# Tiliqua and Egernia

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
 

 plot(sum(grids)/20,zlim=c(1,30),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=FALSE) 

plot(aust_bound, col="black", lwd=1.0,add=TRUE)  
species<-"Tiliqua"
species_dist=occurrences(taxon=species, download_reason_id=10)
species_dist<-as.data.frame(species_dist$data)
species_dist=subset(species_dist,species!="Tiliqua multifasciata")
points(species_dist$longitude, species_dist$latitude, col="black", cex=0.2,pch=16)

 plot(sum(grids)/20,zlim=c(0.1,30),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=FALSE) 

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

































data2<-read.csv(paste(path,'raw/',files[4],sep=""),head=FALSE)
cohort<-rep(seq(1,20),20)
cohort<-cohort[order(cohort)]
cohort<-rep(cohort,(nrow(data2)/400))
data2$cohort<-cohort[1:nrow(data2)]
yearsout.names<-c("n","LONG","LAT","YEAR","MaxStg","MaxWgt","MaxLen","Tmax","Tmin","MinRes","MaxDess","MinShade","MaxShade","MinDep","MaxDep","Bsk","Forage","Dist","Food","Drink","NWaste","Feces","O2","Clutch","Fec","CauseDeath","tLay","tEgg","tStg1","tStg2","tStg3","tStg4","tStg5","tStg6","tStg7","tStg8","mStg1","mStg2","mStg3","mStg4","mStg5","mStg6","mStg7","mStg8","surviv","ovip_surviv","fitness","deathstage","cohort")
colnames(data2)<-yearsout.names

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
 plot(sum(grids)/20,zlim=c(0.1,30),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=TRUE) 
}else{
 plot(sum(grids)/20,zlim=c(0.1,30),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=TRUE) 
}
plot(aust_bound, col="black", lwd=1.0,add=TRUE)  
species<-"Tiliqua scincoides"
species_dist=occurrences(taxon=species, download_reason_id=10)
species_dist<-as.data.frame(species_dist$data)
points(species_dist$longitude, species_dist$latitude, col="black", cex=0.2,pch=16)
species<-"Tiliqua occipitalis"
species_dist=occurrences(taxon=species, download_reason_id=10)
species_dist<-as.data.frame(species_dist$data)
points(species_dist$longitude, species_dist$latitude, col="blue", cex=0.2,pch=16)

############ yearout code ################
yearout<-read.csv(paste(path,'raw/',files[filenum],sep=""),head=FALSE)

yearout.names<-c("site","LONG","LAT","DEVTIME","BIRTHDAY","BIRTHMASS","MONMATURE","MONREPRO","SVLREPRO","FECUNDITY","CLUTCHES","ANNUALACT","MINRESERVE","LASTFOOD","TOTFOOD","MINTB","MAXTB","Pct_Dess","LifeSpan","GenTime","R0","rmax","SVL")

var=23
names(yearout)<-yearout.names
yearout$longlat<-paste(yearout$LONG,yearout$LAT,sep="")
yearout<-cbind(aggregate(yearout[,2:3],by=list(yearout$longlat),FUN=max),aggregate(yearout[,4:23],by=list(yearout$longlat),FUN=max))
#yearout<-yearout[,2:24]
max<-max(yearout[,var])
min<-min(yearout[,var])

lat1<-min(yearout[,3])-.025 # min latitude
lat2<-max(yearout[,3])+.025 # max latitude
lon1<-min(yearout[,2])-.025 # min longitude
lon2<-max(yearout[,2])+.025 # max longitude
quadwid<-(lon2-lon1)/.6
quadlen<-(lat2-lat1)/.6
gridout <- raster(ncol=quadwid, nrow=quadlen, xmn=lon1, xmx=lon2, ymn=lat1, ymx=lat2)

x<-cbind(yearout$LONG,yearout$LAT) # list of co-ordinates
grid <- rasterize(x, gridout, yearout[,var])
grid <- projectRaster(grid, crs="+proj=longlat +datum=WGS84") # change projection here if needed

plot(grid,zlim=c(1,20),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=TRUE) 
plot(aust_bound, col="black", lwd=1.0,add=TRUE)  

#species<-"Tiliqua rugosa"
species<-"Liopholis kintorei"

species_dist=occurrences(taxon=species, download_reason_id=10)
species_dist<-as.data.frame(species_dist$data)
points(species_dist$longitude, species_dist$latitude, col="black", cex=0.2,pch=16)
############ yearout code ################









plot(sum(grids)/20,zlim=c(0.1,35),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=TRUE) 
#plot(sleepy_limit, col="black", lwd=1.0,lty=2,add=TRUE)
plot(aust_bound, col="black", lwd=1.0,add=TRUE)  
species<-"Tiliqua scincoides"
species_dist=occurrences(taxon=species, download_reason_id=10)
species_dist<-as.data.frame(species_dist$data)
points(species_dist$longitude, species_dist$latitude, col="black", cex=0.2,pch=16)
species<-"Tiliqua occipitalis"
species_dist=occurrences(taxon=species, download_reason_id=10)
species_dist<-as.data.frame(species_dist$data)
points(species_dist$longitude, species_dist$latitude, col="blue", cex=0.2,pch=16)

plot(sum(grids)/20,ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=TRUE) 
#plot(sleepy_limit, col="black", lwd=1.0,lty=2,add=TRUE)
plot(aust_bound, col="black", lwd=1.0,add=TRUE)  
species<-"Tiliqua multifasciata"
species_dist=occurrences(taxon=species, download_reason_id=10)
species_dist<-as.data.frame(species_dist$data)
points(species_dist$longitude, species_dist$latitude, col="red", cex=0.2,pch=16)


# causedeath
for(i in 1:20){
  base<-subset(data2,data2$cohort==i & data2$CauseDeath>=0)
  causedeath0<-subset(data2,data2$cohort==i & data2$CauseDeath==0)
  causedeath1<-subset(data2,data2$cohort==i & data2$CauseDeath==1)
  causedeath2<-subset(data2,data2$cohort==i & data2$CauseDeath==2)
  causedeath3<-subset(data2,data2$cohort==i & data2$CauseDeath==3)
  causedeath4<-subset(data2,data2$cohort==i & data2$CauseDeath==4)
  causedeath5<-subset(data2,data2$cohort==i & data2$CauseDeath==5)
  
  base$longlat<-paste(base$LONG,base$LAT,sep="")
  causedeath0$longlat<-paste(causedeath0$LONG,causedeath0$LAT,sep="")
  causedeath1$longlat<-paste(causedeath1$LONG,causedeath1$LAT,sep="")
  causedeath2$longlat<-paste(causedeath2$LONG,causedeath2$LAT,sep="")
  causedeath3$longlat<-paste(causedeath3$LONG,causedeath3$LAT,sep="")
  causedeath4$longlat<-paste(causedeath4$LONG,causedeath4$LAT,sep="")
  causedeath5$longlat<-paste(causedeath5$LONG,causedeath5$LAT,sep="")

    tomap<-cbind(aggregate(base[,2:3],by=list(base$longlat),FUN=max),aggregate(base[,4],by=list(base$longlat),FUN=max)[,2])
  colnames(tomap[4])<-'causedeath'
  max<-max(tomap[,4])
  min<-min(tomap[,4])
  lat1<-min(tomap[,3])-.025 # min latitude
  lat2<-max(tomap[,3])+.025 # max latitude
  lon1<-min(tomap[,2])-.025 # min longitude
  lon2<-max(tomap[,2])+.025 # max longitude
  quadwid<-(lon2-lon1)/.6
  quadlen<-(lat2-lat1)/.6
  gridout <- raster(ncol=quadwid, nrow=quadlen, xmn=lon1, xmx=lon2, ymn=lat1, ymx=lat2)
  x<-cbind(tomap$LONG,tomap$LAT) # list of co-ordinates
  grid <- rasterize(x, gridout, tomap[,4])
  grid <- projectRaster(grid, crs="+proj=longlat +datum=WGS84") # change projection here if needed
  grid<-grid*0
  
  if(nrow(causedeath0)>0){
  tomap0<-cbind(aggregate(causedeath0[,2:3],by=list(causedeath0$longlat),FUN=max),aggregate(causedeath0[,26],by=list(causedeath0$longlat),FUN=length)[,2])
  colnames(tomap0[4])<-'causedeath'

  x<-cbind(tomap0$LONG,tomap0$LAT) # list of co-ordinates
  grid0 <- rasterize(x, gridout, tomap[,4])
  grid0<-merge(grid0,grid)
  #grid0 <- projectRaster(grid0, crs="+proj=longlat +datum=WGS84") # change projection here if needed
  }else{
    grid0<-grid
  }
  if(nrow(causedeath1)>0){
  tomap1<-cbind(aggregate(causedeath1[,2:3],by=list(causedeath1$longlat),FUN=max),aggregate(causedeath1[,26],by=list(causedeath1$longlat),FUN=length)[,2])
  colnames(tomap1[4])<-'causedeath'

  x<-cbind(tomap1$LONG,tomap1$LAT) # list of co-ordinates
  grid1 <- rasterize(x, gridout, tomap[,4])
  grid1<-merge(grid1,grid)
#grid1 <- projectRaster(grid1, crs="+proj=longlat +datum=WGS84") # change projection here if needed
  }else{
    grid1<-grid
  }
    if(nrow(causedeath2)>0){
  tomap2<-cbind(aggregate(causedeath2[,2:3],by=list(causedeath2$longlat),FUN=max),aggregate(causedeath2[,26],by=list(causedeath2$longlat),FUN=length)[,2])
  colnames(tomap2[4])<-'causedeath'

  x<-cbind(tomap2$LONG,tomap2$LAT) # list of co-ordinates
  grid2 <- rasterize(x, gridout, tomap[,4])
  grid2<-merge(grid2,grid)
  #grid2 <- projectRaster(grid2, crs="+proj=longlat +datum=WGS84") # change projection here if needed
  }else{
    grid2<-grid
  }
    if(nrow(causedeath3)>0){
  tomap3<-cbind(aggregate(causedeath3[,2:3],by=list(causedeath3$longlat),FUN=max),aggregate(causedeath3[,26],by=list(causedeath3$longlat),FUN=length)[,2])
  colnames(tomap3[4])<-'causedeath'

  x<-cbind(tomap3$LONG,tomap3$LAT) # list of co-ordinates
  grid3 <- rasterize(x, gridout, tomap[,4])
  grid3<-merge(grid3,grid,tolerance=0.5)
  #grid3 <- projectRaster(grid3, crs="+proj=longlat +datum=WGS84") # change projection here if needed
  }else{
    grid3<-grid
  }
    if(nrow(causedeath4)>0){
  tomap4<-cbind(aggregate(causedeath4[,2:3],by=list(causedeath4$longlat),FUN=max),aggregate(causedeath4[,26],by=list(causedeath4$longlat),FUN=length)[,2])
  colnames(tomap4[4])<-'causedeath'

  x<-cbind(tomap4$LONG,tomap4$LAT) # list of co-ordinates
  grid4 <- rasterize(x, gridout, tomap[,4])
  grid4<-merge(grid4,grid)
  #grid4 <- projectRaster(grid4, crs="+proj=longlat +datum=WGS84") # change projection here if needed
  }else{
    grid4<-grid
  }
    if(nrow(causedeath5)>0){
  tomap5<-cbind(aggregate(causedeath5[,2:3],by=list(causedeath5$longlat),FUN=max),aggregate(causedeath5[,26],by=list(causedeath5$longlat),FUN=length)[,2])
  colnames(tomap5[4])<-'causedeath'

  x<-cbind(tomap5$LONG,tomap5$LAT) # list of co-ordinates
  grid5 <- rasterize(x, gridout, tomap[,4])
  grid5<-merge(grid5,grid)
  #grid5 <- projectRaster(grid5, crs="+proj=longlat +datum=WGS84") # change projection here if needed
  }else{
    grid5<-grid
  }
  if(i==1){
  grids0<-grid0
  grids1<-grid1
  grids2<-grid2
  grids3<-grid3
  grids4<-grid4
  grids5<-grid5
}else{
  grids0<-stack(grids0,grid0)
  grids1<-stack(grids1,grid1)
  grids2<-stack(grids2,grid2)
  grids3<-stack(grids3,grid3)
  grids4<-stack(grids4,grid4)
  grids5<-stack(grids5,grid5)
}
  cat(i,' \n')
} 

plot(sum(grids0)/20,ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=TRUE) 
#plot(sleepy_limit, col="black", lwd=1.0,lty=2,add=TRUE)
plot(aust_bound, col="black", lwd=1.0,add=TRUE)  
species<-"Tiliqua rugosa"
species_dist=occurrences(taxon=species, download_reason_id=10)
species_dist<-as.data.frame(species_dist$data)
points(species_dist$longitude, species_dist$latitude, col="black", cex=0.2,pch=16)

plot(sum(grids1)/20,ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=TRUE) 
#plot(sleepy_limit, col="black", lwd=1.0,lty=2,add=TRUE)
plot(aust_bound, col="black", lwd=1.0,add=TRUE)  
species<-"Tiliqua rugosa"
species_dist=occurrences(taxon=species, download_reason_id=10)
species_dist<-as.data.frame(species_dist$data)
points(species_dist$longitude, species_dist$latitude, col="black", cex=0.2,pch=16)

plot(sum(grids2)/20,ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=TRUE) 
#plot(sleepy_limit, col="black", lwd=1.0,lty=2,add=TRUE)
plot(aust_bound, col="black", lwd=1.0,add=TRUE)  
species<-"Tiliqua rugosa"
species_dist=occurrences(taxon=species, download_reason_id=10)
species_dist<-as.data.frame(species_dist$data)
points(species_dist$longitude, species_dist$latitude, col="black", cex=0.2,pch=16)

plot(sum(grids3)/20,ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=TRUE) 
#plot(sleepy_limit, col="black", lwd=1.0,lty=2,add=TRUE)
plot(aust_bound, col="black", lwd=1.0,add=TRUE)  
species<-"Tiliqua rugosa"
species_dist=occurrences(taxon=species, download_reason_id=10)
species_dist<-as.data.frame(species_dist$data)
points(species_dist$longitude, species_dist$latitude, col="black", cex=0.2,pch=16)

plot(sum(grids4)/20,ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=TRUE) 
#plot(sleepy_limit, col="black", lwd=1.0,lty=2,add=TRUE)
plot(aust_bound, col="black", lwd=1.0,add=TRUE)  
species<-"Tiliqua rugosa"
species_dist=occurrences(taxon=species, download_reason_id=10)
species_dist<-as.data.frame(species_dist$data)
points(species_dist$longitude, species_dist$latitude, col="black", cex=0.2,pch=16)

plot(sum(grids5)/20,ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=TRUE) 
#plot(sleepy_limit, col="black", lwd=1.0,lty=2,add=TRUE)
plot(aust_bound, col="black", lwd=1.0,add=TRUE)  
species<-"Tiliqua rugosa"
species_dist=occurrences(taxon=species, download_reason_id=10)
species_dist<-as.data.frame(species_dist$data)
points(species_dist$longitude, species_dist$latitude, col="black", cex=0.2,pch=16)


}
brks <- seq(-0.5, 5.5,1) 
nb <- length(brks)-1 
cols <- c("dark green","light blue","red","blue","orange","grey")  
plot(max(grids,na.rm=TRUE),zlim=c(0,5),breaks=brks,col=cols,ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=TRUE) 
plot(sleepy_limit, col="black", lwd=1.0,lty=2,add=TRUE)
plot(aust_bound, col="black", lwd=1.0,add=TRUE)    
  
  
  
  
  
  
  
par(mfrow = c(4,5)) # set up for 5 plots in 2 columns
par(oma = c(3,3,3,3) + 0.1) # margin spacing stuff
par(mar=c(0,0,1,0)) # margin spacing stuff 
par(mgp = c(3,1,0) ) # margin spacing stuff  
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
  #plot(grid,main=files[order1[i]],zlim=c(0,35),ylim=c(-45,-10),xlim=c(105,155))
  #plot(grid,zlim=c(0,31),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=FALSE)
  brks <- seq(1, 5) 
nb <- length(brks)-1 
#cols <- c("white","light blue","red","blue","dark green","grey")
#cols<-rainbow(255)
cols<-terrain.colors(35)
#cols='green'
  if(i==1){
    plot(grid,ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=TRUE,main=1990+i-1,col=cols)
  }else{
    plot(grid,ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=FALSE,main=1990+i-1,col=cols)
  }

  #plot(grid,zlim=c(min,max),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE)
  #plot(sleepy_limit, col="black", lwd=1.0,lty=2,add=TRUE)
  
      species<-"Tiliqua rugosa"
    species_dist=occurrences(taxon=species, download_reason_id=10)
    species_dist<-as.data.frame(species_dist$data)
    #points(species_dist$longitude, species_dist$latitude, col="black", cex=0.2,pch=16)
  
  plot(aust_bound, col="black", lwd=1.0,add=TRUE)
}

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
  tomap<-aggregate(data[,2:24],by=list(data$longlat),FUN=max)
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
  #plot(grid,zlim=c(0,31),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=FALSE)
    plot(grid,ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE,legend=TRUE)

  #plot(grid,zlim=c(min,max),ylim=c(-45,-10),xlim=c(105,155),axes=F, box=FALSE)
  plot(sleepy_limit, col="black", lwd=1.0,lty=2,add=TRUE)
  plot(aust_bound, col="black", lwd=1.0,add=TRUE)
  if(i==1){
    text(118,-15,"no water")
    text(118,-16.5, "limit")
    species<-"Tiliqua rugosa"
    species_dist=occurrences(taxon=species, download_reason_id=10)
    species_dist<-as.data.frame(species_dist$data)
    points(species_dist$longitude, species_dist$latitude, col="black", cex=0.2,pch=16)
  }else{
    if(i<7){
      text(118,-15,paste(pctwets[i],"%",sep=""))
    }else{
      text(118,-15,paste(pctwets[i],"%",sep=""))
    }
  }
  
  
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

