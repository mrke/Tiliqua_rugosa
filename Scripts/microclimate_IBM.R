library(NicheMapR)
library(raster)
library(dismo)
spatial <- "w:/"
longlat<-c(139.3109, -33.888) # Bundey Bore study site
loc<-longlat
ystart <- 2008# start year
yfinish <- 2010# end year
nyears<-yfinish-ystart+1# integer, number of years for which to run the microclimate model

prevdir<-getwd()
setwd('x:')
cmd<-paste("R --no-save --args ",longlat[1]," ",longlat[2]," < extract.R",sep='')
system(cmd)
soilpro<-read.csv('data.csv')
setwd(prevdir)
soilpro[,1]<-c(2.5,7.5,22.5,45,80,150)
colnames(soilpro)[1] <- 'depth'
DEP <- c(0., 1.,  3, 5.,  10,  15,  30.,  60.,  100.,  200.) # Soil nodes (cm)
soil.hydro<-pedotransfer(soilpro = as.data.frame(soilpro), DEP = DEP)
PE<-soil.hydro$PE
BB<-soil.hydro$BB
BD<-soil.hydro$BD
KS<-soil.hydro$KS
BulkDensity <- BD[seq(1,19,2)]*1000 #soil bulk density, kg/m3
LAI <- 0.317 # mean for the site

maxshade <- 90 # maximum simulated shade

Usrhyt <- 0.01 # m, use 0.01 for juveniles, 0.03 for adults 
micro_juv<-micro_aust(loc = longlat, LAI = LAI, ystart = ystart, yfinish = yfinish, PE = PE, BB = BB, BD = BD, KS = KS, BulkDensity = BulkDensity, maxshade = maxshade, Usrhyt = Usrhyt, DEP = DEP, spatial = "W:/")

Usrhyt <- 0.03 # m, use 0.01 for juveniles, 0.03 for adults 
micro_adult<-micro_aust(loc = longlat, LAI = LAI, ystart = ystart, yfinish = yfinish, PE = PE, BB = BB, BD = BD, KS = KS, BulkDensity = BulkDensity, maxshade = maxshade, Usrhyt = Usrhyt, DEP = DEP, spatial = "W:/")

# append dates
tzone=paste("Etc/GMT-",10,sep="") # doing it this way ignores daylight savings!
dates=seq(ISOdate(ystart,1,1,tz=tzone)-3600*12, ISOdate((ystart+nyears),1,1,tz=tzone)-3600*13, by="hours")
dates=subset(dates, format(dates, "%m/%d")!= "02/29") # remove leap years


metout_juv=as.data.frame(micro_juv$metout)
shadmet_juv=as.data.frame(micro_juv$shadmet)
soil_juv=as.data.frame(micro_juv$soil)
shadsoil_juv=as.data.frame(micro_juv$shadsoil)

metout_adult=as.data.frame(micro_adult$metout)
shadmet_adult=as.data.frame(micro_adult$shadmet)
soil_adult=as.data.frame(micro_adult$soil)
shadsoil_adult=as.data.frame(micro_adult$shadsoil)

metout_juv=as.data.frame(cbind(dates,metout_juv))
soil_juv=as.data.frame(cbind(dates,soil_juv))
shadmet_juv=as.data.frame(cbind(dates,shadmet_juv))
shadsoil_juv=as.data.frame(cbind(dates,shadsoil_juv))

metout_adult=as.data.frame(cbind(dates,metout_adult))
soil_adult=as.data.frame(cbind(dates,soil_adult))
shadmet_adult=as.data.frame(cbind(dates,shadmet_adult))
shadsoil_adult=as.data.frame(cbind(dates,shadsoil_adult))

# save last two years
write.csv(metout_adult[(365*24+1):nrow(metout_adult),], 'metout_adult.csv')
write.csv(soil_adult[(365*24+1):nrow(metout_adult),], 'soil_adult.csv')
write.csv(shadmet_adult[(365*24+1):nrow(metout_adult),], 'shadmet_adult.csv')
write.csv(shadsoil_adult[(365*24+1):nrow(metout_adult),], 'shadsoil_adult.csv')

write.csv(metout_juv[(365*24+1):nrow(metout_adult),], 'metout_juv.csv')
write.csv(soil_juv[(365*24+1):nrow(metout_adult),], 'soil_juv.csv')
write.csv(shadmet_juv[(365*24+1):nrow(metout_adult),], 'shadmet_juv.csv')
write.csv(shadsoil_juv[(365*24+1):nrow(metout_adult),], 'shadsoil_juv.csv')