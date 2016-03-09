#Tiliqua rugosa analyses for Ecology Letters MS

library(NicheMapR)
source("../micro_australia/get.soil.R")

longlat<-c(139.3109, -33.888) # Bundey Bore study site
loc<-longlat
ystart <- 1991# start year
yfinish <- 2010# end year
nyears<-yfinish-ystart+1# integer, number of years for which to run the microclimate model

DEP <- c(0., 1.,  3, 5.,  10,  15,  30.,  60.,  100.,  200.) # Soil nodes (cm) - keep spacing close near the surface, last value is where it is assumed that the soil temperature is at the annual mean air temperature
soil.hydro<-get.soil(SLGA = 1, soilpro = 0) # extract soil parameters
PE<-soil.hydro$PE
BB<-soil.hydro$BB
BD<-soil.hydro$BD
KS<-soil.hydro$KS
PE[1:9]<-CampNormTbl9_1[3,4] #air entry potential J/kg 
KS[1:9]<-CampNormTbl9_1[3,6] #saturated conductivity, kg s/m3
BB[1:9]<-CampNormTbl9_1[3,5] #soil 'b' parameter
PE[10:13]<-CampNormTbl9_1[4,4] #air entry potential J/kg 
KS[10:13]<-CampNormTbl9_1[4,6] #saturated conductivity, kg s/m3
BB[10:13]<-CampNormTbl9_1[4,5] #soil 'b' parameter
BulkDensity <- BD[seq(1,19,2)]*1000 #soil bulk density, kg/m3

# run microclimate model to get microclimate for ectotherm model and soil temps for predicting egg development and food availability
micro<-micro_aust(loc = longlat, ystart = ystart, yfinish = yfinish, PE = PE, BB = BB, BD = 
    BD, KS = KS, BulkDensity = BulkDensity, maxshade = 50, Usrhyt = 0.03, DEP = DEP, REFL = 0.2)

save(micro,file = 'microclimate/micro.Rda') # for use if no access to AWAP server
#load('microclimate/micro.Rda')

micro$humid[,3:9]<-micro$metout[,5]/100 # assume ambient humidity down to 10cm
micro$shadhumid[,3:9]<-micro$shadmet[,5]/100 # assume ambient humidity down to 10cm
micro$humid[,7:12]<-0.8 # assume higher humidity in burrow, 10cm and lower
micro$shadhumid[,7:12]<-0.8 # assume higher humidity in burrow, 10cm and lower

debpars=as.data.frame(read.csv('DEB model/DEB_pars_Tiliqua_rugosa.csv',header=FALSE))$V1 # read in DEB pars
TBASK = 26
raindrink=2.5
mu_E = 585000
VTMIN = 26
VTMAX = 39
nicheout=ectotherm(ABSMAX = 0.866, ABSMIN = 0.866, VTMAX = VTMAX, VTMIN = VTMIN, write_input = 0, EMISAN = 1, gutfill = 75,
  TBASK = TBASK, TEMERGE = 8.5, ctmax = 39, ctmin = 3.5, TPREF = 33.5,  peyes = 0.03, ptcond = 0.1, skinwet = 0.2,
  shdburrow = 1, minwater = 15, maxshades = micro$MAXSHADES, mindepth = 3, raindrink = raindrink, DEB = 1, z=debpars[8], del_M=debpars[9], F_m = 13290,  
  kap_X=debpars[11],   v=debpars[13]/24, kap=debpars[14], p_M=debpars[16]/24, 
  E_G=debpars[19],   kap_R=debpars[15], k_J=debpars[18]/24, E_Hb=debpars[20],
  E_Hp=debpars[21], h_a=debpars[22]*10^-1/(24^2),   s_G=debpars[23],   E_0=debpars[24],  mu_E = mu_E,
  T_REF = debpars[1]-273, TA = debpars[2], TAL = debpars[5], TAH = debpars[6], TL = debpars[3], TH = debpars[4], 
  E_sm=186.03*6, K = 500, X = 111.7, plantsim = c(4, 4, 14, -200, -1500, 82), clutch_ab = c(0.085,0.7), viviparous = 1,
  photostart = 4, E_init = ((debpars[16]/24)*debpars[8]/debpars[14])/(debpars[13]/24), E_H_init = debpars[20]+5,  
  v_init = debpars[25]^3, stage = 1, mh = 1,  minclutch = 0, clutchsize = 2)

# retrieve output
metout=as.data.frame(nicheout$metout)
shadmet=as.data.frame(nicheout$shadmet)
soil=as.data.frame(nicheout$soil)
shadsoil=as.data.frame(nicheout$shadsoil)
rainfall=as.data.frame(nicheout$RAINFALL)
foodwaters=as.data.frame(nicheout$foodwaters)
foodlevels=as.data.frame(nicheout$foodlevels)
environ=as.data.frame(nicheout$environ)
enbal=as.data.frame(nicheout$enbal)
masbal=as.data.frame(nicheout$masbal)
humid=as.data.frame(nicheout$humid)
debout=as.data.frame(nicheout$debout)
yearout=as.data.frame(nicheout$yearout)
foodwaters=as.data.frame(nicheout$foodwaters)
foodlevels=as.data.frame(nicheout$foodlevels)
yearsout=t(as.data.frame(nicheout$yearsout))

# append dates
tzone=paste("Etc/GMT-",10,sep="") # doing it this way ignores daylight savings!
dates=seq(ISOdate(ystart,1,1,tz=tzone)-3600*12, ISOdate((ystart+nyears),1,1,tz=tzone)-3600*13, by="hours")
dates=subset(dates, format(dates, "%m/%d")!= "02/29") # remove leap years
debout=cbind(dates,debout)
environ=cbind(dates,environ)
masbal=cbind(dates,masbal)
enbal=cbind(dates,enbal)
soil=cbind(dates,soil)
metout=cbind(dates,metout)
shadsoil=cbind(dates,shadsoil)
shadmet=cbind(dates,shadmet)


dates2=seq(ISOdate(ystart,1,1,tz=tzone)-3600*12, ISOdate((ystart+nyears),1,1,tz=tzone)-3600*13, by="days") 
dates2=subset(dates2, format(dates2, "%m/%d")!= "02/29") # remove leap years
grass=cbind(dates2,foodwaters,foodlevels)
colnames(grass)=c("dates","growth","tsdm")
rainfall=as.data.frame(cbind(dates2,rainfall))
colnames(rainfall)=c("dates","rainfall")

################### analysis and plots ################################

environ=as.data.frame(environ)

addTrans = function(color,trans)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.
  
  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color = rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans = rep(trans,length(color))
  
  num2hex = function(x)
  {
    hex = unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb = rbind(col2rgb(color),trans)
  res = paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}

# For Fig. S6
with(debout, {plot(WETMASS~dates,type = "l",ylab = "wet mass (g)/rainfall(mm×10)",ylim=c(0,1300),col='black')})
points(rainfall$rainfall*10~dates2,type='h',col='blue')

# plot mass and reproduction phenology
plotdebout=subset(debout,as.numeric(format(dates, "%Y"))<1994) # use this if you want to subset particular years
plotdebout=debout # this just keeps it to all years
year_vals=subset(plotdebout,as.character(dates,format='%d/%m')=="01/01")
year_vals=subset(year_vals,as.character(year_vals$dates,format='%H')=="00") # get midnight
plot(plotdebout$WETMASS~plotdebout$dates,type='l', ylab="wet mass, g",xlab="date") # plot wet mass as a function of date
abline(v=year_vals$dates,col='grey',lty=2) # add lines to show beginning of each year
plot(debout$CUMBATCH/1000~debout$dates,type='l', ylab='total energy, kJ',xlab="date") # plot energy in batch buffer (yolking eggs)
points(debout$CUMREPRO/1000~debout$dates,type='l',col='red') # plot energy in the reproduction buffer (captial for egg production, but not currently transformed to eggs)
abline(v=year_vals$dates,col='grey',lty=2) # add lines to show beginning of each year
plot(debout$V_baby~debout$dates,type='l', ylab='embryo structure (cm3)',xlab="date") # plot embryo development (volume of structure)
points(debout$Breeding/10~debout$dates,type='l',col='light blue',lty=1)

# get birthdays
V_baby2=debout[1:(nrow(debout)-1),15]-debout[2:(nrow(debout)),15]
V_baby2[V_baby2>0.1]=1
V_baby2[V_baby2!=1]=0
V_baby2=c(V_baby2,0)
debout$birth=V_baby2
format(subset(debout,birth==1)$dates,"%d/%m/%Y")

# Body mass with and without lost water
with(debout, {plot((WETMASS-WETMASS*(Body_cond/100))~dates,type = "l",xlab = "day of year",ylab = "wet mass (g)",col='blue')})
with(debout, {points(WETMASS~dates,type = "l",ylab = "wet mass (g)/grass growth")})

library(lattice)
with(environ, {xyplot(TC+ACT*5+SHADE/10+DEP/10~dates,type = "l")})
merge=as.data.frame(paste(with(environ,as.numeric(format(dates, "%Y"))),"_",with(environ,as.numeric(format(dates, "%m"))),"_",with(environ,as.numeric(format(dates, "%d"))),"_",environ$TIME-1,sep=""))
colnames(merge)='merge'
environ2=cbind(environ,merge)
library(mda)
environ3=environ2
environ4=subset(environ3,format(dates,"%Y")>=2009)
environ5=environ4
for(l in 1:nrow(environ4)){ # make values average of prev hour to account for 1/2 time diff in SA
  if(l==nrow(environ4)){
    environ5[l,6]=environ4[l,6]
  }else{
    environ5[l,6]=environ5[l,6]+(environ4[l+1,6]-environ4[l,6])/2
  }
}

waddlefiles=read.csv("waddleometer/waddle_files_all.csv")
sex=read.csv("waddleometer/sex.csv",stringsAsFactors = FALSE)

threshold.act=50 # threshold steps for counting as active
curyear=2010
environ_bask = subset(environ3,  subset=(ACT>=1 & TC>=TBASK & substr(dates,1,4)==curyear))
environ_forage = subset(environ3,  subset=(ACT>1 & substr(dates,1,4)==curyear))
environ_night= subset(metout,  subset=(ZEN==90))
rbPal = colorRampPalette(c('turquoise','gold1'))

environ_forage = subset(environ3,  subset=(ACT>1 & substr(dates,1,4)==curyear))
environ_all = subset(environ3,  subset=(ACT>=0 & substr(dates,1,4)==curyear))
plotmetout = subset(metout,  subset=(substr(dates,1,4)==curyear))
Tbs=t(as.numeric(as.matrix(c(environ_bask$TC,environ_bask$TC))))
Tbs=as.data.frame(t(Tbs))
colnames(Tbs)='Tb'
Tbs$Col=rbPal(23)[as.numeric(cut(Tbs$Tb, breaks=seq(18, 37,1), include.lowest=TRUE))]
environ_bask$Col=Tbs[1:nrow(environ_bask),2]
environ_night= subset(metout,  subset=(ZEN==90))
environ_night$TIME=environ_night$TIME/60-1
startdy=250
finishdy=365
desic=subset(debout,TIME==24 & substr(dates,1,4)==curyear)
desic=as.data.frame(cbind(desic[,2],desic[,20]))
colnames(desic)=c('day','desic')

# seasonal/daily activity plots

# foraging only
with(environ_night, {plot(TIME+2~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=0.5,col=addTrans("dark grey",50),pch=16)})
with(environ_forage, {points(TIME~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1.,col=environ_bask$Col,pch=15)})
points(foodwaters[(365*17):(365*18),1]/10, type = "l",col='green')
with(desic, {points(desic~day, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",lwd=2,pch=15,col='blue',type='l',lty=2)})

# basking and foraging
with(environ_night, {plot(TIME+2~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=0.5,col=addTrans("dark grey",50),pch=16)})
with(environ_bask, {points(TIME~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1.,col=environ_bask$Col,pch=15)})
points(foodwaters[(365*17):(365*18),1]*10, type = "l",col='green')
with(desic, {points(desic~day, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",lwd=2,pch=15,col='blue',type='l',lty=2)})

# plot food water content
dates4=seq(ISOdate(2009,1,1,tz=tzone)-3600*12, ISOdate(2011,1,1,tz=tzone)-3600*13, by="days") 
dates4=subset(dates4, format(dates4, "%m/%d")!= "02/29") # remove leap years
grassplot=foodwaters[(365*18):(365*20),1]*100
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(grassplot[1:730]~dates4,ylim=c(0,100),type='l',lwd=2, col='dark green',ylab='plant water content/lizard desiccation, %',xlab="")
par(new = TRUE)
axis(side=4, at = pretty(range(grassplot[1:730])))
mtext("rainfall, mm", side=4, line=3)
desic=subset(debout,TIME==24 & substr(dates,1,4)>=2009)
desic=as.data.frame(cbind(desic[,2],desic[,20]))
colnames(desic)=c('day','desic')
with(desic, {points(desic~dates4,xlab = "day of year",ylab = "hour of day",lwd=2,pch=15,col='cyan',type='l',lty=1)})
rainf=rainfall[(365*18):(365*20),2]*1
points(rainf[1:730]~dates4, type = "h",col='blue')

# analyse mass change in 2009 during drought period

# first with all lizards
massdata=read.csv('waddleometer/Church_site_mass_length.csv')
massdata2009=subset(massdata,year==2009)
massagg=aggregate(massdata2009$wgt,by=list(paste(massdata2009$lizard_ID,massdata2009$dryperiod,sep="_")),mean)
massdry=aggregate(massdata2009$dryperiod,by=list(paste(massdata2009$lizard_ID,massdata2009$dryperiod,sep="_")),mean)
massid=aggregate(massdata2009$lizard_ID,by=list(paste(massdata2009$lizard_ID,massdata2009$dryperiod,sep="_")),mean)
massagg_all=as.data.frame(cbind(massdry$x,massid$x,massagg$x))
colnames(massagg_all)=c('dry','id','mass')
massagg1=subset(massagg_all,dry==1)
massagg0=subset(massagg_all,dry==0)
massagg_merge=merge(massagg1,massagg0,by='id')
diff=massagg_merge$mass.x-massagg_merge$mass.y
pctdes=(diff*-1)/massagg_merge$mass.y*100
pctdesic=subset(pctdes,pctdes>0)
mean(pctdesic,na.rm=TRUE)
massagg_merge=cbind(massagg_merge,diff,pctdes)
with(massagg_merge,{t.test(mass.x,mass.y,paired=TRUE)})

s=seq(length(massagg_merge$mass.x))
par(bty="l")
boxplot(massagg_merge$mass.x,massagg_merge$mass.y,xlab="Time",ylab="Measure",names=c("desiccated","hydrated"),col=c("lightblue","lightgreen"),ylim=c(400,1000))
stripchart(list(massagg_merge$mass.x,massagg_merge$mass.y),vertical=T,pch=16,cex=0.5,add=T)
segments(rep(0.95,length(massagg_merge$mass.x))[s],massagg_merge$mass.x[s],rep(2,length(massagg_merge$mass.x))[s],massagg_merge$mass.y[s],col='light grey',lwd=0.5)

# second excluding animals that remained active
activelizards=c(9532,9372,9364,9363,9310,40044,40012,12847,12434,11885,11505,10509)
massdata2009=subset(massdata,year==2009 & !(lizard_ID%in%activelizards))
massagg=aggregate(massdata2009$wgt,by=list(paste(massdata2009$lizard_ID,massdata2009$dryperiod,sep="_")),mean)
massdry=aggregate(massdata2009$dryperiod,by=list(paste(massdata2009$lizard_ID,massdata2009$dryperiod,sep="_")),mean)
massid=aggregate(massdata2009$lizard_ID,by=list(paste(massdata2009$lizard_ID,massdata2009$dryperiod,sep="_")),mean)
massagg_all=as.data.frame(cbind(massdry$x,massid$x,massagg$x))
colnames(massagg_all)=c('dry','id','mass')
massagg1=subset(massagg_all,dry==1)
massagg0=subset(massagg_all,dry==0)
massagg_merge=merge(massagg1,massagg0,by='id')
diff=massagg_merge$mass.x-massagg_merge$mass.y
pctdes=(diff*-1)/massagg_merge$mass.y*100
pctdesic=subset(pctdes,pctdes>0)
mean(pctdesic,na.rm=TRUE)
massagg_merge=cbind(massagg_merge,diff,pctdes)
with(massagg_merge,{t.test(mass.x,mass.y,paired=TRUE)})

s=seq(length(massagg_merge$mass.x))
par(bty="l")
boxplot(massagg_merge$mass.x,massagg_merge$mass.y,xlab="Time",ylab="Measure",names=c("desiccated","hydrated"),col=c("lightblue","lightgreen"),ylim=c(400,1000))
stripchart(list(massagg_merge$mass.x,massagg_merge$mass.y),vertical=T,pch=16,cex=0.5,add=T)
segments(rep(0.95,length(massagg_merge$mass.x))[s],massagg_merge$mass.x[s],rep(2,length(massagg_merge$mass.x))[s],massagg_merge$mass.y[s],col='light grey',lwd=0.5)

# third, only including animals that remained active
massdata2009=subset(massdata,year==2009 & lizard_ID%in%activelizards)
massagg=aggregate(massdata2009$wgt,by=list(paste(massdata2009$lizard_ID,massdata2009$dryperiod,sep="_")),mean)
massdry=aggregate(massdata2009$dryperiod,by=list(paste(massdata2009$lizard_ID,massdata2009$dryperiod,sep="_")),mean)
massid=aggregate(massdata2009$lizard_ID,by=list(paste(massdata2009$lizard_ID,massdata2009$dryperiod,sep="_")),mean)
massagg_all=as.data.frame(cbind(massdry$x,massid$x,massagg$x))
colnames(massagg_all)=c('dry','id','mass')
massagg1=subset(massagg_all,dry==1)
massagg0=subset(massagg_all,dry==0)
massagg_merge=merge(massagg1,massagg0,by='id')
diff=massagg_merge$mass.x-massagg_merge$mass.y
pctdes=(diff*-1)/massagg_merge$mass.y*100
pctdesic=subset(pctdes,pctdes>0)
mean(pctdesic,na.rm=TRUE)
massagg_merge=cbind(massagg_merge,diff,pctdes)
with(massagg_merge,{t.test(mass.x,mass.y,paired=TRUE)})

s=seq(length(massagg_merge$mass.x))
par(bty="l")
boxplot(massagg_merge$mass.x,massagg_merge$mass.y,xlab="Time",ylab="Measure",names=c("desiccated","hydrated"),col=c("lightblue","lightgreen"),ylim=c(400,1000))
stripchart(list(massagg_merge$mass.x,massagg_merge$mass.y),vertical=T,pch=16,cex=0.5,add=T)
segments(rep(0.95,length(massagg_merge$mass.x))[s],massagg_merge$mass.x[s],rep(2,length(massagg_merge$mass.x))[s],massagg_merge$mass.y[s],col='light grey',lwd=0.5)

filename=paste("Appendix S1.pdf",sep="") 
pdf(filename,paper="A4r",width=15,height=11) # doing this means you're going to make a pdf - comment this line out if you want to see them in R
par(mfrow = c(2,2)) # set up for 5 plots in 2 columns
par(oma = c(2,2,2,2) + 0.1) # margin spacing stuff
par(mar = c(3,3,1,1) + 0.1) # margin spacing stuff 
par(mgp = c(3,1,0) ) # margin spacing stuff

############# start big loop to summarise all lizards, create Appendix 1 figs ######################
yearstodo=c(2009,2010)
for(m in 1:2){
  yeartodo=yearstodo[m]
  k=0
  # use individual 9 (burrows) or 115 (dam)
  for(i in 1:121){
    
    sleepy_id=waddlefiles[i,2]
    sexliz=subset(sex,Liz==sleepy_id)
    sexliz=sexliz[2]
    sexlizard=as.character(sexliz)
    if(sexlizard=="u" | sexlizard=="character(0)"){
      sexlizard='unknown'
    }
    if(sexlizard==2){
      sexlizard='F'
    }
    if(sexlizard==3){
      sexlizard='M'
    }
    
    #sleepy=as.data.frame(read.table(file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',sleepy_id,'_2009_ALL.csv',sep=""), sep = ",", head=TRUE))
    sleepy=as.data.frame(read.table(file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',waddlefiles[i,3],sep=""), sep = ",", head=TRUE))
    if(is.null(sleepy$Hours)){
      Hours=as.numeric(substr(sleepy$Time,1,2))
      Minutes=as.numeric(substr(sleepy$Time,4,5))
      sleepy=as.data.frame(cbind(sleepy,Hours,Minutes))
      write.table(sleepy,file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',waddlefiles[i,3],sep=""),row.names=F,sep=",") 
    }
    curyear=max(sleepy$Year,na.rm=TRUE)
    if(curyear==yeartodo){
      k=k+1
      plotrainfall = subset(rainfall,substr(dates,1,4)==curyear)
      environ_bask = subset(environ5,  subset=(ACT>=1 & TC>=TBASK & substr(dates,1,4)==curyear))
      environ_forage = subset(environ5,  subset=(ACT>1 & substr(dates,1,4)==curyear))
      environ_night= subset(metout,  subset=(ZEN==90))
      environ_night$TIME=environ_night$TIME/60-1
      date_waddle1=with(sleepy,ISOdatetime(Year,Month,Day,Hours,Minutes,0))
      date_waddle=date_waddle1+60*60 # adjust for Central Time
      doy=strptime(format(date_waddle, "%y/%m/%d"), "%y/%m/%d")$yday+1
      doy2=strptime(format(plotrainfall$dates, "%y/%m/%d"), "%y/%m/%d")$yday+1
      sleepy=cbind(date_waddle,doy,sleepy)
      plotrainfall2=cbind(doy2,plotrainfall)
      #plotgrass=cbind(doy2,plotgrass)
      colnames(plotrainfall2)=c("JULDAY","dates","RAINFALL")
      desic=subset(debout,TIME==24 & substr(dates,1,4)==curyear)
      desic=as.data.frame(cbind(desic[,2],desic[,20]))
      colnames(desic)=c('day','desic')
      
      date_waddle=aggregate(sleepy$date_waddle,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),min)
      doy=aggregate(sleepy$doy,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
      Tb=aggregate(sleepy$Temperature,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),mean)
      steps=aggregate(sleepy$Steps,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),sum)
      year=aggregate(sleepy$Year,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
      month=aggregate(sleepy$Month,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
      day=aggregate(sleepy$Day,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
      hours=aggregate(sleepy$Hours,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
      sleepy=as.data.frame(cbind(as.data.frame(date_waddle[,2]),doy[,2],Tb[,2],steps[,2],year[,2],month[,2],day[,2],hours[,2]))
      colnames(sleepy)=c('date_waddle','doy','Temperature','Steps','Year','Month','Day','Hours')
      
      # round the times to get rid of occasional 1 min addons
      x=sleepy$date_waddle
      r =  60*60
      H = as.integer(format(x, "%H"))
      M = as.integer(format(x, "%M"))
      S = as.integer(format(x, "%S"))
      D = format(x, "%Y-%m-%d")
      secs = 3600*H + 60*M + S
      x=as.POSIXct(round(secs/r)*r, origin=D)-11*3600
      sleepy$date_waddle=x
      # end rounding times
      
      sleepy2=as.data.frame(as.data.frame(cbind(date_waddle[,2],doy[,2],Tb[,2],steps[,2],year[,2],month[,2],day[,2],hours[,2],date_waddle$Group.1)))
      colnames(sleepy2)=c('date_waddle','doy','Temperature','Steps','Year','Month','Day','Hours','merge')
      
      sleepy2$Temperature=as.factor(sleepy$Temperature)
      
      correl=merge(sleepy2,environ5,by='merge')
      correl_day=subset(correl,ZEN!=90)
      c.doy=as.numeric(levels(correl$doy))[correl$doy]
      c.year=as.numeric(levels(correl$Year))[correl$Year]
      c.month=as.numeric(levels(correl$Month))[correl$Month]
      c.day=as.numeric(levels(correl$Day))[correl$Day]
      c.hour=as.data.frame(correl$TIME)
      c.Tb_obs=as.numeric(levels(correl$Temperature))[correl$Temperature]
      c.steps=as.numeric(levels(correl$Steps))[correl$Steps]
      c.Tb_pred=as.data.frame(correl$TC)
      c.act=as.data.frame(correl$ACT)
      c.shade=as.data.frame(correl$SHADE)
      correl2=cbind(correl$dates,c.doy,c.year,c.month,c.day,c.hour,c.Tb_obs,c.steps,c.Tb_pred,c.act,c.shade)
      colnames(correl2)=c('date','doy','year','month','day','hour','Tb_obs','steps','Tb_pred','act','shade')
      
      c.doy.day=as.numeric(levels(correl_day$doy))[correl_day$doy]
      c.year.day=as.numeric(levels(correl_day$Year))[correl_day$Year]
      c.month.day=as.numeric(levels(correl_day$Month))[correl_day$Month]
      c.day.day=as.numeric(levels(correl_day$Day))[correl_day$Day]
      c.hour.day=as.data.frame(correl_day$TIME)
      c.Tb_obs.day=as.numeric(levels(correl_day$Temperature))[correl_day$Temperature]
      c.steps.day=as.numeric(levels(correl_day$Steps))[correl_day$Steps]
      c.Tb_pred.day=as.data.frame(correl_day$TC)
      c.act.day=as.data.frame(correl_day$ACT)
      c.shade.day=as.data.frame(correl_day$SHADE)
      correl2.day=cbind(correl_day$dates,c.doy.day,c.year.day,c.month.day,c.day.day,c.hour.day,c.Tb_obs.day,c.steps.day,c.Tb_pred.day,c.act.day,c.shade.day)
      colnames(correl2.day)=c('date','doy','year','month','day','hour','Tb_obs','steps','Tb_pred','act','shade')
      
      mean_Tb_obs=mean(correl2$Tb_obs,na.rm=TRUE)
      max_Tb_obs=max(correl2$Tb_obs,na.rm=TRUE)
      min_Tb_obs=min(correl2$Tb_obs,na.rm=TRUE)
      med_Tb_obs=median(correl2$Tb_obs,na.rm=TRUE)
      mean_Tb_pred=mean(correl2$Tb_pred,na.rm=TRUE)
      max_Tb_pred=max(correl2$Tb_pred,na.rm=TRUE)
      min_Tb_pred=min(correl2$Tb_pred,na.rm=TRUE)
      med_Tb_pred=median(correl2$Tb_pred,na.rm=TRUE)
      
      lm_Tb=with(correl2,(lm(Tb_pred~Tb_obs)))
      r_Tb=with(correl2,cor(Tb_pred,Tb_obs))
      #with(correl2,(plot(Tb_pred~Tb_obs)))
      #abline(0,1)
      lm_Tb_R2=summary(lm_Tb)$r.squared
      lm_Tb_rmsd=sqrt(mean(((correl2$Tb_obs-correl2$Tb_pred)^2),na.rm=TRUE))
      #text(11,40,paste("r2=",round(lm_Tb_R2,2),"\n","rmsd=",round(lm_Tb_rmsd,2),sep=""))
      act.obs=ifelse(correl2$steps>threshold.act,1,0)
      act.pred=ifelse(correl2$act>0,1,0)
      #confus=confusion(act.pred,act.obs)
      confus=confusion(act.obs,act.pred)
      
      error=attributes(confus)
      confusion=as.data.frame(t(as.data.frame(c(confus[1:4],error$error))))
      rownames(confusion)=NULL
      colnames(confusion)=c('true-','false-','true+','false+','err')
      
      act.obs.day=ifelse(correl2.day$steps>threshold.act,1,0)
      act.pred.day=ifelse(correl2.day$act>0,1,0)
      #confus.day=confusion(act.pred.day,act.obs.day)
      confus.day=confusion(act.obs.day,act.pred.day)
      
      error.day=attributes(confus.day)
      confusion.day=as.data.frame(t(as.data.frame(c(confus.day[1:4],error.day$error))))
      rownames(confusion.day)=NULL
      colnames(confusion.day)=c('true-','false-','true+','false+','err')
      
      #sleepy$date_waddle=as.POSIXct(as.numeric(date_waddle[,2]+9.5*60*60),tz=tzone,origin="1970-01-01")
      plotlizard_forage = subset(sleepy,  subset=(Steps>threshold.act))
      plotlizard_noforage = subset(sleepy,  subset=(Steps>=0))
      if(k==1){
        allforage=cbind(plotlizard_forage,sexlizard)
        allnoforage=plotlizard_noforage
      }else{
        allforage=rbind(allforage,cbind(plotlizard_forage,sexlizard))
        allnoforage=rbind(allnoforage,plotlizard_noforage)
        
      }
      

      # layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE),
      #        widths = c(lcm(10),lcm(10)), heights = c(lcm(8),lcm(8)))
      startdy=240
      finishdy=365
      
      with(plotlizard_noforage, {plot(Hours+1~doy, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "",ylab = "",cex=1,pch=15,col='ivory 4')})
      mtext(text="hour of day",side=2, padj=-3)
      mtext(text="day of year",side=1, padj=3)
      
      with(environ_night, {points(TIME+2~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=0.5,col="dark grey",pch=16)})
      
      rbPal = colorRampPalette(c('turquoise','gold1'))
      
      #This adds a column of color values
      # based on the y values
      Tbs=t(as.numeric(as.matrix(c(environ_bask$TC,plotlizard_forage$Temperature))))
      Tbs=as.data.frame(t(Tbs))
      colnames(Tbs)='Tb'
      
      Tbs$Col=rbPal(23)[as.numeric(cut(Tbs$Tb, breaks=seq(18, 37,1), include.lowest=TRUE))]
      Tbs$Col=rbPal(23)[as.numeric(cut(Tbs$Tb, breaks=seq(18, 37,1), include.lowest=TRUE))]
      environ_bask$Col=Tbs[1:nrow(environ_bask),2]
      environ_bask$Col = rbPal(10)[as.numeric(cut(environ_bask$TC,breaks = 10))]
      #with(environ_forage, {points(TIME~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1.,col=environ_bask$Col,pch=15)})
      with(environ_bask, {points(TIME~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1.,col="light blue",pch=15)})
      with(environ_forage, {points(TIME~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1.,col="gold",pch=15)})
      with(plotlizard_forage, {points(Hours+1~doy, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=log(plotlizard_forage$Steps/300),pch=15,col=addTrans("red",100))}) #col="#0000FF96" #0000FFFF
      with(desic, {points(desic~day, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",lwd=2,pch=15,col='blue',type='l',lty=2)})
      with(plotrainfall2, {points(RAINFALL~JULDAY, type = "h",col='blue')})
      
      daystart=paste(substr(curyear,3,4),'/08/28',sep="") # y/m/d
      dayfin=paste(substr(curyear,3,4),'/12/31',sep="") # y/m/d
      plotlizard=subset(sleepy, format(sleepy$date_waddle, "%y/%m/%d")>= daystart & format(sleepy$date_waddle, "%y/%m/%d")<=dayfin)
      plotlizard = plotlizard[order(plotlizard$date_waddle),] 
      plotpred=subset(environ5, format(environ5$dates, "%y/%m/%d")>= daystart & format(environ5$dates, "%y/%m/%d")<=dayfin)
      plotdeb=subset(debout, format(debout$dates, "%y/%m/%d")>= daystart & format(environ$dates, "%y/%m/%d")<=dayfin)
      plotmetout=subset(metout, format(metout$dates, "%y/%m/%d")>= daystart & format(metout$dates, "%y/%m/%d")<=dayfin)
      with(plotpred, {plot(TC~dates,type = "l",ylim=c(0,40),ylab="",xlab="")})
      mtext(text="body temperature (deg C)",side=2, padj=-3)
      mtext(text="date",side=1, padj=3)
      plotlizard2=plotlizard
      colnames(plotlizard2)[1] = "dates"
      plotlizard2=merge(plotlizard2,plotpred,all=TRUE) # merge to avoid continuous line between sample gaps
      with(plotlizard2, {points(Temperature~dates,type = "l",col=addTrans("red",150))})
      with(plotdeb, {points(Body_cond~dates,type = "l",col='blue',lty=2,lwd=2)})
      with(plotrainfall2, {points(RAINFALL~dates, type = "h",col='blue')})
      with(environ, {points(CONDEP~dates, type = "l",col='light blue')})
      
      with(plotdeb,{plot((WETMASS-(CUMREPRO+CUMBATCH)/mu_E*23.9/0.3)~dates,type='l',ylim=c(400,1000),ylab="",xlab="")}) #plot non-reproductive weight
      mtext(text="non-reproductive mass, g",side=2, padj=-3)
      mtext(text="date",side=1, padj=3)
      
      massobs=subset(massdata,lizard_ID==sleepy_id)
      date_mass=as.POSIXct(with(massobs,ISOdatetime(year,month,day,0,0,0)))
      doymass=strptime(format(date_mass, "%y/%m/%d"), "%y/%m/%d")$yday+1
      massobs=cbind(massobs,date_mass,doymass)
      with(massobs,{points(wgt~date_mass,type='p',col='red')})
      
      startdy=298
      finishdy=334
      
      rbPal = colorRampPalette(c('turquoise','gold1'))
      
      #This adds a column of color values
      # based on the y values
      Tbs=t(as.numeric(as.matrix(c(environ_bask$TC,plotlizard_forage$Temperature))))
      Tbs=as.data.frame(t(Tbs))
      colnames(Tbs)='Tb'
      Tbs$Col=rbPal(20)[as.numeric(cut(Tbs$Tb, breaks=seq(18, 37,1), include.lowest=TRUE))]
      environ_bask$Col=Tbs[1:nrow(environ_bask),2]
      
      daystart=paste(substr(curyear,3,4),'/10/25',sep="") # y/m/d
      dayfin=paste(substr(curyear,3,4),'/11/30',sep="") # y/m/d
      plotlizard=subset(sleepy, format(sleepy$date_waddle, "%y/%m/%d")>= daystart & format(sleepy$date_waddle, "%y/%m/%d")<=dayfin)
      plotlizard = plotlizard[order(plotlizard$date_waddle),] 
      plotpred=subset(environ, format(environ$dates, "%y/%m/%d")>= daystart & format(environ$dates, "%y/%m/%d")<=dayfin)
      plotdeb=subset(debout, format(debout$dates, "%y/%m/%d")>= daystart & format(environ$dates, "%y/%m/%d")<=dayfin)
      plotmetout=subset(metout, format(metout$dates, "%y/%m/%d")>= daystart & format(metout$dates, "%y/%m/%d")<=dayfin)
      plotlizard2=plotlizard
      colnames(plotlizard2)[1] = "dates"
      plotlizard2=merge(plotlizard2,plotpred,all=TRUE) # merge to avoid continuous line between sample gaps
      
      with(correl2,(plot(Tb_pred~Tb_obs,ylim=c(0,40),xlim=c(0,40),xlab="",ylab="")))
      mtext(text="predicted Tb",side=2, padj=-3)
      mtext(text="observed Tb",side=1, padj=3)
      abline(0,1)
      text(5,35,paste("r=",round(r_Tb,2),"\n","rmsd=",round(lm_Tb_rmsd,2),sep=""))
      act.obs=ifelse(correl2$steps>threshold.act,1,0)
      act.pred=ifelse(correl2$act>0,1,0)  
      title(main=paste(sleepy_id," sex=",sexlizard," ",curyear,sep=" "),outer=T)
      
      
      if(k==1){
        summary=as.data.frame(cbind(as.numeric(sleepy_id),sexlizard,curyear,mean_Tb_obs,mean_Tb_pred,max_Tb_obs,max_Tb_pred,min_Tb_obs,min_Tb_pred,med_Tb_obs,med_Tb_pred,r_Tb,lm_Tb_rmsd,confusion,confusion.day))
      }else{
        summary=rbind(summary, as.data.frame(cbind(as.numeric(sleepy_id),sexlizard,curyear,mean_Tb_obs,mean_Tb_pred,max_Tb_obs,max_Tb_pred,min_Tb_obs,min_Tb_pred,med_Tb_obs,med_Tb_pred,r_Tb,lm_Tb_rmsd,confusion,confusion.day)))
      } 
    }
  } #end loop through lizards
  
  colnames(summary)=c('id','sex','year','mean_Tb_obs','mean_Tb_pred','max_Tb_obs','max_Tb_pred','min_Tb_obs','min_Tb_pred','med_Tb_obs','med_Tb_pred','Tb_r','Tb_rmsd','true-','false-','true+','false+','err','day.true-','day.false-','day.true+','day.false+','day.err')
  
  if(yeartodo==2009){
    if(raindrink==0){
      write.csv(summary,'waddle test/summary_drink_2009_2.csv')
    }else{
      write.csv(summary,'waddle test/summary_nodrink_2009_2.csv')
    }
  }else{
    if(raindrink==0){
      write.csv(summary,'waddle test/summary_drink_2010_2.csv')
    }else{
      write.csv(summary,'waddle test/summary_nodrink_2010_2.csv')
    }    
  }   
}
dev.off()

summary2009=read.csv('waddle test/summary_nodrink_2009_2.csv')
summary2010=read.csv('waddle test/summary_nodrink_2010_2.csv')

summary_all=rbind(summary2009,summary2010)
max(summary_all$max_Tb_obs)
min(summary_all$min_Tb_obs)
mean(summary_all$Tb_rmsd)
max(summary_all$Tb_rmsd)
min(summary_all$Tb_rmsd)
mean(summary_all$Tb_r)
max(summary_all$Tb_r)
min(summary_all$Tb_r)
mean(summary_all$err)
max(summary_all$err)
min(summary_all$err)
mean(summary_all$day.err)
max(summary_all$day.err)
min(summary_all$day.err)
############# end big loop to summarise all lizards, create Appendix 1 figs ######################


######################### mean activity plots ######################################

k=0
liz2do=1
for(i in 1:121){
  #for(i in 1:121){
  sleepy_id=waddlefiles[i,2]
  sexliz=subset(sex,Liz==sleepy_id)
  sexliz=sexliz[2]
  sexlizard=as.character(sexliz)
  if(sexlizard=="u" | sexlizard=="character(0)"){
    sexlizard='unknown'
  }
  if(sexlizard==2){
    sexlizard='F'
  }
  if(sexlizard==3){
    sexlizard='M'
  }
  #sleepy=as.data.frame(read.table(file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',sleepy_id,'_2009_ALL.csv',sep=""), sep = ",", head=TRUE))
  sleepy=as.data.frame(read.table(file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',waddlefiles[i,3],sep=""), sep = ",", head=TRUE))
  if(is.null(sleepy$Hours)){
    Hours=as.numeric(substr(sleepy$Time,1,2))
    Minutes=as.numeric(substr(sleepy$Time,4,5))
    sleepy=as.data.frame(cbind(sleepy,Hours,Minutes))
    write.table(sleepy,file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',waddlefiles[i,3],sep=""),row.names=F,sep=",") 
  }
  curyear=max(sleepy$Year,na.rm=TRUE)
  if(curyear==2010){
    k=k+1
    plotrainfall = subset(rainfall,substr(dates,1,4)==curyear)
    environ_bask = subset(environ5,  subset=(ACT>=1 & TC>=TBASK & substr(dates,1,4)==curyear))
    environ_forage = subset(environ5,  subset=(ACT>1 & substr(dates,1,4)==curyear))
    environ_night= subset(metout,  subset=(ZEN==90))
    environ_night$TIME=environ_night$TIME/60-1
    date_waddle1=with(sleepy,ISOdatetime(Year,Month,Day,Hours,Minutes,0))
    date_waddle=date_waddle1+60*30 # adjust for Central Time
    doy=strptime(format(date_waddle, "%y/%m/%d"), "%y/%m/%d")$yday+1
    doy2=strptime(format(plotrainfall$dates, "%y/%m/%d"), "%y/%m/%d")$yday+1
    sleepy=cbind(date_waddle,doy,sleepy)
    plotrainfall2=cbind(doy2,plotrainfall)
    #plotgrass=cbind(doy2,plotgrass)
    colnames(plotrainfall2)=c("JULDAY","dates","RAINFALL")
    desic=subset(debout,TIME==24 & substr(dates,1,4)==curyear)
    desic=as.data.frame(cbind(desic[,2],desic[,20]))
    colnames(desic)=c('day','desic')
    
    date_waddle=aggregate(sleepy$date_waddle,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),min)
    doy=aggregate(sleepy$doy,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    Tb=aggregate(sleepy$Temperature,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),mean)
    steps=aggregate(sleepy$Steps,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),sum)
    year=aggregate(sleepy$Year,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    month=aggregate(sleepy$Month,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    day=aggregate(sleepy$Day,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    hours=aggregate(sleepy$Hours,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    sleepy=as.data.frame(cbind(date_waddle[,2],doy[,2],Tb[,2],steps[,2],year[,2],month[,2],day[,2],hours[,2]))
    sleepy2=as.data.frame(cbind(date_waddle[,2],doy[,2],Tb[,2],steps[,2],year[,2],month[,2],day[,2],hours[,2],date_waddle$Group.1))
    colnames(sleepy)=c('date_waddle','doy','Temperature','Steps','Year','Month','Day','Hours')
    colnames(sleepy2)=c('date_waddle','doy','Temperature','Steps','Year','Month','Day','Hours','merge')
    
    
    sleepy$date_waddle=as.POSIXct(as.numeric(date_waddle[,2]+9.5*60*60),tz=tzone,origin="1970-01-01")
    plotlizard_forage = subset(sleepy,  subset=(Steps>threshold.act))
    plotlizard_noforage = subset(sleepy,  subset=(Steps>=0))
    if(k==1){
      allforage=cbind(plotlizard_forage,sexlizard)
      allnoforage=plotlizard_noforage
      allwaddleobs=sleepy
    }else{
      allforage=rbind(allforage,cbind(plotlizard_forage,sexlizard))
      allnoforage=rbind(allnoforage,plotlizard_noforage)
      allwaddleobs=rbind(allwaddleobs,sleepy)
    }
  }
} #end loop through lizards

allforage_2009=allforage
allnoforage_2009=allnoforage
allwaddleobs_2009=allwaddleobs
allforage_2010=allforage
allnoforage_2010=allnoforage
allwaddleobs_2010=allwaddleobs
all=rbind(allforage_2009[,1:8],allnoforage_2009,allforage_2010[,1:8],allnoforage_2010)
describe(all$Temperature)
startdy=250
finishdy=355

with(environ_night, {plot(TIME+2~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=0.5,col="dark grey",pch=16)})

rbPal = colorRampPalette(c('turquoise','gold1'))
#This adds a column of color values
# based on the y values
Tbs=t(as.numeric(as.matrix(c(environ_bask$TC,plotlizard_forage$Temperature))))
Tbs=as.data.frame(t(Tbs))
colnames(Tbs)='Tb'
Tbs$Col=rbPal(23)[as.numeric(cut(Tbs$Tb, breaks=seq(18, 37,1), include.lowest=TRUE))]
environ_bask$Col=Tbs[1:nrow(environ_bask),2]
with(environ_bask, {points(TIME~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1.,col="light blue",pch=15)})
with(environ_forage, {points(TIME~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1.,col="light blue",pch=15)})
with(plotrainfall2, {points(RAINFALL~JULDAY, type = "h",col='blue')})

meanwaddle=aggregate(allforage$Steps,by=list(paste(allforage$Year,"_",allforage$Month,"_",allforage$Day,"_",allforage$Hours,sep="")),sum)
meanhour=aggregate(allforage$Hours,by=list(paste(allforage$Year,"_",allforage$Month,"_",allforage$Day,"_",allforage$Hours,sep="")),mean)
meandoy=aggregate(allforage$doy,by=list(paste(allforage$Year,"_",allforage$Month,"_",allforage$Day,"_",allforage$Hours,sep="")),mean)
meanwaddle=cbind(meanwaddle,meanhour[,2],meandoy[,2])
colnames(meanwaddle)=c('date','Steps','Hours','doy')
with(meanwaddle, {points(Hours+1~doy, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=meanwaddle$Steps/25000,pch=15,col=addTrans("red",100))}) #col="#0000FF96" #0000FFFF



################### Figure 2 ######################################

lizards=subset(waddlefiles,year==2010)

#sleepy_id=waddlefiles[i,2]
sleepy_id=11533
sleepy_id=40044
sleepy_id=11885
sexliz=subset(sex,Liz==sleepy_id)
sexliz=sexliz[2]
sexlizard=as.character(sexliz)
if(sexlizard=="u" | sexlizard=="character(0)"){
  sexlizard='unknown'
}
if(sexlizard==2){
  sexlizard='F'
}
if(sexlizard==3){
  sexlizard='M'
}

sleepy=as.data.frame(read.table(file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',sleepy_id,'_2010_ALL.csv',sep=""), sep = ",", head=TRUE))

curyear=max(sleepy$Year,na.rm=TRUE)
plotrainfall = subset(rainfall,substr(dates,1,4)==curyear)
environ_bask = subset(environ5,  subset=(ACT>=1 & TC>=TBASK & substr(dates,1,4)==curyear))
environ_forage = subset(environ5,  subset=(ACT>1 & substr(dates,1,4)==curyear))
environ_night= subset(metout,  subset=(ZEN==90))
environ_night$TIME=environ_night$TIME/60-1
date_waddle1=with(sleepy,ISOdatetime(Year,Month,Day,Hours,Minutes,0))
date_waddle=date_waddle1+60*60 # adjust for Central Time
doy=strptime(format(date_waddle, "%y/%m/%d"), "%y/%m/%d")$yday+1
doy2=strptime(format(plotrainfall$dates, "%y/%m/%d"), "%y/%m/%d")$yday+1
sleepy=cbind(date_waddle,doy,sleepy)
plotrainfall2=cbind(doy2,plotrainfall)
colnames(plotrainfall2)=c("JULDAY","dates","RAINFALL")
desic=subset(debout,TIME==24 & substr(dates,1,4)==curyear)
desic=as.data.frame(cbind(desic[,2],desic[,20]))
colnames(desic)=c('day','desic')

date_waddle=aggregate(sleepy$date_waddle,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),min)
doy=aggregate(sleepy$doy,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
Tb=aggregate(sleepy$Temperature,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),mean)
steps=aggregate(sleepy$Steps,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),sum)
year=aggregate(sleepy$Year,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
month=aggregate(sleepy$Month,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
day=aggregate(sleepy$Day,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
hours=aggregate(sleepy$Hours,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
sleepy=as.data.frame(cbind(as.data.frame(date_waddle[,2]),doy[,2],Tb[,2],steps[,2],year[,2],month[,2],day[,2],hours[,2]))

colnames(sleepy)=c('date_waddle','doy','Temperature','Steps','Year','Month','Day','Hours')

# round the times to get rid of occasional 1 min addons
x=sleepy$date_waddle
r =  60*60
H = as.integer(format(x, "%H"))
M = as.integer(format(x, "%M"))
S = as.integer(format(x, "%S"))
D = format(x, "%Y-%m-%d")
secs = 3600*H + 60*M + S
x=as.POSIXct(round(secs/r)*r, origin=D)-11*3600
sleepy$date_waddle=x
# end rounding times

sleepy2=as.data.frame(as.data.frame(cbind(date_waddle[,2],doy[,2],Tb[,2],steps[,2],year[,2],month[,2],day[,2],hours[,2],date_waddle$Group.1)))
colnames(sleepy2)=c('date_waddle','doy','Temperature','Steps','Year','Month','Day','Hours','merge')
sleepy2$Temperature=as.factor(sleepy$Temperature)
plotlizard_forage = subset(sleepy,  subset=(Steps>threshold.act))
plotlizard_noforage = subset(sleepy,  subset=(Steps>=0))

with(plotlizard_noforage, {plot(Hours+1~doy, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "",ylab = "",cex=1,pch=15,col='white')})
mtext(text="hour of day",side=2, padj=-3)
mtext(text="day of year",side=1, padj=3)

with(environ_night, {points(TIME+2~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=0.5,col="dark grey",pch=16)})

rbPal = colorRampPalette(c('turquoise','gold1'))

#This adds a column of color values
# based on the y values
Tbs=t(as.numeric(as.matrix(c(environ_bask$TC,plotlizard_forage$Temperature))))
Tbs=as.data.frame(t(Tbs))
colnames(Tbs)='Tb'

Tbs$Col=rbPal(23)[as.numeric(cut(Tbs$Tb, breaks=seq(18, 37,1), include.lowest=TRUE))]
Tbs$Col=rbPal(23)[as.numeric(cut(Tbs$Tb, breaks=seq(18, 37,1), include.lowest=TRUE))]
environ_bask$Col=Tbs[1:nrow(environ_bask),2]
environ_bask$Col = rbPal(10)[as.numeric(cut(environ_bask$TC,breaks = 10))]
#with(environ_forage, {points(TIME~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1.,col=environ_bask$Col,pch=15)})
with(environ_bask, {points(TIME~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1.,col="light blue",pch=15)})
with(environ_forage, {points(TIME~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1.,col="light blue",pch=15)})
with(plotlizard_forage, {points(Hours+1~doy, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=log(plotlizard_forage$Steps/300),pch=15,col=addTrans("red",100))}) #col="#0000FF96" #0000FFFF
with(desic, {points(desic~day, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",lwd=2,pch=15,col='blue',type='l',lty=2)})
with(plotrainfall2, {points(RAINFALL~JULDAY, type = "h",col='blue')})

################### end Figure 2 ######################################



daystart=paste(substr(curyear,3,4),'/08/28',sep="") # y/m/d
dayfin=paste(substr(curyear,3,4),'/12/31',sep="") # y/m/d
plotlizard=subset(sleepy, format(sleepy$date_waddle, "%y/%m/%d")>= daystart & format(sleepy$date_waddle, "%y/%m/%d")<=dayfin)
plotlizard = plotlizard[order(plotlizard$date_waddle),] 
plotpred=subset(environ5, format(environ5$dates, "%y/%m/%d")>= daystart & format(environ5$dates, "%y/%m/%d")<=dayfin)
plotdeb=subset(debout, format(debout$dates, "%y/%m/%d")>= daystart & format(environ$dates, "%y/%m/%d")<=dayfin)
plotmetout=subset(metout, format(metout$dates, "%y/%m/%d")>= daystart & format(metout$dates, "%y/%m/%d")<=dayfin)
with(plotpred, {plot(TC~dates,type = "l",ylim=c(0,45),ylab="",xlab="")})
mtext(text="body temperature (deg C)",side=2, padj=-3)
mtext(text="date",side=1, padj=3)
plotlizard2=plotlizard
colnames(plotlizard2)[1] = "dates"
plotlizard2=merge(plotlizard2,plotpred,all=TRUE) # merge to avoid continuous line between sample gaps
with(plotlizard2, {points(Temperature~dates,type = "l",col=addTrans("red",150))})
with(plotdeb, {points(Body_cond~dates,type = "l",col='blue',lty=2,lwd=2)})
with(plotrainfall2, {points(RAINFALL~dates, type = "h",col='blue')})
with(environ, {points(CONDEP~dates, type = "l",col='light blue')})

all=rbind(allforage[,1:8],allnoforage)
meanTb_obs=aggregate(all$Temperature,by=list(paste(all$Year,"_",all$Month,"_",all$Day,"_",all$Hours,sep="")),mean)
maxTb_obs=aggregate(all$Temperature,by=list(paste(all$Year,"_",all$Month,"_",all$Day,"_",all$Hours,sep="")),max)
minTb_obs=aggregate(all$Temperature,by=list(paste(all$Year,"_",all$Month,"_",all$Day,"_",all$Hours,sep="")),min)
meanhour=aggregate(all$Hours,by=list(paste(all$Year,"_",all$Month,"_",all$Day,"_",all$Hours,sep="")),mean)
meandoy=aggregate(all$doy,by=list(paste(all$Year,"_",all$Month,"_",all$Day,"_",all$Hours,sep="")),mean)
meanTb_obs=cbind(meanTb_obs,maxTb_obs[,2],minTb_obs[,2],meanhour[,2],meandoy[,2])
colnames(meanTb_obs)=c('merge','Tb','Tbmax','Tbmin','Hours','doy')
meanTb_obs$day=meanTb_obs$doy+meanTb_obs$Hours/24
meanTb_obs=meanTb_obs[order(meanTb_obs$day),]
plotpred$merge=as.character(plotpred$merge)
all_merged=merge(meanTb_obs,plotpred)

plotpred$day=plotpred$JULDAY+plotpred$TIME/24
plot(plotpred$TC~plotpred$day,type='l',cex=0.5,ylim=c(0,50))
points(meanTb_obs$Tb~meanTb_obs$day,type='l',col='red',cex=0.5,pch=16)
points(meanTb_obs$Tbmin~meanTb_obs$day,type='b',col='blue',cex=0.5,pch=16)
points(meanTb_obs$Tbmax~meanTb_obs$day,type='b',col='orange',cex=0.5,pch=16)

polygon(c(meanTb_obs$day,meanTb_obs$day),c(meanTb_obs$Tbmin,meanTb_obs$Tbmax),col=gray(0.8),border=NA)
polygon(c(meanTb_obs$day,meanTb_obs$day),c(meanTb_obs$Tbmax,meanTb_obs$Tbmin),col=gray(0.8),border=NA)

startdy=1
finishdy=365

with(environ_night, {plot(TIME+2~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=0.5,col=addTrans("dark grey",50),pch=16)})

rbPal = colorRampPalette(c('turquoise','gold1'))

#This adds a column of color values
# based on the y values
Tbs=t(as.numeric(as.matrix(c(environ_bask$TC,plotlizard_forage$Temperature))))
Tbs=as.data.frame(t(Tbs))
colnames(Tbs)='Tb'
Tbs$Col=rbPal(23)[as.numeric(cut(Tbs$Tb, breaks=seq(18, 37,1), include.lowest=TRUE))]
environ_bask$Col=Tbs[1:nrow(environ_bask),2]
with(environ_bask, {points(TIME~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=.75,col=environ_bask$Col,pch=15)})
with(desic, {points(desic~day, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",lwd=2,pch=15,col='blue',type='l',lty=2)})
with(plotrainfall2, {points(RAINFALL~JULDAY, type = "h",col='blue')})


################### Figure 3 ######################################

lizards=subset(waddlefiles,year==2009)

sleepy_id=11129
sleepy_id=12434
sexliz=subset(sex,Liz==sleepy_id)
sexliz=sexliz[2]
sexlizard=as.character(sexliz)
if(sexlizard=="u" | sexlizard=="character(0)"){
  sexlizard='unknown'
}
if(sexlizard==2){
  sexlizard='F'
}
if(sexlizard==3){
  sexlizard='M'
}

sleepy=as.data.frame(read.table(file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',sleepy_id,'_2009_ALL.csv',sep=""), sep = ",", head=TRUE))

curyear=max(sleepy$Year,na.rm=TRUE)
plotrainfall = subset(rainfall,substr(dates,1,4)==curyear)
environ_bask = subset(environ5,  subset=(ACT>=1 & TC>=TBASK & substr(dates,1,4)==curyear))
environ_forage = subset(environ5,  subset=(ACT>1 & substr(dates,1,4)==curyear))
environ_night= subset(metout,  subset=(ZEN==90))
environ_night$TIME=environ_night$TIME/60-1
date_waddle1=with(sleepy,ISOdatetime(Year,Month,Day,Hours,Minutes,0))
date_waddle=date_waddle1+60*60 # adjust for Central Time
doy=strptime(format(date_waddle, "%y/%m/%d"), "%y/%m/%d")$yday+1
doy2=strptime(format(plotrainfall$dates, "%y/%m/%d"), "%y/%m/%d")$yday+1
sleepy=cbind(date_waddle,doy,sleepy)
plotrainfall2=cbind(doy2,plotrainfall)
colnames(plotrainfall2)=c("JULDAY","dates","RAINFALL")
desic=subset(debout,TIME==24 & substr(dates,1,4)==curyear)
desic=as.data.frame(cbind(desic[,2],desic[,20]))
colnames(desic)=c('day','desic')

date_waddle=aggregate(sleepy$date_waddle,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),min)
doy=aggregate(sleepy$doy,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
Tb=aggregate(sleepy$Temperature,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),mean)
steps=aggregate(sleepy$Steps,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),sum)
year=aggregate(sleepy$Year,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
month=aggregate(sleepy$Month,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
day=aggregate(sleepy$Day,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
hours=aggregate(sleepy$Hours,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
sleepy=as.data.frame(cbind(as.data.frame(date_waddle[,2]),doy[,2],Tb[,2],steps[,2],year[,2],month[,2],day[,2],hours[,2]))

colnames(sleepy)=c('date_waddle','doy','Temperature','Steps','Year','Month','Day','Hours')

# round the times to get rid of occasional 1 min addons
x=sleepy$date_waddle
r =  60*60

H = as.integer(format(x, "%H"))
M = as.integer(format(x, "%M"))
S = as.integer(format(x, "%S"))
D = format(x, "%Y-%m-%d")
secs = 3600*H + 60*M + S
x=as.POSIXct(round(secs/r)*r, origin=D)-11*3600
sleepy$date_waddle=x
# end rounding times

sleepy2=as.data.frame(as.data.frame(cbind(date_waddle[,2],doy[,2],Tb[,2],steps[,2],year[,2],month[,2],day[,2],hours[,2],date_waddle$Group.1)))
colnames(sleepy2)=c('date_waddle','doy','Temperature','Steps','Year','Month','Day','Hours','merge')
sleepy2$Temperature=as.factor(sleepy$Temperature)

correl=merge(sleepy2,environ5,by='merge')
correl_day=subset(correl,ZEN!=90)
c.doy=as.numeric(levels(correl$doy))[correl$doy]
c.year=as.numeric(levels(correl$Year))[correl$Year]
c.month=as.numeric(levels(correl$Month))[correl$Month]
c.day=as.numeric(levels(correl$Day))[correl$Day]
c.hour=as.data.frame(correl$TIME)
c.Tb_obs=as.numeric(levels(correl$Temperature))[correl$Temperature]
c.steps=as.numeric(levels(correl$Steps))[correl$Steps]
c.Tb_pred=as.data.frame(correl$TC)
c.act=as.data.frame(correl$ACT)
c.shade=as.data.frame(correl$SHADE)
correl2=cbind(correl$dates,c.doy,c.year,c.month,c.day,c.hour,c.Tb_obs,c.steps,c.Tb_pred,c.act,c.shade)
colnames(correl2)=c('date','doy','year','month','day','hour','Tb_obs','steps','Tb_pred','act','shade')

c.doy.day=as.numeric(levels(correl_day$doy))[correl_day$doy]
c.year.day=as.numeric(levels(correl_day$Year))[correl_day$Year]
c.month.day=as.numeric(levels(correl_day$Month))[correl_day$Month]
c.day.day=as.numeric(levels(correl_day$Day))[correl_day$Day]
c.hour.day=as.data.frame(correl_day$TIME)
c.Tb_obs.day=as.numeric(levels(correl_day$Temperature))[correl_day$Temperature]
c.steps.day=as.numeric(levels(correl_day$Steps))[correl_day$Steps]
c.Tb_pred.day=as.data.frame(correl_day$TC)
c.act.day=as.data.frame(correl_day$ACT)
c.shade.day=as.data.frame(correl_day$SHADE)
correl2.day=cbind(correl_day$dates,c.doy.day,c.year.day,c.month.day,c.day.day,c.hour.day,c.Tb_obs.day,c.steps.day,c.Tb_pred.day,c.act.day,c.shade.day)
colnames(correl2.day)=c('date','doy','year','month','day','hour','Tb_obs','steps','Tb_pred','act','shade')

mean_Tb_obs=mean(correl2$Tb_obs,na.rm=TRUE)
max_Tb_obs=max(correl2$Tb_obs,na.rm=TRUE)
min_Tb_obs=min(correl2$Tb_obs,na.rm=TRUE)
med_Tb_obs=median(correl2$Tb_obs,na.rm=TRUE)
mean_Tb_pred=mean(correl2$Tb_pred,na.rm=TRUE)
max_Tb_pred=max(correl2$Tb_pred,na.rm=TRUE)
min_Tb_pred=min(correl2$Tb_pred,na.rm=TRUE)
med_Tb_pred=median(correl2$Tb_pred,na.rm=TRUE)


lm_Tb=with(correl2,(lm(Tb_pred~Tb_obs)))
r_Tb=with(correl2,cor(Tb_pred,Tb_obs))
lm_Tb_R2=summary(lm_Tb)$r.squared
lm_Tb_rmsd=sqrt(mean(((correl2$Tb_obs-correl2$Tb_pred)^2),na.rm=TRUE))
act.obs=ifelse(correl2$steps>threshold.act,1,0)
act.pred=ifelse(correl2$act>0,1,0)
confus=confusion(act.obs,act.pred)

error=attributes(confus)
confusion=as.data.frame(t(as.data.frame(c(confus[1:4],error$error))))
rownames(confusion)=NULL
colnames(confusion)=c('true-','false-','true+','false+','err')

act.obs.day=ifelse(correl2.day$steps>threshold.act,1,0)
act.pred.day=ifelse(correl2.day$act>0,1,0)
confus.day=confusion(act.obs.day,act.pred.day)

error.day=attributes(confus.day)
confusion.day=as.data.frame(t(as.data.frame(c(confus.day[1:4],error.day$error))))
rownames(confusion.day)=NULL
colnames(confusion.day)=c('true-','false-','true+','false+','err')


#sleepy$date_waddle=as.POSIXct(as.numeric(date_waddle[,2]+9.5*60*60),tz=tzone,origin="1970-01-01")
plotlizard_forage = subset(sleepy,  subset=(Steps>threshold.act))
plotlizard_noforage = subset(sleepy,  subset=(Steps>=0))

startdy=240
finishdy=365

with(plotlizard_noforage, {plot(Hours+1~doy, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "",ylab = "",cex=1,pch=15,col='white')})
mtext(text="hour of day",side=2, padj=-3)
mtext(text="day of year",side=1, padj=3)

with(environ_night, {points(TIME+2~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=0.5,col="dark grey",pch=16)})

rbPal = colorRampPalette(c('turquoise','gold1'))

#This adds a column of color values
# based on the y values
Tbs=t(as.numeric(as.matrix(c(environ_bask$TC,plotlizard_forage$Temperature))))
Tbs=as.data.frame(t(Tbs))
colnames(Tbs)='Tb'

Tbs$Col=rbPal(23)[as.numeric(cut(Tbs$Tb, breaks=seq(18, 37,1), include.lowest=TRUE))]
Tbs$Col=rbPal(23)[as.numeric(cut(Tbs$Tb, breaks=seq(18, 37,1), include.lowest=TRUE))]
environ_bask$Col=Tbs[1:nrow(environ_bask),2]
environ_bask$Col = rbPal(10)[as.numeric(cut(environ_bask$TC,breaks = 10))]
#with(environ_forage, {points(TIME~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1.,col=environ_bask$Col,pch=15)})
with(environ_bask, {points(TIME~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1.,col="light blue",pch=15)})
with(environ_forage, {points(TIME~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1.,col="gold",pch=15)})
with(plotlizard_forage, {points(Hours+1~doy, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=log(plotlizard_forage$Steps/300),pch=15,col=addTrans("red",100))}) #col="#0000FF96" #0000FFFF
with(desic, {points(desic~day, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",lwd=2,pch=15,col='blue',type='l',lty=2)})
with(plotrainfall2, {points(RAINFALL~JULDAY, type = "h",col='blue')})


daystart=paste(substr(curyear,3,4),'/08/28',sep="") # y/m/d
dayfin=paste(substr(curyear,3,4),'/12/31',sep="") # y/m/d
plotlizard=subset(sleepy, format(sleepy$date_waddle, "%y/%m/%d")>= daystart & format(sleepy$date_waddle, "%y/%m/%d")<=dayfin)
plotlizard = plotlizard[order(plotlizard$date_waddle),] 
plotpred=subset(environ5, format(environ5$dates, "%y/%m/%d")>= daystart & format(environ5$dates, "%y/%m/%d")<=dayfin)
plotdeb=subset(debout, format(debout$dates, "%y/%m/%d")>= daystart & format(environ$dates, "%y/%m/%d")<=dayfin)
plotmetout=subset(metout, format(metout$dates, "%y/%m/%d")>= daystart & format(metout$dates, "%y/%m/%d")<=dayfin)
with(plotpred, {plot(TC~dates,type = "l",ylim=c(0,45),ylab="",xlab="")})
mtext(text="body temperature (deg C)",side=2, padj=-3)
mtext(text="date",side=1, padj=3)
plotlizard2=plotlizard
colnames(plotlizard2)[1] = "dates"
plotlizard2=merge(plotlizard2,plotpred,all=TRUE) # merge to avoid continuous line between sample gaps
with(plotlizard2, {points(Temperature~dates,type = "l",col=addTrans("red",150))})
with(plotdeb, {points(Body_cond~dates,type = "l",col='blue',lty=2,lwd=2)})
with(plotrainfall2, {points(RAINFALL~dates, type = "h",col='blue')})
with(environ, {points(CONDEP~dates, type = "l",col='light blue')})


#########################Figure S5##################################

# ######## comparison with Fig. 5 Kerr 2004 #############

plotmasbal = subset(masbal, format(masbal$dates, "%y")=="02"  | format(masbal$dates, "%y")=="03")
plotdebout = subset(debout, format(debout$dates, "%y")=="02"  | format(debout$dates, "%y")=="03")
plotenviron = subset(environ, format(environ$dates, "%y")=="02"  | format(environ$dates, "%y")=="03")
plotrainfall = subset(rainfall,format(dates, "%y")=="02"  | format(dates, "%y")=="03")
plotgrass = subset(grass,format(grass$dates, "%y")=="02"  | format(grass$dates, "%y")=="03")
with(plotdebout, {plot(WETMASS~dates,type = "l",ylab = "wet mass (g)/grass growth",ylim=c(400,950),groups=Stage,auto.key=list(columns = 5))})
Kerr_fig5=read.csv("c:/NicheMapR_Working/projects/Sleepy Lizards/Kerr_Fig5.csv")
date_Kerr=with(Kerr_fig5,ISOdatetime(Year,Month,Day,0,0,0))
date_Kerr=date_Kerr
Kerr_fig5=cbind(date_Kerr,Kerr_fig5)
with(plotdebout, {plot(WETMASS_STD~dates,type = "l",ylab = "wet mass (g)/grass growth",ylim=c(400,950),groups=Stage,auto.key=list(columns = 5))})

with(Kerr_fig5,points(Mass~date_Kerr,type='p',col='blue'))
with(plotdebout, {plot(Body_cond~dates,type = "l",ylim=c(-2,50),xlab = "year", ylab = "% desiccated",col='dark grey')})
with(Kerr_fig5,points(Mass/20~date_Kerr,type='p',col='blue'))
with(plotenviron, {xyplot(TC+ACT*10+SHADE/10+DEP/10~dates,type = "l")})


environ_bask = subset(plotenviron,  subset=(ACT>=1 & TC>=VTMIN))
environ_forage = subset(plotenviron,  subset=(ACT>1))
environ_trap = subset(plotenviron,  subset=(ACT>=1 & SHADE==0 & TC>=VTMIN))
with(environ_bask, {plot(TIME~DAY, ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1,col='gold',pch=15)})

juldays=rep(seq(1,365*nyears,1),24)
juldays=juldays[order(juldays)]
doy1=seq(1,365*20,1)

DAY=plotenviron$DAY
environ_night=cbind(metout,DAY)
environ_night= subset(environ_night,  subset=(ZEN==90))

with(environ_night, {points(TIME/60+1~DAY, ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=0.5,col="dark grey",pch=16)})

doy2=strptime(format(rainfall$dates, "%y/%m/%d"), "%y/%m/%d")$yday+1
plotrainfall2=cbind(doy2,rainfall)
colnames(plotrainfall2)=c("JULDAY","dates","RAINFALL")
with(plotrainfall2, {points(RAINFALL~doy1, type = "h",col='blue')})

desic=subset(debout,TIME==24 )
desic=as.data.frame(cbind(desic[,2],desic[,20]))
colnames(desic)=c('day','desic')
doy2=as.numeric(rownames(desic))
with(desic, {points(desic~doy2, ylim=c(0,25),xlab = "day of year",ylab = "hour of day",lwd=1,pch=15,col='dark grey',type='l',lty=2)})

wetgut=as.data.frame(((plotdebout$MASS_GUT/mu_E)*23.9)/0.3)
wetgut=cbind(plotdebout$dates,wetgut)
colnames(wetgut)=c("dates","wetgut")
#with(wetgut,plot(wetgut~dates,ylim='wet gut, g',type='l',ylim=c(-.25,70)))

#male
with(plotdebout, {plot((WETMASS_STD-WETMASS_STD*(Body_cond/100))~dates,ylim=c(400,750),type = "l",xlab = "day of year",ylab = "wet mass (g)")})
with(plotdebout, {points(WETMASS_STD~dates,type = "l",col='4',ylim=c(400,650))})
with(Kerr_fig5,points(Mass~date_Kerr,type='b',col='red'))
###############################################


###########################Figure 1#######################################

######## compute average Tb obs vs Tb pred #############
yearstodo=c(2009,2010)
for(m in 1:2){
  yeartodo=yearstodo[m]
  k=0
  # use individual 9 (burrows) or 115 (dam)
  for(i in 1:121){
    sleepy_id=waddlefiles[i,2]
    sexliz=subset(sex,Liz==sleepy_id)
    sexliz=sexliz[2]
    sexlizard=as.character(sexliz)
    if(sexlizard=="integer(0)"){
      sexlizard='unknown'
    }
    if(sexlizard==2){
      sexlizard='F'
    }
    if(sexlizard==3){
      sexlizard='M'
    }
    #sleepy=as.data.frame(read.table(file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',sleepy_id,'_2009_ALL.csv',sep=""), sep = ",", head=TRUE))
    sleepy=as.data.frame(read.table(file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',waddlefiles[i,3],sep=""), sep = ",", head=TRUE))
    if(is.null(sleepy$Hours)){
      Hours=as.numeric(substr(sleepy$Time,1,2))
      Minutes=as.numeric(substr(sleepy$Time,4,5))
      sleepy=as.data.frame(cbind(sleepy,Hours,Minutes))
      write.table(sleepy,file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',waddlefiles[i,3],sep=""),row.names=F,sep=",") 
    }
    curyear=max(sleepy$Year,na.rm=TRUE)
    if(curyear==yeartodo){
      k=k+1
      plotrainfall = subset(rainfall,substr(dates,1,4)==curyear)
      environ_bask = subset(environ5,  subset=(ACT>=1 & TC>=TBASK & substr(dates,1,4)==curyear))
      environ_forage = subset(environ5,  subset=(ACT>1 & substr(dates,1,4)==curyear))
      environ_night= subset(metout,  subset=(ZEN==90))
      environ_night$TIME=environ_night$TIME/60-1
      date_waddle1=with(sleepy,ISOdatetime(Year,Month,Day,Hours,Minutes,0))
      date_waddle=date_waddle1+60*60 # adjust for Central Time
      doy=strptime(format(date_waddle, "%y/%m/%d"), "%y/%m/%d")$yday+1
      doy2=strptime(format(plotrainfall$dates, "%y/%m/%d"), "%y/%m/%d")$yday+1
      sleepy=cbind(date_waddle,doy,sleepy)
      plotrainfall2=cbind(doy2,plotrainfall)
      #plotgrass=cbind(doy2,plotgrass)
      colnames(plotrainfall2)=c("JULDAY","dates","RAINFALL")
      desic=subset(debout,TIME==24 & substr(dates,1,4)==curyear)
      desic=as.data.frame(cbind(desic[,2],desic[,20]))
      colnames(desic)=c('day','desic')
      
      date_waddle=aggregate(sleepy$date_waddle,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),min)
      doy=aggregate(sleepy$doy,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
      Tb=aggregate(sleepy$Temperature,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),mean)
      steps=aggregate(sleepy$Steps,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),sum)
      year=aggregate(sleepy$Year,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
      month=aggregate(sleepy$Month,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
      day=aggregate(sleepy$Day,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
      hours=aggregate(sleepy$Hours,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
      sleepy=as.data.frame(cbind(as.data.frame(date_waddle[,2]),doy[,2],Tb[,2],steps[,2],year[,2],month[,2],day[,2],hours[,2]))
      colnames(sleepy)=c('date_waddle','doy','Temperature','Steps','Year','Month','Day','Hours')
      
      
      
      # round the times to get rid of occasional 1 min addons
      x=sleepy$date_waddle
      r =  60*60
      
      H = as.integer(format(x, "%H"))
      M = as.integer(format(x, "%M"))
      S = as.integer(format(x, "%S"))
      D = format(x, "%Y-%m-%d")
      secs = 3600*H + 60*M + S
      x=as.POSIXct(round(secs/r)*r, origin=D)-11*3600
      sleepy$date_waddle=x
      # end rounding times
      
      
      sleepy2=as.data.frame(as.data.frame(cbind(date_waddle[,2],doy[,2],Tb[,2],steps[,2],year[,2],month[,2],day[,2],hours[,2],date_waddle$Group.1)))
      colnames(sleepy2)=c('date_waddle','doy','Temperature','Steps','Year','Month','Day','Hours','merge')
      
      
      sleepy2$Temperature=as.factor(sleepy$Temperature)
      
      correl=merge(sleepy2,environ5,by='merge')
      correl_day=subset(correl,ZEN!=90)
      c.doy=as.numeric(levels(correl$doy))[correl$doy]
      c.year=as.numeric(levels(correl$Year))[correl$Year]
      c.month=as.numeric(levels(correl$Month))[correl$Month]
      c.day=as.numeric(levels(correl$Day))[correl$Day]
      c.hour=as.data.frame(correl$TIME)
      c.Tb_obs=as.numeric(levels(correl$Temperature))[correl$Temperature]
      c.steps=as.numeric(levels(correl$Steps))[correl$Steps]
      c.Tb_pred=as.data.frame(correl$TC)
      c.act=as.data.frame(correl$ACT)
      c.shade=as.data.frame(correl$SHADE)
      correl2=cbind(correl$dates,c.doy,c.year,c.month,c.day,c.hour,c.Tb_obs,c.steps,c.Tb_pred,c.act,c.shade)
      colnames(correl2)=c('date','doy','year','month','day','hour','Tb_obs','steps','Tb_pred','act','shade')
      
      c.doy.day=as.numeric(levels(correl_day$doy))[correl_day$doy]
      c.year.day=as.numeric(levels(correl_day$Year))[correl_day$Year]
      c.month.day=as.numeric(levels(correl_day$Month))[correl_day$Month]
      c.day.day=as.numeric(levels(correl_day$Day))[correl_day$Day]
      c.hour.day=as.data.frame(correl_day$TIME)
      c.Tb_obs.day=as.numeric(levels(correl_day$Temperature))[correl_day$Temperature]
      c.steps.day=as.numeric(levels(correl_day$Steps))[correl_day$Steps]
      c.Tb_pred.day=as.data.frame(correl_day$TC)
      c.act.day=as.data.frame(correl_day$ACT)
      c.shade.day=as.data.frame(correl_day$SHADE)
      correl2.day=cbind(correl_day$dates,c.doy.day,c.year.day,c.month.day,c.day.day,c.hour.day,c.Tb_obs.day,c.steps.day,c.Tb_pred.day,c.act.day,c.shade.day)
      colnames(correl2.day)=c('date','doy','year','month','day','hour','Tb_obs','steps','Tb_pred','act','shade')
      
      mean_Tb_obs=mean(correl2$Tb_obs,na.rm=TRUE)
      max_Tb_obs=max(correl2$Tb_obs,na.rm=TRUE)
      min_Tb_obs=min(correl2$Tb_obs,na.rm=TRUE)
      med_Tb_obs=median(correl2$Tb_obs,na.rm=TRUE)
      mean_Tb_pred=mean(correl2$Tb_pred,na.rm=TRUE)
      max_Tb_pred=max(correl2$Tb_pred,na.rm=TRUE)
      min_Tb_pred=min(correl2$Tb_pred,na.rm=TRUE)
      med_Tb_pred=median(correl2$Tb_pred,na.rm=TRUE)
      
      
      lm_Tb=with(correl2,(lm(Tb_pred~Tb_obs)))
      if(k==1){
        #with(correl2,(plot(Tb_pred~Tb_obs,cex=0.1,ylim=c(0,45),xlim=c(0,45),xlab='observed Tb',ylab='predicted Tb')))
        correl_all=correl2
      }else{
        #with(correl2,(points(Tb_pred~Tb_obs,cex=0.1)))
        correl_all=rbind(correl_all,correl2)
      }
    }
  } 
  
  if(yeartodo==2010){
    correl_all_2010=correl_all
    lm_Tb_2010=with(correl_all_2010,(lm(Tb_pred~Tb_obs)))
    r_Tb_2010=with(correl_all_2010,cor(Tb_pred,Tb_obs))
    lm_Tb_R2_2010=summary(lm_Tb)$r.squared
    lm_Tb_rmsd_2010=sqrt(mean(((correl_all_2010$Tb_obs-correl_all_2010$Tb_pred)^2),na.rm=TRUE))
    write.csv(correl_all_2010,'correl_all_2010.csv')
  }else{
    correl_all_2009=correl_all
    lm_Tb_2009=with(correl_all_2009,(lm(Tb_pred~Tb_obs)))
    r_Tb_2009=with(correl_all_2009,cor(Tb_pred,Tb_obs))
    lm_Tb_R2_2009=summary(lm_Tb)$r.squared
    lm_Tb_rmsd_2009=sqrt(mean(((correl_all_2009$Tb_obs-correl_all_2009$Tb_pred)^2),na.rm=TRUE))
    write.csv(correl_all_2009,'correl_all_2009.csv')
  }
} # end loop through two years

# Figure 1b&d

# plot observed and predicted Tb (and Tair) vs time for Sept-Nov
with(correl_all_2009,plot(Tb_pred~Tb_obs,col=addTrans("black",10),pch=16,cex=0.5,ylim=c(5,45),xlim=c(5,45),main="",xlab=expression(paste("Observed Temperature [",degree,"C]")),ylab=expression(paste("Predicted Temperature [",degree,"C]"))))
abline(0,1,col='black')
with(correl_all_2010,plot(Tb_pred~Tb_obs,col=addTrans("black",10),pch=16,cex=0.5,ylim=c(5,45),xlim=c(5,45),main="",xlab=expression(paste("Observed Temperature [",degree,"C]")),ylab=expression(paste("Predicted Temperature [",degree,"C]"))))
abline(0,1,col='black')

# Figure 1a&c
yearstodo=c(2009,2010)
for(m in 1:2){
  yeartodo=yearstodo[m]
  k=0
  # use individual 9 (burrows) or 115 (dam)
  for(i in 1:121){
    
    sleepy_id=waddlefiles[i,2]
    sexliz=subset(sex,Liz==sleepy_id)
    sexliz=sexliz[2]
    sexlizard=as.character(sexliz)
    if(sexlizard=="integer(0)"){
      sexlizard='unknown'
    }
    if(sexlizard==2){
      sexlizard='F'
    }
    if(sexlizard==3){
      sexlizard='M'
    }
    
    sleepy=as.data.frame(read.table(file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',waddlefiles[i,3],sep=""), sep = ",", head=TRUE))
    if(is.null(sleepy$Hours)){
      Hours=as.numeric(substr(sleepy$Time,1,2))
      Minutes=as.numeric(substr(sleepy$Time,4,5))
      sleepy=as.data.frame(cbind(sleepy,Hours,Minutes))
      write.table(sleepy,file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',waddlefiles[i,3],sep=""),row.names=F,sep=",") 
    }
    curyear=max(sleepy$Year,na.rm=TRUE)
    if(curyear==yeartodo){
      
      plotrainfall = subset(rainfall,substr(dates,1,4)==curyear)
      environ_bask = subset(environ5,  subset=(ACT>=1 & TC>=TBASK & substr(dates,1,4)==curyear))
      environ_forage = subset(environ5,  subset=(ACT>1 & substr(dates,1,4)==curyear))
      environ_night= subset(metout,  subset=(ZEN==90))
      environ_night$TIME=environ_night$TIME/60-1
      date_waddle1=with(sleepy,ISOdatetime(Year,Month,Day,Hours,Minutes,0))
      date_waddle=date_waddle1+60*60 # adjust for Central Time
      doy=strptime(format(date_waddle, "%y/%m/%d"), "%y/%m/%d")$yday+1
      doy2=strptime(format(plotrainfall$dates, "%y/%m/%d"), "%y/%m/%d")$yday+1
      sleepy=cbind(date_waddle,doy,sleepy)
      plotrainfall2=cbind(doy2,plotrainfall)
      colnames(plotrainfall2)=c("JULDAY","dates","RAINFALL")
      desic=subset(debout,TIME==24 & substr(dates,1,4)==curyear)
      desic=as.data.frame(cbind(desic[,2],desic[,20]))
      colnames(desic)=c('day','desic')
      
      date_waddle=aggregate(sleepy$date_waddle,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),min)
      doy=aggregate(sleepy$doy,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
      Tb=aggregate(sleepy$Temperature,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),mean)
      steps=aggregate(sleepy$Steps,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),sum)
      year=aggregate(sleepy$Year,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
      month=aggregate(sleepy$Month,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
      day=aggregate(sleepy$Day,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
      hours=aggregate(sleepy$Hours,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
      sleepy=as.data.frame(cbind(as.data.frame(date_waddle[,2]),doy[,2],Tb[,2],steps[,2],year[,2],month[,2],day[,2],hours[,2]))
      colnames(sleepy)=c('date_waddle','doy','Temperature','Steps','Year','Month','Day','Hours')
      
      # round the times to get rid of occasional 1 min addons
      x=sleepy$date_waddle
      r =  60*60
      H = as.integer(format(x, "%H"))
      M = as.integer(format(x, "%M"))
      S = as.integer(format(x, "%S"))
      D = format(x, "%Y-%m-%d")
      secs = 3600*H + 60*M + S
      x=as.POSIXct(round(secs/r)*r, origin=D)-11*3600
      sleepy$date_waddle=x
      # end rounding times
      
      sleepy2=as.data.frame(as.data.frame(cbind(date_waddle[,2],doy[,2],Tb[,2],steps[,2],year[,2],month[,2],day[,2],hours[,2],date_waddle$Group.1)))
      colnames(sleepy2)=c('date_waddle','doy','Temperature','Steps','Year','Month','Day','Hours','merge')
      
      sleepy3=subset(sleepy,sleepy$Month<11)
      if(k==0){
        if(curyear==2009){
          with(sleepy3,plot(Temperature~date_waddle,col='red',cex=0.2,ylim=c(0,45),xlim=c(as.numeric(as.POSIXct("2009-09-15",origin="1970-01-01")),as.numeric(as.POSIXct("2009-10-15",origin="1970-01-01"))),ylab=c('body temperature (deg C)'),xaxt = "n",xlab=""))
        }else{
          with(sleepy3,plot(Temperature~date_waddle,col='red',cex=0.2,ylim=c(0,45),xlim=c(as.numeric(as.POSIXct("2010-09-15",origin="1970-01-01")),as.numeric(as.POSIXct("2010-10-15",origin="1970-01-01"))),ylab=c('body temperature (deg C)'),xaxt = "n",xlab=""))
        }
      }else{
        with(sleepy3,points(Temperature~date_waddle,col='red',cex=0.2,ylim=c(0,50)))
      }
      
      k=k+1
    }
  } #end loop through lizards
  k=0
  #points(metout$TAREF~environ$dates,cex=0.2,pch=16,col='grey',type='l',lty=2)
  points(environ5$TC~environ5$dates,cex=0.5,pch=16,type='b')
  axis.POSIXct(side = 1, x = environ5$dates,
    at = seq(from = round(environ5$dates[1], "days"),
      to = environ5$dates[1] + ceiling(difftime(tail(environ5$dates, 1), head(environ5$dates, 1), 
        units = "days")),
      by = "1 day"),
    las = 2)
}
##########################################################################################


################## plot home ranges, Figs. 5 & S3 ######################################

######################### Tbs specific period and individual, average per hour ######################################
getEdges = function(x) {
  stopifnot(class(x) == "SpatialPolygons")
  lapply(x@polygons, function(y) {
    y@Polygons[[1]]@coords
  })
}
#getEdges(cp)

library(adehabitatHR)
library(rgdal)
########## active lizards ###############


activelizards=c(11505,11885,12434,12847,40012,40044,9310,9372)

lizards=subset(waddlefiles,year==2009 & (id%in%activelizards))

lizards=lizards$id
k=0
for(i in 1:length(lizards)){
  #i=9# 32 or 9 
  
  #sleepy_id=waddlefiles[i,2]
  sleepy_id=lizards[i]
  sexliz=subset(sex,Liz==sleepy_id)
  sexliz=sexliz[2]
  sexlizard=as.character(sexliz)
  if(sexlizard=="u" | sexlizard=="character(0)"){
    sexlizard='unknown'
  }
  if(sexlizard==2){
    sexlizard='F'
  }
  if(sexlizard==3){
    sexlizard='M'
  }
  
  
  sleepy=as.data.frame(read.table(file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',sleepy_id,'_2009_ALL.csv',sep=""), sep = ",", head=TRUE))
  #sleepy=as.data.frame(read.table(file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',waddlefiles[i,3],sep=""), sep = ",", head=TRUE))
  sleepy=subset(sleepy,Month==11 & Day<=20 & Day>=11) #  & Day<28 & Day>20 week either side of 21th Nov, when little drought broke
  sleepy=subset(sleepy,Month==11) #  & Day<28 & Day>20 week either side of 21th Nov, when little drought broke
  if(nrow(sleepy)>0){
    # prepare UTM coordinates matrix
    UTMzone=(floor((longlat[1] + 180)/6) %% 60)
    xy=na.omit(as.data.frame(cbind(sleepy$Easting,sleepy$Northing)))
    if(nrow(xy)>4){
      k=k+1
      xy=xy[xy[,1]>0,]
      if(nrow(xy)>4){
        utmcoor=SpatialPoints(xy, proj4string=CRS(paste("+proj=utm +south +zone=",54,sep="")))
        #utmdata$X and utmdata$Y are corresponding to UTM Easting and Northing, respectively.
        #zone= UTM zone
        # converting
        longlats=spTransform(utmcoor,CRS("+proj=longlat"))
        cp = mcp(longlats, percent=95)
        cp=SpatialPolygons(cp@polygons)
        cp=as.data.frame(getEdges(cp))
        colnames(cp)=c("x","y")
        longlatcoor=as.data.frame(longlats)
        #plot(googlemap+ geom_point(data=longlatcoor, aes(x=x, y=y),colour = i, size = 0.5))
        # plot(googlemap + geom_polygon(data = cp, aes(x = x, y = y),color=i,fill="NA"))
        
        if(k==1){
          allcoords=cbind(i,sleepy_id,cp)
        }else{
          allcoords=rbind(allcoords,cbind(i,sleepy_id,cp))
        }
      }
    }
  }
  
}
colnames(allcoords)=c('id','value','x','y')
library(ggmap)
#ggplot(data=allcoords,aes(x = x, y = y, group = id))

googlemap = ggmap(get_map(location = c(lon = longlat[1], lat = longlat[2]), zoom = 16, maptype = 'satellite',filename = "ggmapTemp.png"))
#plot(googlemap+ geom_point(data=allcoords, aes(x=x, y=y),colour = factor(allcoords$i), size = 0.5))
plot(googlemap+ geom_polygon(data = subset(allcoords,id==1), aes(x = x, y = y),color=1,fill=addTrans(1,60))
  + geom_polygon(data = subset(allcoords,id==2), aes(x = x, y = y),color=2,fill=addTrans(2,60))
  + geom_polygon(data = subset(allcoords,id==3), aes(x = x, y = y),color=3,fill=addTrans(3,60))
  + geom_polygon(data = subset(allcoords,id==4), aes(x = x, y = y),color=4,fill=addTrans(4,60))
  + geom_polygon(data = subset(allcoords,id==5), aes(x = x, y = y),color=5,fill=addTrans(5,60))
  + geom_polygon(data = subset(allcoords,id==6), aes(x = x, y = y),color=6,fill=addTrans(6,60))
  + geom_polygon(data = subset(allcoords,id==7), aes(x = x, y = y),color=7,fill=addTrans(7,60))
  + geom_polygon(data = subset(allcoords,id==8), aes(x = x, y = y),color=8,fill=addTrans(8,60))
)


########## inactive lizards ###############

lizards=subset(waddlefiles,year==2009 & !(id%in%activelizards))

lizards=lizards$id
k=0
for(i in 1:length(lizards)){
  #i=9# 32 or 9 
  
  #sleepy_id=waddlefiles[i,2]
  sleepy_id=lizards[i]
  sexliz=subset(sex,Liz==sleepy_id)
  sexliz=sexliz[2]
  sexlizard=as.character(sexliz)
  if(sexlizard=="u" | sexlizard=="character(0)"){
    sexlizard='unknown'
  }
  if(sexlizard==2){
    sexlizard='F'
  }
  if(sexlizard==3){
    sexlizard='M'
  }
  
  
  sleepy=as.data.frame(read.table(file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',sleepy_id,'_2009_ALL.csv',sep=""), sep = ",", head=TRUE))
  #sleepy=as.data.frame(read.table(file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',waddlefiles[i,3],sep=""), sep = ",", head=TRUE))
  #sleepy=subset(sleepy,Month==11 & Day<=20 & Day>=11) #  & Day<28 & Day>20 week either side of 21th Nov, when little drought broke
  sleepy=subset(sleepy,Month==11) #  & Day<28 & Day>20 week either side of 21th Nov, when little drought broke
  if(nrow(sleepy)>0){
    # prepare UTM coordinates matrix
    UTMzone=(floor((longlat[1] + 180)/6) %% 60)
    xy=na.omit(as.data.frame(cbind(sleepy$Easting,sleepy$Northing)))
    if(nrow(xy)>4){
      k=k+1
      xy=xy[xy[,1]>0,]
      if(nrow(xy)>4){
        utmcoor=SpatialPoints(xy, proj4string=CRS(paste("+proj=utm +south +zone=",54,sep="")))
        #utmdata$X and utmdata$Y are corresponding to UTM Easting and Northing, respectively.
        #zone= UTM zone
        # converting
        longlats=spTransform(utmcoor,CRS("+proj=longlat"))
        cp = mcp(longlats, percent=95)
        cp=SpatialPolygons(cp@polygons)
        cp=as.data.frame(getEdges(cp))
        colnames(cp)=c("x","y")
        longlatcoor=as.data.frame(longlats)
        #plot(googlemap+ geom_point(data=longlatcoor, aes(x=x, y=y),colour = i, size = 0.5))
        # plot(googlemap + geom_polygon(data = cp, aes(x = x, y = y),color=i,fill="NA"))
        
        if(k==1){
          allcoords=cbind(i,sleepy_id,cp)
        }else{
          allcoords=rbind(allcoords,cbind(i,sleepy_id,cp))
        }
      }
    }
  }
  
}
colnames(allcoords)=c('id','value','x','y')

plot(googlemap+ geom_polygon(data = subset(allcoords,id==1), aes(x = x, y = y),color=1,fill=addTrans(1,60))
  + geom_polygon(data = subset(allcoords,id==2), aes(x = x, y = y),color=2,fill=addTrans(2,60))
  + geom_polygon(data = subset(allcoords,id==3), aes(x = x, y = y),color=3,fill=addTrans(3,60))
  + geom_polygon(data = subset(allcoords,id==4), aes(x = x, y = y),color=4,fill=addTrans(4,60))
  + geom_polygon(data = subset(allcoords,id==5), aes(x = x, y = y),color=5,fill=addTrans(5,60))
  + geom_polygon(data = subset(allcoords,id==6), aes(x = x, y = y),color=6,fill=addTrans(6,60))
  + geom_polygon(data = subset(allcoords,id==7), aes(x = x, y = y),color=7,fill=addTrans(7,60))
  + geom_polygon(data = subset(allcoords,id==8), aes(x = x, y = y),color=8,fill=addTrans(8,60))
  + geom_polygon(data = subset(allcoords,id==9), aes(x = x, y = y),color=9,fill=addTrans(9,60))
  + geom_polygon(data = subset(allcoords,id==10), aes(x = x, y = y),color=10,fill=addTrans(10,60))
  + geom_polygon(data = subset(allcoords,id==11), aes(x = x, y = y),color=11,fill=addTrans(11,60))
  + geom_polygon(data = subset(allcoords,id==12), aes(x = x, y = y),color=12,fill=addTrans(12,60))
  + geom_polygon(data = subset(allcoords,id==13), aes(x = x, y = y),color=13,fill=addTrans(13,60))
  + geom_polygon(data = subset(allcoords,id==14), aes(x = x, y = y),color=14,fill=addTrans(14,60))
  + geom_polygon(data = subset(allcoords,id==15), aes(x = x, y = y),color=15,fill=addTrans(15,60))
  + geom_polygon(data = subset(allcoords,id==16), aes(x = x, y = y),color=16,fill=addTrans(16,60))
  + geom_polygon(data = subset(allcoords,id==17), aes(x = x, y = y),color=17,fill=addTrans(17,60))
  + geom_polygon(data = subset(allcoords,id==18), aes(x = x, y = y),color=18,fill=addTrans(18,60))
  + geom_polygon(data = subset(allcoords,id==19), aes(x = x, y = y),color=19,fill=addTrans(19,60))
  + geom_polygon(data = subset(allcoords,id==20), aes(x = x, y = y),color=20,fill=addTrans(20,60))
  + geom_polygon(data = subset(allcoords,id==21), aes(x = x, y = y),color=21,fill=addTrans(21,60))
  + geom_polygon(data = subset(allcoords,id==22), aes(x = x, y = y),color=22,fill=addTrans(22,60))
  + geom_polygon(data = subset(allcoords,id==23), aes(x = x, y = y),color=23,fill=addTrans(23,60))
  + geom_polygon(data = subset(allcoords,id==24), aes(x = x, y = y),color=24,fill=addTrans(24,60))
  + geom_polygon(data = subset(allcoords,id==25), aes(x = x, y = y),color=25,fill=addTrans(25,60))
  + geom_polygon(data = subset(allcoords,id==26), aes(x = x, y = y),color=26,fill=addTrans(26,60))
  + geom_polygon(data = subset(allcoords,id==27), aes(x = x, y = y),color=27,fill=addTrans(27,60))
  + geom_polygon(data = subset(allcoords,id==28), aes(x = x, y = y),color=28,fill=addTrans(28,60))
  + geom_polygon(data = subset(allcoords,id==29), aes(x = x, y = y),color=29,fill=addTrans(29,60))
  + geom_polygon(data = subset(allcoords,id==30), aes(x = x, y = y),color=30,fill=addTrans(30,60))
  + geom_polygon(data = subset(allcoords,id==31), aes(x = x, y = y),color=31,fill=addTrans(31,60))
  + geom_polygon(data = subset(allcoords,id==32), aes(x = x, y = y),color=32,fill=addTrans(32,60))
  + geom_polygon(data = subset(allcoords,id==33), aes(x = x, y = y),color=33,fill=addTrans(33,60))
  + geom_polygon(data = subset(allcoords,id==34), aes(x = x, y = y),color=34,fill=addTrans(34,60))
  + geom_polygon(data = subset(allcoords,id==35), aes(x = x, y = y),color=35,fill=addTrans(35,60))
  + geom_polygon(data = subset(allcoords,id==36), aes(x = x, y = y),color=36,fill=addTrans(36,60))
  + geom_polygon(data = subset(allcoords,id==37), aes(x = x, y = y),color=37,fill=addTrans(37,60))
  + geom_polygon(data = subset(allcoords,id==38), aes(x = x, y = y),color=38,fill=addTrans(38,60))
  + geom_polygon(data = subset(allcoords,id==39), aes(x = x, y = y),color=39,fill=addTrans(39,60))
  + geom_polygon(data = subset(allcoords,id==40), aes(x = x, y = y),color=40,fill=addTrans(40,60))
  + geom_polygon(data = subset(allcoords,id==41), aes(x = x, y = y),color=41,fill=addTrans(41,60))
  + geom_polygon(data = subset(allcoords,id==42), aes(x = x, y = y),color=42,fill=addTrans(42,60))
  + geom_polygon(data = subset(allcoords,id==43), aes(x = x, y = y),color=43,fill=addTrans(43,60))
  + geom_polygon(data = subset(allcoords,id==44), aes(x = x, y = y),color=44,fill=addTrans(44,60))
  + geom_polygon(data = subset(allcoords,id==45), aes(x = x, y = y),color=45,fill=addTrans(45,60))
  + geom_polygon(data = subset(allcoords,id==46), aes(x = x, y = y),color=46,fill=addTrans(46,60))
  + geom_polygon(data = subset(allcoords,id==47), aes(x = x, y = y),color=47,fill=addTrans(47,60))
  + geom_polygon(data = subset(allcoords,id==48), aes(x = x, y = y),color=48,fill=addTrans(48,60))
  + geom_polygon(data = subset(allcoords,id==49), aes(x = x, y = y),color=49,fill=addTrans(49,60))
  + geom_polygon(data = subset(allcoords,id==50), aes(x = x, y = y),color=50,fill=addTrans(50,60))
  + geom_polygon(data = subset(allcoords,id==51), aes(x = x, y = y),color=51,fill=addTrans(51,60))
  + geom_polygon(data = subset(allcoords,id==52), aes(x = x, y = y),color=52,fill=addTrans(52,60))
  + geom_polygon(data = subset(allcoords,id==53), aes(x = x, y = y),color=53,fill=addTrans(53,60))
)


### week after rain ####

lizards=subset(waddlefiles,year==2009 )

lizards=lizards$id
k=0
for(i in 1:length(lizards)){
  #i=9# 32 or 9 
  
  #sleepy_id=waddlefiles[i,2]
  sleepy_id=lizards[i]
  sexliz=subset(sex,Liz==sleepy_id)
  sexliz=sexliz[2]
  sexlizard=as.character(sexliz)
  if(sexlizard=="u" | sexlizard=="character(0)"){
    sexlizard='unknown'
  }
  if(sexlizard==2){
    sexlizard='F'
  }
  if(sexlizard==3){
    sexlizard='M'
  }
  
  
  sleepy=as.data.frame(read.table(file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',sleepy_id,'_2009_ALL.csv',sep=""), sep = ",", head=TRUE))
  #sleepy=as.data.frame(read.table(file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',waddlefiles[i,3],sep=""), sep = ",", head=TRUE))
  #sleepy=subset(sleepy,Month==11 & Day<=20 & Day>=11) #  & Day<28 & Day>20 week either side of 21th Nov, when little drought broke
  sleepy=subset(sleepy,Month==11 & Year==2009 & Day<28 & Day>20) #  & Day<28 & Day>20 week either side of 21th Nov, when little drought broke
  if(nrow(sleepy)>0){
    # prepare UTM coordinates matrix
    UTMzone=(floor((longlat[1] + 180)/6) %% 60)
    xy=na.omit(as.data.frame(cbind(sleepy$Easting,sleepy$Northing)))
    if(nrow(xy)>4){
      k=k+1
      xy=xy[xy[,1]>0,]
      if(nrow(xy)>4){
        utmcoor=SpatialPoints(xy, proj4string=CRS(paste("+proj=utm +south +zone=",54,sep="")))
        #utmdata$X and utmdata$Y are corresponding to UTM Easting and Northing, respectively.
        #zone= UTM zone
        # converting
        longlats=spTransform(utmcoor,CRS("+proj=longlat"))
        cp = mcp(longlats, percent=95)
        cp=SpatialPolygons(cp@polygons)
        cp=as.data.frame(getEdges(cp))
        colnames(cp)=c("x","y")
        longlatcoor=as.data.frame(longlats)
        #plot(googlemap+ geom_point(data=longlatcoor, aes(x=x, y=y),colour = i, size = 0.5))
        # plot(googlemap + geom_polygon(data = cp, aes(x = x, y = y),color=i,fill="NA"))
        
        if(k==1){
          allcoords=cbind(i,sleepy_id,cp)
        }else{
          allcoords=rbind(allcoords,cbind(i,sleepy_id,cp))
        }
      }
    }
  }
  
}
colnames(allcoords)=c('id','value','x','y')

plot(googlemap+ geom_polygon(data = subset(allcoords,id==1), aes(x = x, y = y),color=1,fill=addTrans(1,60))
  + geom_polygon(data = subset(allcoords,id==2), aes(x = x, y = y),color=2,fill=addTrans(2,60))
  + geom_polygon(data = subset(allcoords,id==3), aes(x = x, y = y),color=3,fill=addTrans(3,60))
  + geom_polygon(data = subset(allcoords,id==4), aes(x = x, y = y),color=4,fill=addTrans(4,60))
  + geom_polygon(data = subset(allcoords,id==5), aes(x = x, y = y),color=5,fill=addTrans(5,60))
  + geom_polygon(data = subset(allcoords,id==6), aes(x = x, y = y),color=6,fill=addTrans(6,60))
  + geom_polygon(data = subset(allcoords,id==7), aes(x = x, y = y),color=7,fill=addTrans(7,60))
  + geom_polygon(data = subset(allcoords,id==8), aes(x = x, y = y),color=8,fill=addTrans(8,60))
  + geom_polygon(data = subset(allcoords,id==9), aes(x = x, y = y),color=9,fill=addTrans(9,60))
  + geom_polygon(data = subset(allcoords,id==10), aes(x = x, y = y),color=10,fill=addTrans(10,60))
  + geom_polygon(data = subset(allcoords,id==11), aes(x = x, y = y),color=11,fill=addTrans(11,60))
  + geom_polygon(data = subset(allcoords,id==12), aes(x = x, y = y),color=12,fill=addTrans(12,60))
  + geom_polygon(data = subset(allcoords,id==13), aes(x = x, y = y),color=13,fill=addTrans(13,60))
  + geom_polygon(data = subset(allcoords,id==14), aes(x = x, y = y),color=14,fill=addTrans(14,60))
  + geom_polygon(data = subset(allcoords,id==15), aes(x = x, y = y),color=15,fill=addTrans(15,60))
  + geom_polygon(data = subset(allcoords,id==16), aes(x = x, y = y),color=16,fill=addTrans(16,60))
  + geom_polygon(data = subset(allcoords,id==17), aes(x = x, y = y),color=17,fill=addTrans(17,60))
  + geom_polygon(data = subset(allcoords,id==18), aes(x = x, y = y),color=18,fill=addTrans(18,60))
  + geom_polygon(data = subset(allcoords,id==19), aes(x = x, y = y),color=19,fill=addTrans(19,60))
  + geom_polygon(data = subset(allcoords,id==20), aes(x = x, y = y),color=20,fill=addTrans(20,60))
  + geom_polygon(data = subset(allcoords,id==21), aes(x = x, y = y),color=21,fill=addTrans(21,60))
  + geom_polygon(data = subset(allcoords,id==22), aes(x = x, y = y),color=22,fill=addTrans(22,60))
  + geom_polygon(data = subset(allcoords,id==23), aes(x = x, y = y),color=23,fill=addTrans(23,60))
  + geom_polygon(data = subset(allcoords,id==24), aes(x = x, y = y),color=24,fill=addTrans(24,60))
  + geom_polygon(data = subset(allcoords,id==25), aes(x = x, y = y),color=25,fill=addTrans(25,60))
  + geom_polygon(data = subset(allcoords,id==26), aes(x = x, y = y),color=26,fill=addTrans(26,60))
  + geom_polygon(data = subset(allcoords,id==27), aes(x = x, y = y),color=27,fill=addTrans(27,60))
  + geom_polygon(data = subset(allcoords,id==28), aes(x = x, y = y),color=28,fill=addTrans(28,60))
  + geom_polygon(data = subset(allcoords,id==29), aes(x = x, y = y),color=29,fill=addTrans(29,60))
  + geom_polygon(data = subset(allcoords,id==30), aes(x = x, y = y),color=30,fill=addTrans(30,60))
  + geom_polygon(data = subset(allcoords,id==31), aes(x = x, y = y),color=31,fill=addTrans(31,60))
  + geom_polygon(data = subset(allcoords,id==32), aes(x = x, y = y),color=32,fill=addTrans(32,60))
  + geom_polygon(data = subset(allcoords,id==33), aes(x = x, y = y),color=33,fill=addTrans(33,60))
  + geom_polygon(data = subset(allcoords,id==34), aes(x = x, y = y),color=34,fill=addTrans(34,60))
  + geom_polygon(data = subset(allcoords,id==35), aes(x = x, y = y),color=35,fill=addTrans(35,60))
  + geom_polygon(data = subset(allcoords,id==36), aes(x = x, y = y),color=36,fill=addTrans(36,60))
  + geom_polygon(data = subset(allcoords,id==37), aes(x = x, y = y),color=37,fill=addTrans(37,60))
  + geom_polygon(data = subset(allcoords,id==38), aes(x = x, y = y),color=38,fill=addTrans(38,60))
  + geom_polygon(data = subset(allcoords,id==39), aes(x = x, y = y),color=39,fill=addTrans(39,60))
  + geom_polygon(data = subset(allcoords,id==40), aes(x = x, y = y),color=40,fill=addTrans(40,60))
  + geom_polygon(data = subset(allcoords,id==41), aes(x = x, y = y),color=41,fill=addTrans(41,60))
  + geom_polygon(data = subset(allcoords,id==42), aes(x = x, y = y),color=42,fill=addTrans(42,60))
  + geom_polygon(data = subset(allcoords,id==43), aes(x = x, y = y),color=43,fill=addTrans(43,60))
  + geom_polygon(data = subset(allcoords,id==44), aes(x = x, y = y),color=44,fill=addTrans(44,60))
  + geom_polygon(data = subset(allcoords,id==45), aes(x = x, y = y),color=45,fill=addTrans(45,60))
  + geom_polygon(data = subset(allcoords,id==46), aes(x = x, y = y),color=46,fill=addTrans(46,60))
  + geom_polygon(data = subset(allcoords,id==47), aes(x = x, y = y),color=47,fill=addTrans(47,60))
  + geom_polygon(data = subset(allcoords,id==48), aes(x = x, y = y),color=48,fill=addTrans(48,60))
  + geom_polygon(data = subset(allcoords,id==49), aes(x = x, y = y),color=49,fill=addTrans(49,60))
  + geom_polygon(data = subset(allcoords,id==50), aes(x = x, y = y),color=50,fill=addTrans(50,60))
  + geom_polygon(data = subset(allcoords,id==51), aes(x = x, y = y),color=51,fill=addTrans(51,60))
  + geom_polygon(data = subset(allcoords,id==52), aes(x = x, y = y),color=52,fill=addTrans(52,60))
  + geom_polygon(data = subset(allcoords,id==53), aes(x = x, y = y),color=53,fill=addTrans(53,60))
  + geom_polygon(data = subset(allcoords,id==54), aes(x = x, y = y),color=54,fill=addTrans(54,60))
  + geom_polygon(data = subset(allcoords,id==55), aes(x = x, y = y),color=55,fill=addTrans(55,60))
  + geom_polygon(data = subset(allcoords,id==56), aes(x = x, y = y),color=56,fill=addTrans(56,60))
  + geom_polygon(data = subset(allcoords,id==57), aes(x = x, y = y),color=57,fill=addTrans(57,60))
  + geom_polygon(data = subset(allcoords,id==58), aes(x = x, y = y),color=58,fill=addTrans(58,60))
  + geom_polygon(data = subset(allcoords,id==59), aes(x = x, y = y),color=59,fill=addTrans(59,60))
  + geom_polygon(data = subset(allcoords,id==60), aes(x = x, y = y),color=60,fill=addTrans(60,60))
  + geom_polygon(data = subset(allcoords,id==61), aes(x = x, y = y),color=61,fill=addTrans(61,60))
)


### week before rain ####

lizards=subset(waddlefiles,year==2009 )

lizards=lizards$id
k=0
for(i in 1:length(lizards)){
  #i=9# 32 or 9 
  
  #sleepy_id=waddlefiles[i,2]
  sleepy_id=lizards[i]
  sexliz=subset(sex,Liz==sleepy_id)
  sexliz=sexliz[2]
  sexlizard=as.character(sexliz)
  if(sexlizard=="u" | sexlizard=="character(0)"){
    sexlizard='unknown'
  }
  if(sexlizard==2){
    sexlizard='F'
  }
  if(sexlizard==3){
    sexlizard='M'
  }
  
  
  sleepy=as.data.frame(read.table(file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',sleepy_id,'_2009_ALL.csv',sep=""), sep = ",", head=TRUE))
  #sleepy=as.data.frame(read.table(file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',waddlefiles[i,3],sep=""), sep = ",", head=TRUE))
  #sleepy=subset(sleepy,Month==11 & Day<=20 & Day>=11) #  & Day<28 & Day>20 week either side of 21th Nov, when little drought broke
  sleepy=subset(sleepy,Month==11 & Year==2009 & Day<=19 & Day>10) #  & Day<28 & Day>20 week either side of 21th Nov, when little drought broke
  if(nrow(sleepy)>0){
    # prepare UTM coordinates matrix
    UTMzone=(floor((longlat[1] + 180)/6) %% 60)
    xy=na.omit(as.data.frame(cbind(sleepy$Easting,sleepy$Northing)))
    if(nrow(xy)>4){
      k=k+1
      xy=xy[xy[,1]>0,]
      if(nrow(xy)>4){
        utmcoor=SpatialPoints(xy, proj4string=CRS(paste("+proj=utm +south +zone=",54,sep="")))
        #utmdata$X and utmdata$Y are corresponding to UTM Easting and Northing, respectively.
        #zone= UTM zone
        # converting
        longlats=spTransform(utmcoor,CRS("+proj=longlat"))
        cp = mcp(longlats, percent=95)
        cp=SpatialPolygons(cp@polygons)
        cp=as.data.frame(getEdges(cp))
        colnames(cp)=c("x","y")
        longlatcoor=as.data.frame(longlats)
        #plot(googlemap+ geom_point(data=longlatcoor, aes(x=x, y=y),colour = i, size = 0.5))
        # plot(googlemap + geom_polygon(data = cp, aes(x = x, y = y),color=i,fill="NA"))
        
        if(k==1){
          allcoords=cbind(i,sleepy_id,cp)
        }else{
          allcoords=rbind(allcoords,cbind(i,sleepy_id,cp))
        }
      }
    }
  }
  
}
colnames(allcoords)=c('id','value','x','y')

plot(googlemap+ geom_polygon(data = subset(allcoords,id==1), aes(x = x, y = y),color=1,fill=addTrans(1,60))
  + geom_polygon(data = subset(allcoords,id==2), aes(x = x, y = y),color=2,fill=addTrans(2,60))
  + geom_polygon(data = subset(allcoords,id==3), aes(x = x, y = y),color=3,fill=addTrans(3,60))
  + geom_polygon(data = subset(allcoords,id==4), aes(x = x, y = y),color=4,fill=addTrans(4,60))
  + geom_polygon(data = subset(allcoords,id==5), aes(x = x, y = y),color=5,fill=addTrans(5,60))
  + geom_polygon(data = subset(allcoords,id==6), aes(x = x, y = y),color=6,fill=addTrans(6,60))
  + geom_polygon(data = subset(allcoords,id==7), aes(x = x, y = y),color=7,fill=addTrans(7,60))
  + geom_polygon(data = subset(allcoords,id==8), aes(x = x, y = y),color=8,fill=addTrans(8,60))
  + geom_polygon(data = subset(allcoords,id==9), aes(x = x, y = y),color=9,fill=addTrans(9,60))
  + geom_polygon(data = subset(allcoords,id==10), aes(x = x, y = y),color=10,fill=addTrans(10,60))
  + geom_polygon(data = subset(allcoords,id==11), aes(x = x, y = y),color=11,fill=addTrans(11,60))
  + geom_polygon(data = subset(allcoords,id==12), aes(x = x, y = y),color=12,fill=addTrans(12,60))
  + geom_polygon(data = subset(allcoords,id==13), aes(x = x, y = y),color=13,fill=addTrans(13,60))
  + geom_polygon(data = subset(allcoords,id==14), aes(x = x, y = y),color=14,fill=addTrans(14,60))
  + geom_polygon(data = subset(allcoords,id==15), aes(x = x, y = y),color=15,fill=addTrans(15,60))
  + geom_polygon(data = subset(allcoords,id==16), aes(x = x, y = y),color=16,fill=addTrans(16,60))
  + geom_polygon(data = subset(allcoords,id==17), aes(x = x, y = y),color=17,fill=addTrans(17,60))
  + geom_polygon(data = subset(allcoords,id==18), aes(x = x, y = y),color=18,fill=addTrans(18,60))
  + geom_polygon(data = subset(allcoords,id==19), aes(x = x, y = y),color=19,fill=addTrans(19,60))
  + geom_polygon(data = subset(allcoords,id==20), aes(x = x, y = y),color=20,fill=addTrans(20,60))
  + geom_polygon(data = subset(allcoords,id==21), aes(x = x, y = y),color=21,fill=addTrans(21,60))
  + geom_polygon(data = subset(allcoords,id==22), aes(x = x, y = y),color=22,fill=addTrans(22,60))
  + geom_polygon(data = subset(allcoords,id==23), aes(x = x, y = y),color=23,fill=addTrans(23,60))
  + geom_polygon(data = subset(allcoords,id==24), aes(x = x, y = y),color=24,fill=addTrans(24,60))
  + geom_polygon(data = subset(allcoords,id==25), aes(x = x, y = y),color=25,fill=addTrans(25,60))
  + geom_polygon(data = subset(allcoords,id==26), aes(x = x, y = y),color=26,fill=addTrans(26,60))
  + geom_polygon(data = subset(allcoords,id==27), aes(x = x, y = y),color=27,fill=addTrans(27,60))
  + geom_polygon(data = subset(allcoords,id==28), aes(x = x, y = y),color=28,fill=addTrans(28,60))
  + geom_polygon(data = subset(allcoords,id==29), aes(x = x, y = y),color=29,fill=addTrans(29,60))
  + geom_polygon(data = subset(allcoords,id==30), aes(x = x, y = y),color=30,fill=addTrans(30,60))
  + geom_polygon(data = subset(allcoords,id==31), aes(x = x, y = y),color=31,fill=addTrans(31,60))
  + geom_polygon(data = subset(allcoords,id==32), aes(x = x, y = y),color=32,fill=addTrans(32,60))
  + geom_polygon(data = subset(allcoords,id==33), aes(x = x, y = y),color=33,fill=addTrans(33,60))
  + geom_polygon(data = subset(allcoords,id==34), aes(x = x, y = y),color=34,fill=addTrans(34,60))
  + geom_polygon(data = subset(allcoords,id==35), aes(x = x, y = y),color=35,fill=addTrans(35,60))
  + geom_polygon(data = subset(allcoords,id==36), aes(x = x, y = y),color=36,fill=addTrans(36,60))
  + geom_polygon(data = subset(allcoords,id==37), aes(x = x, y = y),color=37,fill=addTrans(37,60))
  + geom_polygon(data = subset(allcoords,id==38), aes(x = x, y = y),color=38,fill=addTrans(38,60))
  + geom_polygon(data = subset(allcoords,id==39), aes(x = x, y = y),color=39,fill=addTrans(39,60))
  + geom_polygon(data = subset(allcoords,id==40), aes(x = x, y = y),color=40,fill=addTrans(40,60))
  + geom_polygon(data = subset(allcoords,id==41), aes(x = x, y = y),color=41,fill=addTrans(41,60))
  + geom_polygon(data = subset(allcoords,id==42), aes(x = x, y = y),color=42,fill=addTrans(42,60))
  + geom_polygon(data = subset(allcoords,id==43), aes(x = x, y = y),color=43,fill=addTrans(43,60))
  + geom_polygon(data = subset(allcoords,id==44), aes(x = x, y = y),color=44,fill=addTrans(44,60))
  + geom_polygon(data = subset(allcoords,id==45), aes(x = x, y = y),color=45,fill=addTrans(45,60))
  + geom_polygon(data = subset(allcoords,id==46), aes(x = x, y = y),color=46,fill=addTrans(46,60))
  + geom_polygon(data = subset(allcoords,id==47), aes(x = x, y = y),color=47,fill=addTrans(47,60))
  + geom_polygon(data = subset(allcoords,id==48), aes(x = x, y = y),color=48,fill=addTrans(48,60))
  + geom_polygon(data = subset(allcoords,id==49), aes(x = x, y = y),color=49,fill=addTrans(49,60))
  + geom_polygon(data = subset(allcoords,id==50), aes(x = x, y = y),color=50,fill=addTrans(50,60))
  + geom_polygon(data = subset(allcoords,id==51), aes(x = x, y = y),color=51,fill=addTrans(51,60))
  + geom_polygon(data = subset(allcoords,id==52), aes(x = x, y = y),color=52,fill=addTrans(52,60))
  + geom_polygon(data = subset(allcoords,id==53), aes(x = x, y = y),color=53,fill=addTrans(53,60))
  + geom_polygon(data = subset(allcoords,id==54), aes(x = x, y = y),color=54,fill=addTrans(54,60))
  + geom_polygon(data = subset(allcoords,id==55), aes(x = x, y = y),color=55,fill=addTrans(55,60))
  + geom_polygon(data = subset(allcoords,id==56), aes(x = x, y = y),color=56,fill=addTrans(56,60))
  + geom_polygon(data = subset(allcoords,id==57), aes(x = x, y = y),color=57,fill=addTrans(57,60))
  + geom_polygon(data = subset(allcoords,id==58), aes(x = x, y = y),color=58,fill=addTrans(58,60))
  + geom_polygon(data = subset(allcoords,id==59), aes(x = x, y = y),color=59,fill=addTrans(59,60))
  + geom_polygon(data = subset(allcoords,id==60), aes(x = x, y = y),color=60,fill=addTrans(60,60))
  + geom_polygon(data = subset(allcoords,id==61), aes(x = x, y = y),color=61,fill=addTrans(61,60))
)
