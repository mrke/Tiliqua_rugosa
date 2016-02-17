basedir<-getwd()
setwd("/hsm/VR0212/shared/NicheMapR/")
args <- (commandArgs(TRUE))
simnum<-as.numeric(args[1])
bioregion<-as.numeric(args[2])
scenario<-""

barcoo<-paste('/scratch/VR0212/bio',bioregion,'/',sep="")
load(paste(barcoo,'longlat.bin',sep=''))
longlat <- data[simnum,1:2]

numsites<-ceiling(nrow(data)/2/1000)
jstart<-numsites*(simnum-1)+1
jfinish<-numsites*(simnum-1)+numsites

if(jstart<=nrow(data)){
  
  if(jfinish>nrow(data)){
    jfinish<-nrow(data)
  }
  
  
  for(jobnum in jstart:jfinish){
    
    longlat <- c(data[jobnum,1],data[jobnum,2])

#longlat<-c(139.3109, -33.888) 
#longlats<-read.csv("/vlsci/VR0212/shared/NicheMapR_Working/bioregion_points_final.csv")
#longlats<-subset(longlats,RASTERVALU!=0)
#lng<-which.min(abs(longlats[,1]-longlat[1]))
#coarselong<-longlats[lng,1]
#longlats<-subset(longlats,longlats$long==longlats[lng,1])
#lt<-which.min(abs(longlats[,2]-longlat[2]))
#coarselat<-longlats[lt,2]
#bioregion<-longlats[lt,3]
#barcoo<-paste('/scratch/VR0212/bio',bioregion,'/',sep="")
#load(paste(barcoo,'longlat.bin',sep=''))
#jobnum<-as.numeric(rownames(subset(data,data$V2==coarselong & data$V3==coarselat)))
quadrangle<-jobnum



############# ectotherm model parameters ################################
source("/hsm/VR0212/shared/NicheMapR/micro_aust.R")
source("/hsm/VR0212/shared/NicheMapR/microrun.R")
source("/hsm/VR0212/shared/NicheMapR/ectotherm.R")
source("/hsm/VR0212/shared/NicheMapR/ectorun.R")
source("/hsm/VR0212/shared/NicheMapR/getgads.R")
source("/hsm/VR0212/shared/NicheMapR/get.soil.R")
source("/hsm/VR0212/shared/NicheMapR/plantgro.R")
dir.exists <- function(d) {
    de <- file.info(d)$isdir
    ifelse(is.na(de), FALSE, de)
}

load('/hsm/VR0212/shared/NicheMapR/CampNormTbl9_1.rda')

loc<-c(as.numeric(longlat[1]),as.numeric(longlat[2]))
ystart <- 1990# start year for weather generator calibration dataset or AWAP database
yfinish <- 2009# end year for weather generator calibration dataset
nyears<-yfinish-ystart+1# integer, number of years for which to run the microclimate model, only for AWAP data (!!max 10 years!!)

DEP <- c(0., 1.,  3, 5.,  10,  15,  30.,  60.,  100.,  200.) # Soil nodes (cm) - keep spacing close near the surface, last value is where it is assumed that the soil temperature is at the annual mean air temperature
#DEP <- c(0., 2.5,  5, 10,  15,  20,  30.,  60.,  100.,  200.) # Soil nodes (cm) - keep spacing close near the surface, last value is where it is assumed that the soil temperature is at the annual mean air temperature
  soil.hydro<-get.soil(path="/hsm/VR0212/shared/CSIRO Soil and Landscape Grid/")
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
  BulkDensity <- BD[seq(1,19,2)]*1000#rep(1360,10) # soil bulk density (kg/m3)
  
  # run microclimate model to get microclimate for ectotherm model and soil temps for predicting egg development and food availability
  micro<-micro_aust(quadrangle=quadrangle, barcoo=barcoo, scenario=scenario, loc = longlat, ystart = ystart, yfinish = yfinish, PE = PE, BB = BB, BD = 
      BD, KS = KS, BulkDensity = BulkDensity, maxshade = 50, Usrhyt = 3, DEP = DEP, REFL = 0.2, vlsci=1, write_input=1)

  micro$humid[,3:9]<-micro$metout[,5]/100 # assume ambient humidity down to 30cm
  micro$shadhumid[,3:9]<-micro$shadmet[,5]/100 # assume ambient humidity down to 30cm
  micro$humid[,7:12]<-0.8 # assume higher humidity in burrow, 60cm and lower
  micro$shadhumid[,7:12]<-0.8 # assume higher humidity in burrow, 60cm and lower

  micro$shadmet[,10]<-0
  micro$metout[,10]<-0

debpars=as.data.frame(read.csv('projects/Tiliqua_rugosa/DEB_pars_Tiliqua_rugosa.csv',header=FALSE))$V1
startday=1#365*6 # make it 90 for T. rugosa loop day of year at which DEB model starts
TBASK = 26
raindrink=2.5
mu_E = 585000
peyes = 0.03
skinwet = 0.2
for (ystrt in 0:19){
nicheout=ectotherm(ABSMAX = 0.866, ABSMIN = 0.866, VTMAX = 39, VTMIN = 26, write_input = 0, EMISAN = 1, gutfill = 75,
  TBASK = TBASK, TEMERGE = 8.5, ctmax = 39, ctmin = 3.5, TPREF = 33.5,  peyes = peyes , ptcond = 0.1, skinwet = skinwet,
  shdburrow = 1, minwater = 100, maxshades = micro$MAXSHADES, mindepth = 3, raindrink = raindrink, DEB = 1, z=debpars[8], del_M=debpars[9], F_m = 13290,  
  kap_X=debpars[11],   v=debpars[13]/24, kap=debpars[14], p_M=debpars[16]/24, 
  E_G=debpars[19],   kap_R=debpars[15], k_J=debpars[18]/24, E_Hb=debpars[20],
  E_Hp=debpars[21], h_a=debpars[22]*10^-1/(24^2),   s_G=debpars[23],   E_0=debpars[24],  mu_E = mu_E,
  T_REF = debpars[1]-273, TA = debpars[2], TAL = debpars[5], TAH = debpars[6], TL = debpars[3], TH = debpars[4], 
  E_sm=186.03*6, K = 500, X = 11.7, plantsim = c(4, 4, 14, -200, -1500, 82), clutch_ab = c(0.085,0.7), viviparous = 1,
  photostart = 4, E_init = ((debpars[16]/24)*debpars[8]/debpars[14])/(debpars[13]/24), E_H_init = debpars[20]+5,  
  v_init = debpars[25]^3, stage = 1, mh = 1,  minclutch = 0, clutchsize = 2., startday = startday, ystrt=ystrt)

# retrieve output
#metout=as.data.frame(nicheout$metout)
#shadmet=as.data.frame(nicheout$shadmet)
#soil=as.data.frame(nicheout$soil)
#shadsoil=as.data.frame(nicheout$shadsoil)
#rainfall=as.data.frame(nicheout$RAINFALL)
#foodwaters=as.data.frame(nicheout$foodwaters)
#foodlevels=as.data.frame(nicheout$foodlevels)
#environ=as.data.frame(nicheout$environ)
#enbal=as.data.frame(nicheout$enbal)
#masbal=as.data.frame(nicheout$masbal)
#humid=as.data.frame(nicheout$humid)
#debout=as.data.frame(nicheout$debout)
yearout=as.data.frame(nicheout$yearout)
#foodwaters=as.data.frame(nicheout$foodwaters)
#foodlevels=as.data.frame(nicheout$foodlevels)
if(nyears>1){
  yearsout=as.data.frame(nicheout$yearsout[1:nyears,])
}else{
  yearsout=t(as.data.frame(nicheout$yearsout))
}

yearoutput<-cbind(longlat[1],longlat[2],yearout)
yearsoutput<-cbind(longlat[1],longlat[2],yearsout)
if(ystrt==0){
  yearout_loop<-yearout
  yearsout_loop<-yearsout
}else{
  yearout_loop<-rbind(yearout_loop,yearout)
  yearsout_loop<-rbind(yearsout_loop,yearsout)
}  
} #end loop through 20 year blocks
yearoutput_loop<-cbind(longlat[1],longlat[2],yearout_loop)
yearsoutput_loop<-cbind(longlat[1],longlat[2],yearsout_loop)
setwd("/hsm/VR0212/shared/NicheMapR/projects/Tiliqua_rugosa/")

write.table(yearoutput_loop, file = paste("yearoutput_loop_nohreg.csv",sep=""), sep = ",", col.names = F, qmethod = "double", append = T)
write.table(yearsoutput_loop, file = paste("yearsoutput_loop_nohreg.csv",sep=""), sep = ",", col.names = F, qmethod = "double",append = T)
} # end loop through sites
}