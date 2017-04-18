ectotherm2 <- function(amass=40,lometry=3,ABSMAX=0.85,ABSMIN=0.85,VTMAX=34,VTMIN=24,TBASK=17.5,
  TEMERGE=17.5,TPREF=30,ctmax=40,ctmin=6,dayact=1,nocturn=0,crepus=0,CkGrShad=1,burrow=1,climb=0,
  shdburrow=0,mindepth=2,maxdepth=10,aquabask=0,M_1=0.013,M_2=0.8,M_3=0.038,skinwet=0.1,peyes=0.03,extref=20.,
  DELTAR=0.1,microin="none",nyears=micro$nyears,ystrt=0,enberr=0.0002,live=1,write_input=0,
  startday=1,minshade=0.,maxshades=micro$MAXSHADES,FLTYPE=0.0,SUBTK=2.79,REFL=micro$REFL,
  DEP=micro$DEP,metout=micro$metout,shadmet=micro$shadmet,soil=micro$soil,shadsoil=micro$shadsoil,
  soilmoist=micro$soilmoist,shadmoist=micro$shadmoist,humid=micro$humid,shadhumid=micro$shadhumid,
  soilpot=micro$soilpot,shadpot=micro$shadpot,RAINFALL=micro$RAINFALL,
  ectoin=rbind(as.numeric(micro$ALTT),as.numeric(micro$REFL)[1],micro$longlat[1],micro$longlat[2])
  ,customallom=c(10.4713,0.688,0.425,0.85,3.798,0.683,0.694,0.743),
  shape_a=1.,shape_b=3,shape_c=0.6666666667,FATOSK=0.4,FATOSB=0.4,rinsul=0.,ptcond=0.1,
  Spheat=3073,Flshcond=0.5,Andens=1000,EMISAN=0.95,
  warmsig=0,fosorial=0,rainact=0,actrainthresh=0.1,soilnode=4,eggshade=0,ctminthresh=12,ctkill=0,
  PFEWAT=73,PTUREA=0,FoodWater=82,minwater=15,gutfill=75,raindrink=0.,
  DEB=0,fract=1,z=2.825*fract,del_M=0.2144,F_m=12420,kap_X=0.85,v=0.02795/24.,
  kap=0.8206,p_M=48.81/24.,E_G=7512,kap_R=0.95,k_J=0.00628/24.,E_Hb=866.6*fract^3,
  E_Hj=E_Hb*fract^3,E_Hp=1.019e+04*fract^3,h_a=1.051e-08/(24.^2),s_G=0.01,
  T_REF=20,TA=8817,TAL=5.0e+04,TAH=9.0+04,TL=279,TH=306,
  E_0=9220*fract^4,f=1.,E_sm=350.,K=1,X=10,plantsim=0, andens_deb=Andens/1000,
  d_V=0.3,d_E=0.3,d_Egg=0.3,mu_X=525000,mu_E=585000,mu_V=500000,mu_P=480000,
  kap_X_P=0.1,n_X=c(1,1.8,0.5,0.15),n_E=c(1,1.8,0.5,0.15),n_V=c(1,1.8,0.5,0.15),n_P=c(1,1.8,0.5,0.15),
  n_M_nitro=c(1,4/5,3/5,4/5),metab_mode=0,stages=7,y_EV_l=0.95,S_instar=c(2.660,2.310,1.916,0),
  s_j=0.999,L_b=0.06148,v_init=3e-9,E_init=E_0/v_init,E_H_init=0,stage=0,aestivate=0,depress=0.3,
  clutchsize=5,clutch_ab=c(0,0),viviparous=0,minclutch=0,batch=1,photostart=3,photofinish=1,
  daylengthstart=12.5,daylengthfinish=13.,photodirs=1,photodirf=0,frogbreed=0,frogstage=0,
  reset=0,breedactthresh=1,breedrainthresh=0,breedtempthresh=200,breedtempcum=24*7,ma=1e-4,mi=0,mh=0.5,
  container=0,wetmod=0,conth=10,contw=100,contype=1,rainmult=1,continit=0,conthole=0,contonly=1,
  contwet=80,wetlandTemps=matrix(data = 0., nrow = 24*dim, ncol = 1),
  wetlandDepths=matrix(data = 0., nrow = 24*dim, ncol = 1),
  thermal_stages=matrix(data = c(rep(ctmin,8),rep(ctmax,8),rep(VTMIN,8),rep(VTMAX,8),rep(TBASK,8),
    rep(TPREF,8)), nrow = 8, ncol = 6),
  behav_stages=matrix(data = c(rep(dayact,8),rep(nocturn,8),rep(crepus,8),rep(burrow,8),
    rep(shdburrow,8),rep(mindepth,8),rep(maxdepth,8),rep(CkGrShad,8),rep(climb,8),rep(fosorial,8),
    rep(rainact,8),rep(actrainthresh,8),rep(breedactthresh,8),rep(flyer,8),rep(aquabask,8)), nrow = 8, ncol = 15),
  water_stages=matrix(data = c(rep(skinwet,8),rep(extref,8),rep(PFEWAT,8),rep(PTUREA,8),
    rep(FoodWater,8),rep(minwater,8),rep(raindrink,8),rep(gutfill,8)), nrow = 8, ncol = 8),
  arrhenius=matrix(data = matrix(data = c(rep(TA,8),rep(TAL,8),rep(TAH,8),rep(TL,8),rep(TH,8)),
    nrow = 8, ncol = 5), nrow = 8, ncol = 5),
  wings=0,rho1_3=0.2,trans1=0.00,aref=0.26,bref=2.04,cref=1.47,phi=179.,phimax=phi,phimin=phi,
  flyer=0,flyspeed=5,flymetab=0.1035,dessdeath=35,write_csv=0, aestdepth=7){
  #

  if(lometry==3){
    shape_a<-1.
    shape_b<-1.
    shape_c<-4.
  }
  if(lometry==4){
    shape_a<-1.
    shape_b<-1.
    shape_c<-0.5
  }

  #turn on container model if aquatic egg/larval phase
  if(frogbreed==1 | frogbreed==2){
    container<-1
  }
  if(frogbreed==3){
    container<-0
  }

  # container/pond initial conditons
  contlast<-0.
  templast<-7.

  iyear<-0 #initializing year counter
  countday<-1 #initializing day counter

  if(microin!="none"){
    message('reading microclimate input \n')
    RAINFALL<-as.matrix(read.csv(file=paste(microin,'rainfall.csv',sep=""),sep=","))[,2]
    dim=length(RAINFALL)
    metout<-read.csv(file=paste(microin,'metout.csv',sep=""),sep=",")[,-1]
    shadmet<-read.csv(file=paste(microin,'shadmet.csv',sep=""),sep=",")[,-1]
    soil<-read.csv(file=paste(microin,'soil.csv',sep=""),sep=",")[,-1]
    shadsoil<-read.csv(file=paste(microin,'shadsoil.csv',sep=""),sep=",")[,-1]
    if(file.exists(paste(microin,'wetlandTemps.csv',sep=""))){
      wetlandTemps<-read.csv(file=paste(microin,'wetlandTemps.csv',sep=""),sep=",")[,-1]
      wetlandDepths<-read.csv(file=paste(microin,'wetlandDepths.csv',sep=""),sep=",")[,-1]
    }else{
      wetlandTemps=matrix(data = 0., nrow = 24*dim, ncol = 1)
      wetlandDepths=matrix(data = 0., nrow = 24*dim, ncol = 1)
    }
    if(file.exists(paste(microin,'soilpot.csv',sep=""))){
      soilpot<-read.csv(file=paste(microin,'soilpot.csv',sep=""),sep=",")[,-1]
      soilmoist<-read.csv(file=paste(microin,'soilmoist.csv',sep=""),sep=",")[,-1]
      shadpot<-read.csv(file=paste(microin,'shadpot.csv',sep=""),sep=",")[,-1]
      shadmoist<-read.csv(file=paste(microin,'shadmoist.csv',sep=""),sep=",")[,-1]
      humid<-read.csv(file=paste(microin,'humid.csv',sep=""),sep=",")[,-1]
      shadhumid<-read.csv(file=paste(microin,'shadhumid.csv',sep=""),sep=",")[,-1]
    }else{
      soilpot<-soil
      soilmoist<-soil
      shadpot<-soil
      shadmoist<-soil
      humid<-soil
      shadhumid<-soil
      soilpot[,3:12]<-0
      soilmoist[,3:12]<-0.5
      shadpot[,3:12]<-0
      shadmoist[,3:12]<-0.5
      humid[,3:12]<-0.99
      shadhumid[,3:12]<-0.99
    }
    metout<-as.matrix(metout)
    shadmet<-as.matrix(shadmet)
    shadsoil<-as.matrix(shadsoil)
    soil<-as.matrix(soil)
    soilmoist<-as.matrix(soilmoist)
    shadmoist<-as.matrix(shadmoist)
    soilpot<-as.matrix(soilpot)
    shadpot<-as.matrix(shadpot)
    humid<-as.matrix(humid)
    shadhumid<-as.matrix(shadhumid)
    ectoin<-read.csv(file=paste(microin,'ectoin.csv',sep=""),sep=",")[,-1]
    DEP<-as.matrix(read.csv(file=paste(microin,'DEP.csv',sep=""),sep=","))[,2]
    maxshades<-as.matrix(read.csv(file=paste(microin,'MAXSHADES.csv',sep=""),sep=","))[,2]
    metout2=matrix(data = 0., nrow = 24*dim, ncol = 18)
    soil2=matrix(data = 0., nrow = 24*dim, ncol = 12)
    shadmet2=matrix(data = 0., nrow = 24*dim, ncol = 18)
    shadsoil2=matrix(data = 0., nrow = 24*dim, ncol = 12)
    soilmoist2=matrix(data = 0., nrow = 24*dim, ncol = 12)
    shadmoist2=matrix(data = 0., nrow = 24*dim, ncol = 12)
    soilpot2=matrix(data = 0., nrow = 24*dim, ncol = 12)
    shadpot2=matrix(data = 0., nrow = 24*dim, ncol = 12)
    humid2=matrix(data = 0., nrow = 24*dim, ncol = 12)
    shadhumid2=matrix(data = 0., nrow = 24*dim, ncol = 12)
    wetlandTemps2=matrix(data = 0., nrow = 24*dim, ncol = 1)
    wetlandDepths2=matrix(data = 0., nrow = 24*dim, ncol = 1)
    metout2[1:nrow(metout),]<-metout
    shadmet2[1:nrow(metout),]<-shadmet
    soil2[1:nrow(metout),]<-soil
    shadsoil2[1:nrow(metout),]<-shadsoil
    soilmoist2[1:nrow(metout),]<-soilmoist
    shadmoist2[1:nrow(metout),]<-shadmoist
    soilpot2[1:nrow(metout),]<-soilpot
    shadpot2[1:nrow(metout),]<-shadpot
    humid2[1:nrow(metout),]<-humid
    shadhumid2[1:nrow(metout),]<-shadhumid
    wetlandTemps2[1:nrow(metout)]<-wetlandTemps
    wetlandDepths2[1:nrow(metout)]<-wetlandDepths
    metout<-metout2
    shadmet<-shadmet2
    soil<-soil2
    shadsoil<-shadsoil2
    soilmoist<-soilmoist2
    shadmoist<-shadmoist2
    soilpot<-soilpot2
    shadpot<-shadpot2
    humid<-humid2
    shadhumid<-shadhumid2
    wetlandTemps<-wetlandTemps2
    wetlandDepths<-wetlandDepths2
    metout.names<-c("JULDAY","TIME","TALOC","TAREF","RHLOC","RH","VLOC","VREF","SOILMOIST3","POOLDEP","TDEEP","ZEN","SOLR","TSKYC","DEW","FROST","SNOWFALL","SNOWDEP")
    colnames(metout)<-metout.names
    colnames(shadmet)<-metout.names
    soil.names<-c("JULDAY","TIME",paste("D",DEP,"cm", sep = ""))
    colnames(soil)<-soil.names
    colnames(shadsoil)<-soil.names
    moist.names<-c("JULDAY","TIME",paste("WC",DEP,"cm", sep = ""))
    colnames(soilmoist)<-moist.names
    colnames(shadmoist)<-moist.names
    pot.names<-c("JULDAY","TIME",paste("PT",DEP,"cm", sep = ""))
    colnames(soilpot)<-pot.names
    colnames(shadpot)<-pot.names
    hum.names<-c("JULDAY","TIME",paste("RH",DEP,"cm", sep = ""))
    colnames(humid)<-hum.names
    colnames(shadhumid)<-hum.names
  }else{
    dim=length(RAINFALL)
  }

  # habitat
  ALT<-ectoin[1] # altitude (m)
  OBJDIS<-1.0 # distance from object (e.g. bush)
  OBJL<-0.0001
  PCTDIF<-0.1 # percent of sunlight that is diffuse (decimal %)
  EMISSK<-1.0 # emissivity of the sky (decimal %)
  EMISSB<-1.0 # emissivity of the substrate (decimal %)
  ABSSB<-1-ectoin[2] # solar absorbtivity of the substrate (decimal %)
  shade<-minshade # shade (%)

  # animal properties
  AMASS<-amass/1000 # animal mass (kg)
  absan<-ABSMAX # animal solar absorbtivity
  RQ<-0.8 # respiratory quotient

  FATOBJ<-0.
  TIMBAS<-1.
  SKINW<-skinwet
  skint<-0.
  O2gas<-20.95
  CO2gas<-0.03
  N2gas<-79.02
  gas<-c(O2gas,CO2gas,N2gas)
  transt<-0
  tranin<-1
  tcinit<-metout[1,"TALOC"]

  ACTLVL<-1
  nodnum<-10
  xbas<-1.
  nofood<-0
  tdigpr<-TPREF
  o2max<-extref
  maxshd<-maxshades[1]
  minshd<-minshade
  behav=c(dayact,nocturn,crepus,rainact,burrow,CkGrShad,climb,fosorial,nofood)
  julday<-1

  # conversions from percent to proportion
  PTUREA1<-PTUREA/100
  PFEWAT1<-PFEWAT/100
  FoodWater1<-FoodWater/100
  water_stages[,3]<-water_stages[,3]/100
  water_stages[,4]<-water_stages[,4]/100
  water_stages[,5]<-water_stages[,5]/100

  #DEB mass balance calculations
  E_m<-(p_M*z/kap)/v # maximum reserve density, J/cm3
  n_O<-cbind(n_X,n_V,n_E,n_P) # matrix of composition of organics, i.e. food, structure, reserve and faeces
  CHON<-c(12,1,16,14)
  wO<-CHON%*%n_O
  w_V=wO[3]
  M_V<-d_V/w_V
  y_EX<-kap_X*mu_X/mu_E # yield of reserve on food
  y_XE<-1/y_EX # yield of food on reserve
  y_VE<-mu_E*M_V/E_G  # yield of structure on reserve
  y_PX<-kap_X_P*mu_X/mu_P # yield of faeces on food
  y_PE<-y_PX/y_EX # yield of faeces on reserve  0.143382353
  nM<-matrix(c(1,0,2,0,0,2,1,0,0,0,2,0,n_M_nitro),nrow=4)
  n_M_nitro_inv<-c(-1*n_M_nitro[1]/n_M_nitro[4],(-1*n_M_nitro[2])/(2*n_M_nitro[4]),(4*n_M_nitro[1]+n_M_nitro[2]-2*n_M_nitro[3])/(4*n_M_nitro[4]),1/n_M_nitro[4])
  n_M_inv<-matrix(c(1,0,-1,0,0,1/2,-1/4,0,0,0,1/2,0,n_M_nitro_inv),nrow=4)
  JM_JO<--1*n_M_inv%*%n_O
  eta_O<-matrix(c(y_XE/mu_E*-1,0,1/mu_E,y_PE/mu_E,0,0,-1/mu_E,0,0,y_VE/mu_E,-1/mu_E,0),nrow=4)
  w_N<-CHON%*%n_M_nitro

  # DEB model initial conditions
  V_init_baby<-3e-9
  E_init_baby<-E_0/V_init_baby
  E_baby_init<-E_init_baby
  V_baby_init<-V_init_baby
  ms_init<-0.
  cumrepro_init<-0.
  q_init<-0.
  hs_init<-0.
  cumbatch_init<-0.
  pregnant<-0

  # food and food water levels
  if(length(X) == 1){ # no day-specific food levels given
    foodlevels <- rep(X, nrow(metout) / 24)
  }
  if(length(FoodWater) == 1){ # no day-specific food water levels given
    foodwaters <- rep(FoodWater, nrow(metout) / 24)
  }
  if (plantsim[1] != 0) { # plantgro model is being run
    plant <- plantgro(soilmoist = soilmoist, soilpot = soilpot,
      root_shallow = plantsim[1], root_deep = plantsim[2],
      growth_delay = plantsim[3], wilting_thresh = plantsim[4],
      permanent_wilting_point = plantsim[5], FoodWater = plantsim[6])
    foodwaters <- plant[, 3]
    foodlev <- plant[, 3]
    foodlev[foodlev > 0] <- 1
    foodlev[foodlev <= 0] <- plantsim[7] # factor by which background food level is cut down when food is dry
    if(length(X) == 1){ # no day-specific food levels given
      foodlevels <- rep(X, nrow(metout) / 24) * foodlev
    }else
      foodlevels <- X * foodlev
  }

  lat<-ectoin[4]
  julstart<-metout[1,2]
  tannul<-as.numeric(mean(soil[,12]))
  monthly<-0
  tester<-0
  microyear<-1

  ectoinput<-as.matrix(c(ALT,FLTYPE,OBJDIS,OBJL,PCTDIF,EMISSK,EMISSB,ABSSB,shade,enberr,AMASS,EMISAN,absan,RQ,rinsul,lometry,live,TIMBAS,Flshcond,Spheat,Andens,ABSMAX,ABSMIN,FATOSK,FATOSB,FATOBJ,VTMAX,VTMIN,DELTAR,SKINW,peyes,xbas,extref,TPREF,ptcond,skint,gas,transt,soilnode,o2max,ACTLVL,tannul,nodnum,tdigpr,maxshd,minshd,ctmax,ctmin,behav,julday,actrainthresh,viviparous,pregnant,conth,contw,contlast,tranin,tcinit,nyears,lat,rainmult,julstart,monthly,customallom,M_1,M_2,M_3,DEB,tester,rho1_3,trans1,aref,bref,cref,phi,wings,phimax,phimin,shape_a,shape_b,shape_c,minwater,microyear,container,flyer,flyspeed,dim,maxdepth,ctminthresh,ctkill,gutfill,mindepth,TBASK,TEMERGE,F_m,SUBTK,flymetab,continit,wetmod,contonly,conthole,contype,shdburrow,breedtempthresh,breedtempcum,contwet,warmsig,aquabask,dessdeath,write_csv,aestdepth,eggshade))
  debmod<-c(clutchsize,andens_deb,d_V,d_Egg,mu_X,mu_E,mu_V,mu_P,T_REF,z,kap,kap_X,p_M,v,E_G,kap_R,E_sm,del_M,h_a,V_init_baby,E_init_baby,k_J,E_Hb,E_Hj,E_Hp,clutch_ab[2],batch,breedrainthresh,photostart,photofinish,daylengthstart,daylengthfinish,photodirs,photodirf,clutch_ab[1],frogbreed,frogstage,eta_O,JM_JO,E_0,kap_X_P,PTUREA1,PFEWAT1,wO,w_N,FoodWater1,f,s_G,K,X,metab_mode,stages,y_EV_l,s_j,1,raindrink,reset,ma,mi,mh,aestivate,depress,minclutch,L_b)
  deblast<-c(iyear,countday,v_init,E_init,ms_init,cumrepro_init,q_init,hs_init,cumbatch_init,V_baby_init,E_baby_init,E_H_init,stage)

  origjulday<-metout[,1]
  if(ystrt>0){
    metout<-rbind(metout[((ystrt)*startday*24+1):(dim*24),],metout[1:((ystrt)*startday*24),])
    shadmet<-rbind(shadmet[((ystrt)*startday*24+1):(dim*24),],shadmet[1:((ystrt)*startday*24),])
    soil<-rbind(soil[((ystrt)*startday*24+1):(dim*24),],soil[1:((ystrt)*startday*24),])
    shadsoil<-rbind(shadsoil[((ystrt)*startday*24+1):(dim*24),],shadsoil[1:((ystrt)*startday*24),])
    soilmoist<-rbind(soilmoist[((ystrt)*startday*24+1):(dim*24),],soilmoist[1:((ystrt)*startday*24),])
    shadmoist<-rbind(shadmoist[((ystrt)*startday*24+1):(dim*24),],shadmoist[1:((ystrt)*startday*24),])
    soilpot<-rbind(soilpot[((ystrt)*startday*24+1):(dim*24),],soilpot[1:((ystrt)*startday*24),])
    shadpot<-rbind(shadpot[((ystrt)*startday*24+1):(dim*24),],shadpot[1:((ystrt)*startday*24),])
    humid<-rbind(humid[((ystrt)*startday*24+1):(dim*24),],humid[1:((ystrt)*startday*24),])
    shadhumid<-rbind(shadhumid[((ystrt)*startday*24+1):(dim*24),],shadhumid[1:((ystrt)*startday*24),])
    wetlandDepths<-c(wetlandDepths[((ystrt)*startday*24+1):(dim*24)],wetlandDepths[1:((ystrt)*startday*24)])
    wetlandTemps<-c(wetlandTemps[((ystrt)*startday*24+1):(dim*24)],wetlandTemps[1:((ystrt)*startday*24)])
    maxshades<-c(maxshades[((ystrt)*startday+1):(dim)],maxshades[1:((ystrt)*startday)])
    RAINFALL<-c(RAINFALL[((ystrt)*startday+1):(dim)],RAINFALL[1:((ystrt)*startday)])
    foodwaters<-c(foodwaters[((ystrt)*startday+1):(dim)],foodwaters[1:((ystrt)*startday)])
    metout[,1]<-origjulday
    shadmet[,1]<-origjulday
    soil[,1]<-origjulday
    shadsoil[,1]<-origjulday
    soilmoist[,1]<-origjulday
    shadmoist[,1]<-origjulday
    soilpot[,1]<-origjulday
    shadpot[,1]<-origjulday
    humid[,1]<-origjulday
    shadhumid[,1]<-origjulday
  }


  # code to determine wet periods for activity in a pond

  if(wetmod==1){
    wet_thresh<-10*24 # threshold pond duration
    wet_depth<-100 # threshold pond depth (mm)
    wet_temp<-28 # threshold exit temp (deg C)
    b<-cbind(as.data.frame(wetlandDepths),as.data.frame(wetlandTemps))
    colnames(b)<-c('depth','temp')
    b$depth[b$temp>wet_temp]<-0
    b<-b$depth
    b[b>=wet_depth]<-1
    b[b!=1]<-0
    bb<-rle(b)
    bb$values[bb$lengths<wet_thresh]<-0
    c<-b*0
    values<-bb$values
    lengths<-bb$lengths
    for(k in 1:length(bb$values)){
      d<-c(rep(values[k],lengths[k]))
      if(k==1){
        e<-d
      }else{
        e<-c(e,d)
      }
    }
    wetlandDepths<-wetlandDepths*e
  }

  if(write_input==1){
    if(dir.exists("ecto csv input")==FALSE){
      dir.create("ecto csv input")
    }
    message('writing input csv files \n')
    write.csv(ectoinput, file = "ecto csv input/ectoinput.csv")
    write.csv(debmod, file = "ecto csv input/debmod.csv")
    write.csv(deblast, file = "ecto csv input/deblast.csv")
    write.csv(RAINFALL, file = "ecto csv input/rainfall.csv")
    write.csv(DEP, file = "ecto csv input/dep.csv")
    write.csv(foodwaters, file = "ecto csv input/foodwaters.csv")
    write.csv(foodlevels, file = "ecto csv input/foodlevels.csv")
    write.csv(wetlandTemps, file = "ecto csv input/wetlandTemps.csv")
    write.csv(wetlandDepths, file = "ecto csv input/wetlandDepths.csv")
    write.csv(arrhenius, file = "ecto csv input/arrhenius.csv")
    write.csv(thermal_stages, file = "ecto csv input/thermal_stages.csv")
    write.csv(behav_stages, file = "ecto csv input/behav_stages.csv")
    write.csv(water_stages, file = "ecto csv input/water_stages.csv")
    write.csv(maxshades, file = "ecto csv input/Maxshades.csv")
    write.csv(S_instar, file = "ecto csv input/S_instar.csv")
    write.table(metout[(seq(1,dim*24)),], file = "ecto csv input/metout.csv",sep=",",row.names=FALSE)
    write.table(shadmet[(seq(1,dim*24)),], file = "ecto csv input/shadmet.csv",sep=",",row.names=FALSE)
    write.table(soil[(seq(1,dim*24)),], file = "ecto csv input/soil.csv",sep=",",row.names=FALSE)
    write.table(shadsoil[(seq(1,dim*24)),], file = "ecto csv input/shadsoil.csv",sep=",",row.names=FALSE)
    write.table(soilmoist[(seq(1,dim*24)),], file = "ecto csv input/soilmoist.csv",sep=",",row.names=FALSE)
    write.table(shadmoist[(seq(1,dim*24)),], file = "ecto csv input/shadmoist.csv",sep=",",row.names=FALSE)
    write.table(soilpot[(seq(1,dim*24)),], file = "ecto csv input/soilpot.csv",sep=",",row.names=FALSE)
    write.table(shadpot[(seq(1,dim*24)),], file = "ecto csv input/shadpot.csv",sep=",",row.names=FALSE)
    write.table(humid[(seq(1,dim*24)),], file = "ecto csv input/humid.csv",sep=",",row.names=FALSE)
    write.table(shadhumid[(seq(1,dim*24)),], file = "ecto csv input/shadhumid.csv",sep=",",row.names=FALSE)
  }
  ecto<-list(dim=dim,ectoinput=ectoinput,metout=metout,shadmet=shadmet,soil=soil,shadsoil=shadsoil,soilmoist=soilmoist,shadmoist=shadmoist,soilpot=soilpot,shadpot=shadpot,humid=humid,shadhumid=shadhumid,DEP=DEP,RAINFALL=RAINFALL,iyear=iyear,countday=countday,debmod=debmod,deblast=deblast,foodwaters=foodwaters,foodlevels=foodlevels,wetlandTemps=wetlandTemps,wetlandDepths=wetlandDepths,arrhenius=arrhenius,thermal_stages=thermal_stages,behav_stages=behav_stages,water_stages=water_stages,maxshades=maxshades,S_instar=S_instar)

  message('running ectotherm model ... \n')

  ptm <- proc.time() # Start timing
  ectout<-ectorun(ecto)
  message(paste0('runtime ', (proc.time() - ptm)[3], ' seconds')) # Stop the clock

  environ<-ectout$environ[1:(dim*24),]
  enbal<-ectout$enbal[1:(dim*24),]
  masbal<-ectout$masbal[1:(dim*24),]
  debout<-ectout$debout[1:(dim*24),]
  yearout<-ectout$yearout
  yearsout<-ectout$yearsout[1:nyears,]

  if(DEB==0){
    return(list(soil=soil,shadsoil=shadsoil,metout=metout,shadmet=shadmet,soilmoist=soilmoist,shadmoist=shadmoist,soilpot=soilpot,shadpot=shadpot,humid=humid,shadhumid=shadhumid,RAINFALL=RAINFALL,enbal=enbal,environ=environ,masbal=masbal,yearout=yearout,yearsout=yearsout,foodwaters=foodwaters,foodlevels=foodlevels,VTMAX=VTMAX,VTMIN=VTMIN,ctmax=ctmax,ctmin=ctmin,TBASK=TBASK,TEMERGE=TEMERGE))
  }else{
    return(list(soil=soil,shadsoil=shadsoil,metout=metout,shadmet=shadmet,soilmoist=soilmoist,shadmoist=shadmoist,soilpot=soilpot,shadpot=shadpot,humid=humid,shadhumid=shadhumid,RAINFALL=RAINFALL,enbal=enbal,masbal=masbal,environ=environ,debout=debout,yearout=yearout,yearsout=yearsout,foodwaters=foodwaters,foodlevels=foodlevels,VTMAX=VTMAX,VTMIN=VTMIN,ctmax=ctmax,ctmin=ctmin,TBASK=TBASK,TEMERGE=TEMERGE))
  }

}
