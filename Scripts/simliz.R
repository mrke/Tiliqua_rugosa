simliz <- function(lizard = lizard, yrstrt = yrstrt, init = init, init2 = init2, yrstrt2 = yrstrt, limx = 1){
  
  subyear<-runyear(yst = 1971, yfin = 2015, yr = yrstrt, micro = micro_all) 
  micro <- subyear$micro
  ystart <- subyear$ystart
  yfinish <- subyear$yfinish
  nyears <- subyear$nyears
  dates <- subyear$dates
  dates2 <- subyear$dates2
  dim <- micro$dim
  nyears=micro$nyears
  maxshades=micro$MAXSHADES
  REFL=micro$REFL
  DEP=micro$DEP
  metout=micro$metout
  shadmet=micro$shadmet
  soil=micro$soil
  shadsoil=micro$shadsoil
  soilmoist=micro$soilmoist
  shadmoist=micro$shadmoist
  humid=micro$humid
  shadhumid=micro$shadhumid
  soilpot=micro$soilpot
  shadpot=micro$shadpot
  RAINFALL=micro$RAINFALL
  ectoin=rbind(as.numeric(micro$ALTT), as.numeric(micro$REFL)[1], micro$longlat[1], micro$longlat[2])
  if(yrstrt <= yrstrt2){
    dates3 <- dates2
    RAINFALL3 <- RAINFALL
  }
  # Inital conditions for DEB model:
  if(init == 1){
    E_init = ((debpars[16]/24)*debpars[8]/debpars[14])/(debpars[13]/24) # initial reserve
    E_H_init = debpars[20]+5 # initial maturity
    v_init = debpars[25]^3 # initial structure
    stage = 1 # initial stage
  }
  if(init == 2){
    E_H_init <- debpars[21]+5 # mature
    v_init <- debpars[27]^3 # structure 
    E_init <- ((debpars[16] / 24) * debpars[8] / debpars[14]) / (debpars[13] / 24) # [E_m]
    stage <- 2 # Initial stage (2 = early mature)
  }
  if(init == 3){
    E_H_init <- debpars[21]+5 # mature
    v_init <- debpars[26]^3 * 0.7 # structure 
    E_init <- ((debpars[16] / 24) * debpars[8] / debpars[14]) / (debpars[13] / 24) # [E_m]
    stage <- 3 # Initial stage (3 = late mature)
  }

  liz <- subset(sleepydata, LIZN == lizard)
  lenmax <- mean(liz$LGTH[floor(length(liz$LGTH) / 2) : length(liz$LGTH)], na.rm = TRUE)
  fract <- lenmax / 33
  #clutch_ab = c(0.085,0.7*(1-fract+1))#c(0.085,0.7) # paramaters for relationship between length (cm) and clutch size: clutch size = a*SVL-b
  clutch_ab = c(0, 0) # fix clutch size at 2 for these sims - something going wrong with clutch size relationship when scaling to different sizes
  
  raindrink = 3.51
  # run ectotherm model
  ecto <- ectotherm(ABSMAX = ABSMAX, ABSMIN = ABSMIN, VTMAX = VTMAX, VTMIN = VTMIN, EMISAN = EMISAN, gutfill = gutfill, TBASK = TBASK, TEMERGE = TEMERGE, ctmax = ctmax, ctmin = ctmin, TPREF = TPREF,  peyes = peyes, ptcond = ptcond, skinwet = skinwet, shdburrow = shdburrow, minwater = minwater, maxshades = maxshades, mindepth = mindepth, raindrink = raindrink, DEB = DEB, z=z*fract, del_M=del_M, F_m = F_m*fract, kap_X=kap_X, v=v, kap=kap, p_M=p_M, E_G=E_G, kap_R=kap_R, k_J=k_J, E_Hb=E_Hb*fract^3, E_Hp=E_Hp*fract^3,   h_a=h_a, s_G=s_G, E_0=E_0*fract^4, T_REF = T_REF, TA = TA, TAL = TAL, TAH = TAH, TL = TL, TH = TH, E_sm=E_sm, K = K, X = X, plantsim = plantsim, clutch_ab = clutch_ab, viviparous = viviparous, photostart = photostart, E_init = E_init, E_H_init = E_H_init, v_init = v_init, stage = stage, mh = mh,  clutchsize = clutchsize, aestdepth = aestdepth, startday = startday, ma = ma, aestivate = aestivate, depress = depress, nyears=nyears,  REFL=REFL,  DEP=DEP,  metout=metout,  shadmet=shadmet,  soil=soil,  shadsoil=shadsoil,  soilmoist=soilmoist,  shadmoist=shadmoist,  humid=humid,  shadhumid=shadhumid,  soilpot=soilpot,  shadpot=shadpot,  RAINFALL=RAINFALL,  ectoin=ectoin)
  
  debout<-as.data.frame(ecto$debout)
  debout<-cbind(dates,debout)
  
  raindrink = 0
  # run ectotherm model
  subyear<-runyear(yst = 1971, yfin = 2015, yr = yrstrt2, micro = micro_all) 
  micro <- subyear$micro
  ystart <- subyear$ystart
  yfinish <- subyear$yfinish
  nyears <- subyear$nyears
  dates_drk <- subyear$dates
  dates2 <- subyear$dates2
  dim <- micro$dim
  nyears=micro$nyears
  maxshades=micro$MAXSHADES
  REFL=micro$REFL
  DEP=micro$DEP
  metout=micro$metout
  shadmet=micro$shadmet
  soil=micro$soil
  shadsoil=micro$shadsoil
  soilmoist=micro$soilmoist
  shadmoist=micro$shadmoist
  humid=micro$humid
  shadhumid=micro$shadhumid
  soilpot=micro$soilpot
  shadpot=micro$shadpot
  RAINFALL=micro$RAINFALL
  ectoin=rbind(as.numeric(micro$ALTT), as.numeric(micro$REFL)[1], micro$longlat[1], micro$longlat[2])
  if(yrstrt > yrstrt2){
    dates3 <- dates2
    RAINFALL3 <- RAINFALL
  }
  if(init2 == 1){
    E_init = ((debpars[16]/24)*debpars[8]/debpars[14])/(debpars[13]/24) # initial reserve
    E_H_init = debpars[20]+5 # initial maturity
    v_init = debpars[25]^3 # initial structure
    stage = 1 # initial stage
  }
  if(init2 == 2){
    E_H_init <- debpars[21]+5 # mature
    v_init <- debpars[27]^3 # structure 
    E_init <- ((debpars[16] / 24) * debpars[8] / debpars[14]) / (debpars[13] / 24) # [E_m]
    stage <- 2 # Initial stage (2 = early mature)
  }
  if(init2 == 3){
    E_H_init <- debpars[21]+5 # mature
    v_init <- debpars[26]^3 * 0.7 # structure 
    E_init <- ((debpars[16] / 24) * debpars[8] / debpars[14]) / (debpars[13] / 24) # [E_m]
    stage <- 3 # Initial stage (3 = late mature)
  }
  
  ecto_drk <- ectotherm(ABSMAX = ABSMAX, ABSMIN = ABSMIN, VTMAX = VTMAX, VTMIN = VTMIN, EMISAN = EMISAN, gutfill = gutfill, TBASK = TBASK, TEMERGE = TEMERGE, ctmax = ctmax, ctmin = ctmin, TPREF = TPREF,  peyes = peyes, ptcond = ptcond, skinwet = skinwet, shdburrow = shdburrow, minwater = minwater, maxshades = maxshades, mindepth = mindepth, raindrink = raindrink, DEB = DEB, z=z*fract, del_M=del_M, F_m = F_m*fract, kap_X=kap_X, v=v, kap=kap, p_M=p_M, E_G=E_G, kap_R=kap_R, k_J=k_J, E_Hb=E_Hb*fract^3, E_Hp=E_Hp*fract^3,   h_a=h_a, s_G=s_G, E_0=E_0*fract^4, T_REF = T_REF, TA = TA, TAL = TAL, TAH = TAH, TL = TL, TH = TH, E_sm=E_sm, K = K, X = X, plantsim = plantsim, clutch_ab = clutch_ab, viviparous = viviparous, photostart = photostart, E_init = E_init, E_H_init = E_H_init, v_init = v_init, stage = stage, mh = mh,  clutchsize = clutchsize, aestdepth = aestdepth, startday = startday, ma = ma, aestivate = aestivate, depress = depress, nyears=nyears,  REFL=REFL,  DEP=DEP,  metout=metout,  shadmet=shadmet,  soil=soil,  shadsoil=shadsoil,  soilmoist=soilmoist,  shadmoist=shadmoist,  humid=humid,  shadhumid=shadhumid,  soilpot=soilpot,  shadpot=shadpot,  RAINFALL=RAINFALL,  ectoin=ectoin)
  foodwaters<-ecto$foodwaters
  foodwaters_drk<-ecto_drk$foodwaters
  if(yrstrt > yrstrt2){
    foodwaters3 <- foodwaters_drk
  }else{
    foodwaters3 <- foodwaters
  }
  debout_drk<-as.data.frame(ecto_drk$debout)
  debout_drk<-cbind(dates_drk,debout_drk)
  plot_growth(lizard = lizard, debout = debout, debout_drk = debout_drk, dates = dates, dates_drk = dates_drk, limx = limx, dates3 = dates3, foodwaters3 = foodwaters3, RAINFALL = RAINFALL3) # lizard to do
}