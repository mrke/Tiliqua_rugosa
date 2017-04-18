find_pwet <- function(pwet) {
  
  skint=0
  
  #VAPPRS subroutine for calculating ESAT at given DB: creating function to use later...
  vapprs<-function(Tk){
    loge<-Tk
    loge[Tk>273.15] <- -7.90298*(373.16/Tk[Tk>273.15]-1)+5.02808*log10(373.16/Tk[Tk>273.15])-0.00000013816*(10^(11.344*(1-Tk[Tk>273.15]/373.16))-1)+0.0081328*(10^(-3.49149*(373.16/Tk[Tk>273.15]-1))-1)+log10(1013.246) 
    loge[Tk<=273.15] <- -9.09718*(273.16/Tk[Tk<=273.15]-1)-3.56654*log10(273.16/Tk[Tk<=273.15])+0.876793* (1-Tk[Tk<=273.15]/273.16)+log10(6.1071)
    ESAT <-(10^loge)*100 #SATuration vapour pressure (pa)
    return(ESAT)
  }  
  
  if(is.na(max(Ts))==TRUE){
    Ts<-Tb-0.001           #C, skin surface temperature, if not known this adds 0.001 to Tb
    Ta<-Tb-0.002}     
  #estimate vent rate if necessary
  if(is.na(vent.ml.min)==TRUE) { 
    #If V unknown, can be determined if MR (ml/min) is known: Assume atmospheric oxygen is 20.94%, then ventilation rate V (ml/min) is
    V.ml.s <- VO2.ml.min/(0.2094*oee/100) /60  #ml/min to ml/s
  }
  
  #estimate oxygen extraction and metabolism if necessarry
  if(is.na(oee)==TRUE){oee<-15} 
  if(is.na(max(VO2.ml.min))==TRUE){
    #If V unknown, can be determined if MR (ml/min) is known: Assume atmospheric oxygen is 20.94%, then ventilation rate V (ml/min) is
    Ms<-1                #Ms is 0 for standard MR and 1 for resting MR
    VO2.ml.min <- 0.13*mass.g^0.8*10^(0.038 * Tb)*10^(0.14*Ms) /60 #ml/hr from Anders Pough regression to ml/min
  }
  
  VEL <- wind #m/s

  Et.reported.g.s<-Et.reported.mg.min/60/1000 #mg/min to g/s
    
  if(ECdata==1){
    #Input data
    Ec.reported.g.s<-Ec.reported.mg.min/60/1000 #mg/min to g/s
    Er.reported.g.s<-Er.reported.mg.min/60/1000 #mg/min to g/s
    Eo.reported.g.s<-Eo.reported.mg.min/60/1000 #mg/min to g/s
  }  
  
  if(is.na(VEL)==TRUE){VEL<-0.0001}
  
  #constants
  mr3<-0.0038
  mr2<-0.8
  mr1<-0.013
  andens<-1000 #flesh density (kg/m3)
  T_A	<- 8000 #(K)
  T_ref <- 20 #(C)
  TAL <- 53050 #(K)
  TL <- 0.00001 #(K)
  TAH <- 90760 #(K)
  TH <- 90000 #(K)
  
  DB <- Ta #Dry bulb temp (C)
  
  ATOT <- SA.m2	#total area, m2	
  #AV <- (0.425*mass.g^0.85)/10000
  VOL <- mass.g/1000/andens
  AL <- VOL^0.333	#m,	FROM ALLOM
  At <- 0 #??
  D <- AL	#dimension for convection, m	
  
  #Dryair subroutine for calculating properties of air
  CP <- 1.01E+03	#specific heat dry air, J/kg-C
  G <- 9.80665	#acceleration from gravity, m/s
  TSTD <- 273.15	#(K)
  PTSD <- 101325	#(pa)
  VISNOT <- 1.83E-05 #reference viscocity
  TNOT <- 296.16 #reference temp
  C <- 120 #sutherlands constant 
  BP <- PTSD*((1-(0.0065*alt/288))^(1/0.190284)) #Standard atm pressure
  DENSITY <- BP/(287.04*(DB+TSTD)) #(kg/m3)
  VISDYN <- (VISNOT*((TNOT+C)/(DB+TSTD+C)))*(((DB+TSTD)/TNOT)^1.5)	#Dynamic viscosity (kg/m2), this is sutherland's formula
  VISKIN <- VISDYN/DENSITY	#Kinematic viscosity (m2/s)
  DIFVPR <- 0.0000226*(((DB+TSTD)/TSTD)^1.81)*(100000/BP) #Diffusivity water vapour in air (m2/s)
  THCOND <- 0.02425+(0.00007038*DB)	#thermal conductivity (w/m-K)
  HTOVER <- 2501200-2378.7*DB	#latent heat of vapourization (J/kg)
  TCOEFF <- 1/(DB+TSTD)	#temp coeff of volume expansion (1/K)
  GGROUP <- 0.0980616*TCOEFF/(VISKIN*VISKIN)	#group of variables in grashof number (1/m3-K)
  
  #FOR AIR
  Tk <- DB + 273.16
  ESAT <- vapprs(Tk)
  e_air <- ESAT*RHin/100 #Vapour pressure (pa)
  vd_air <- e_air * 0.018016 / (0.998 * 8.31434 * Tk) #Vapour density (kg/m3)
  
  #FOR SKIN SURFACE
  Tk <- Ts + 273.16
  ESAT <- vapprs(Tk)
  e_SAT<-ESAT
  e_skin <- ESAT*RHex/100 #Vapour pressure (pa)
  vd_skin <- e_skin * 0.018016 / (0.998 * 8.31434 * Tk) #Vapour density (kg/m3)
  
  #FOR LUNG (BREATHING)
  Tlung <- Tb #Lung temp equal to core temp (C)
  Tk <- Tlung + 273.16
  ESAT <- vapprs(Tk)
  e_SAT<-ESAT
  e_lung <- ESAT*RHex/100 #Vapour pressure (pa)
  vd_lung <- e_lung * 0.018016 / (0.998 * 8.31434 * Tk) #Vapour density (kg/m3)
  
  
  RpctO2 <- 0.2095	#Ref percent O2 (decimal %)
  RpctCO2 <- 0.0003	 #Ref percent CO2 (decimal %)
  RpctN2 <- 0.7902	#Ref percent N2 (decimal %)
  PCTO2 <- 0.2095	#percent O2 (decimal %)
  PCTCO2 <- 0.0003	 #percent CO2 (decimal %)
  PCTN2 <- 0.7902	#percent N2 (decimal %)
  RGC <- 8309.28	#UniverSA.m2l gas constant (pa-liters/mol-k)	
  PO2 <- PCTO2 * BP #Partial pressure O2 ()
  RefPO2 <- RpctO2 * 101325 #Ref partial pressure O2 ()  

  Tcorr <- exp(T_A*(1/(273+T_ref)-1/(273+Tb)))/(1+exp(TAL*(1/(273+Tb)-1/TL))+exp(TAH*(1/TH-1/(273+Tb))))
  
  O2STP <- VO2.ml.min/60/1000#*Tcorr #L/s
  VO2CON <- (O2STP*PTSD/273.15)*((Ta+273.15)/BP) #L/s, note dividing by BP
  O2MOLC <- BP*VO2CON/(RGC*(Ta+273.15)) #mol/s, note using BP to get the number of moles - not yet a mixture!
  O2MOL1 <- O2MOLC/(oee/100) #mol/s
  N2MOL1 <- O2MOL1*(PCTN2/PCTO2) #mol/s
  Airato <- (PCTN2+PCTO2+PCTCO2)/PCTO2 

  if(is.na(vent.ml.min)==FALSE) { 
    AirSTP<-V.ml.s/1000*Tcorr #L/s
    AIRML1 <-  BP*AirSTP /(RGC*(Ta +273.15)) #mol/s
  }
  if(is.na(vent.ml.min)==TRUE) { 
    AIRML1 <- O2MOL1*Airato*(RpctO2/PCTO2)*(RefPO2/PO2) #Total moles of air at 1 (mol/s)
  }
  AirVol = (AIRML1*RGC*(Ta+273.15))/BP # L/s, note dividing by BP

  #AirVol*60*1000/mass.g # ml/(g min)
  WMOL1 <- AIRML1*(e_air*(RHin/100))/(BP-e_air*(RHin/100))
  RQ <- 0.766 #Respiratory quotient, assuming O2 in equals CO2 out
  O2MOL2 <- O2MOL1 - O2MOLC #Moles O2 at 2 (mol/s)
  CO2MOL<- O2MOLC * RQ #Moles CO2 (mol/s)
  
  if(is.na(vent.ml.min)==FALSE) { 
    AIRML2 <- AIRML1 #Moles air at 2 (mol/s)
  }
  if(is.na(vent.ml.min)==TRUE) { 
    AIRML2 <- (O2MOL2+CO2MOL)*((PCTN2+PCTO2)/PCTO2)*(RpctO2/PCTO2)*(RefPO2/PO2) #Moles air at 2 (mol/s)
  }
  
  WMOL2 <- AIRML2*(e_lung/(BP-e_lung)) #Moles water at 2 (mol/s)
  EVAPMOL <- WMOL2 - WMOL1 #Moles evaporative water lost (mol/s)
  GEVAP <- EVAPMOL*18 #Evaporative water (g/s), moles lost * gram molecular weight water
  
  AirVol = (AIRML1*RGC*(Ta+273.15))/BP # L/s, note dividing by PO2
  #AirVol*60*1000/mass.g # ml/(g min)
  BETA <-	1/(Ta+273)	#coeff thermal expansion at constant density, 1/K	  
  

if(Welch == 1){  
  alpha <- 1
  FI_O2 <- RpctO2  
  rho_E <- vd_lung * 1000 # g/m3
  rho_I <- vd_air * 1000 # g/m3
  Vdot_O2 <- (VO2.ml.min/mass.g)/ 1e6 # convert from cm3 / g / min to m3 / g / min
  Vdot = Vdot_O2 / (1 * (oee/100) * FI_O2) # m3 air / (g min)
  #Vt30 = 4.31 * (mass.g/1000) ^ 0.85 # cm3 air / breath # from Bennet 1973
  #f30 = 12.8 * (mass.g/1000) ^ 0.07 # breaths / min # from Bennet 1973
  #Tcorr <- exp(T_A*(1/(273+30)-1/(273+Tb)))/(1+exp(TAL*(1/(273+Tb)-1/TL))+exp(TAH*(1/TH-1/(273+Tb))))
  #Vdot = (Vt30 * f30) / 1e6 / mass.g * Tcorr
  #Vdot*1e+6 # cm3/min  
  mdot_R = Vdot * ((rho_E / alpha) - rho_I) # g H2O / (g min)
  mdot_R_mg.g.h = mdot_R * 1000 * 60 # mg / (g h)
  Er.g.s<-mdot_R_mg.g.h/1000*mass.g/3600 # g / s  
}else{
  Er.g.s <- GEVAP #g/s
}
  
  DBair <- Ta	#C	
  BETA <-	1/(Ta+273)	#coeff thermal expansion at constant density, 1/K	  
  #convar <- ATOT-AV-At	#Convective area, m2
  convar <- ATOT-ptcond*ATOT-At	#Convective area, m2

  PR <- CP*VISDYN/THCOND
  SC <- VISDYN/(DENSITY*DIFVPR)
  DELTALT <- Ts-Ta #C	lower limit of 0.01
  DELTALT[DELTALT<0.01]<0.01
  GR <- abs(((DENSITY^2)*BETA*G*(D^3)*DELTALT)/(VISDYN^2) ) #Grashof number
  
#   if(ECdata==0){
#     VEL <- 0.0001	#air velocity, ?????	lower limit of 0.0001
#   }
  
  RE <- DENSITY*VEL*D/VISDYN #Reynold's number
  #*********************  FREE CONVECTION (Conv subroutine)  ********************  	#If GR/RE^2 is greater than 20, significant free convection
  GRRE2 <- GR/(RE^2) #Grashof/reynolds number squared	
  Raylei=(GR^.25)*(PR^.333) 		
  
  Raylei <- GR*PR	#Rayleigh number
  ANUfre <- Raylei*0 #Nusselt number
  ANUfre[Raylei < 1.0e-05]<- 0.4
  ANUfre[Raylei < 0.1]<- 0.976*Raylei^0.0784
  ANUfre[Raylei <= 100]<- 1.1173*Raylei^0.1344
  ANUfre[Raylei < 10000]<- 0.7455*Raylei^0.2167
  ANUfre[Raylei < 1*10^09]<- 0.5168*Raylei^0.2501
  ANUfre[Raylei < 1.0e-05]<- 0.5168*Raylei^0.2501
  ANUfre[Raylei < 1*10^12]<- 0.4

  HCfree <- (ANUfre*THCOND)/D	#Heat transfer coeff, W/m2k, min value of 5	- Warren finds this from ANU, but Mike finds ANU from HC and then uses ANU to get sh and hd and then awet. 
  HCfree[HCfree<5]<-5
  ANUfre=HCfree*D/THCOND #I am recalcualting ANUfre based on the fact that HC cannot be lower than 5
  Shfree <- ANUfre * (SC/PR)^0.333	#Sherwood number, ratio with no units
  Hdfree <- Shfree*DIFVPR/D		#mass.g transfer coeff, m/s
  Qfree <- HCfree * convar * (Ts - Ta) 	#Convective heat loss at skin	
  #*******************  FORCED CONVECTION  ********************* 				
  PRforc <-	0.72		#FROM ANCORR	
  SCforc <-	0.6			#FROM ANCORR
  ANUforc <- 0.35*RE^0.6	#FROM ANCORR
  Shforc <- ANUforc* (SCforc/PRforc)^0.333	 #Sherwood number
  HCforc <- ANUforc*THCOND/D		#Heat tranbsfer coeff
  Hdforc <-	Shforc*DIFVPR/D		#mass.g transfer coeff
  Qforced <-	HCforc * convar * (Ts - Ta)		#Convective heat loss at skin 
  Qconv <- Qfree + Qforced	#Total convective heat loss at skin 
  HDD <- Hdfree + Hdforc #m/s
  HC <- HCfree + HCforc #W/m2k
  
#  if(ECdata==0){
    #Following lines correct for the fact that part of animals surface is touching ground and not evaporating, I haven't included this yet
    #ptcond <- 0.25 #decimal % of surface contacting the substrate
    ##Ocular water loss
    CWLeye<-(SAeye.m2*Popen)*HDD*(vd_skin - vd_air) #kg/s
    Eo.g.s<-CWLeye*1000 #g/s
#   }else{
#     ##Ocular water loss, use measured value to get Popen
#     Eo.reported.kg.s<-Eo.reported.g.s/1000
#     Popen.reported<-Eo.reported.kg.s/HDD/(vd_skin - vd_air)/(SAeye.m2) #decimal percent
#     Eo.kg.s<-(SAeye.m2*Popen)*HDD*(vd_skin - vd_air) #decimal percent
#     Eo.g.s<-Eo.kg.s*1000 #g/s
#   }
  
  if(eyecorrect==1){ #if eyes were open
    Ec.g.s <- Et.reported.g.s - Er.g.s - Eo.g.s #g/s
  }
  if(eyecorrect==0){ #if eyes were closed or already corrected for
    Ec.g.s <- Et.reported.g.s - Er.g.s #g/s
  }
  
  if(ECdata==0){
    if(headcorrect==1){ #if skin of the head hasn't been corrected for
      Er.g.s <- Er.g.s - headskin/100*Ec.g.s  #g/s
      Ec.g.s <- Ec.g.s + headskin/100*Ec.g.s  #g/s
    }
  }
  
  Ec.kg.s<-Ec.g.s/1000 #g/s
  Aeff = Ec.kg.s / (HDD * (vd_skin - vd_air)) #m2
  skinwet = Aeff / (ATOT - ATOT*ptcond - ATOT*skint - SAeye.m2*Popen + SAeye.m2*(1-Popen)) *100
  Awet = skinwet*SA.m2 /100
  Ra <- DENSITY * CP / HC #boundary layer resistance, s/m 
  Rs <- (vd_skin - vd_air) / (Ec.kg.s/(SA.m2-SAeye.m2))  - Ra #skin resistance, s/m
  
  if(ECdata==0){
  
    Er.reported.g.s<-NA
    Ec.reported.g.s<-NA
    Eo.reported.g.s<-NA
    Er.eyecorrect.g.s<-NA
    Ec.eyecorrect.g.s<-NA  
  }else{
    Ec.reported.kg.s <- Ec.reported.g.s/1000
    Aeff.reported = Ec.reported.kg.s / (HDD * (vd_skin - vd_air)) #m2
    #skinwet.reported = Aeff.reported *100 / (ATOT - ATOT*ptcond - ATOT*skint - SAeye.m2*Popen.reported + SAeye.m2*(1-Popen.reported)) #%
  }
  Et.reported.mg.g.h <- Et.reported.g.s * 1000 / mass.g * 3600
  Er.reported.mg.g.h <- Er.reported.g.s * 1000 / mass.g * 3600
  Ec.reported.mg.g.h <- Ec.reported.g.s * 1000 / mass.g * 3600
  Eo.reported.mg.g.h <- Eo.reported.g.s * 1000 / mass.g * 3600
  Er.mg.g.h <- Er.g.s * 1000 / mass.g * 3600
  Ec.mg.g.h <- Ec.g.s * 1000 / mass.g * 3600
  Eo.mg.g.h <- Eo.g.s * 1000 / mass.g * 3600
    result<-list(Et.reported.mg.g.h=Et.reported.mg.g.h,
      Er.reported.mg.g.h=Er.reported.mg.g.h,
      Ec.reported.mg.g.h=Ec.reported.mg.g.h,
      Eo.reported.mg.g.h=Eo.reported.mg.g.h,
      Er.mg.g.h=Er.mg.g.h,
      Ec.mg.g.h=Ec.mg.g.h,
      Eo.mg.g.h=Eo.mg.g.h,
      Awet=Awet,
      skinwet=skinwet,
      Ra=Ra,
      Rs=Rs)
  return(result)
}