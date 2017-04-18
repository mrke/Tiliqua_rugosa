makegrids_lifetable <- function(data2 = data2, ma = ma, mi = mi, resamp = resamp, stat = stat, act = act){
  # append cohort number
  yearsout.names<-c("n","LONG","LAT","YEAR","MaxStg","MaxWgt","MaxLen","Tmax","Tmin","MinRes","MaxDess","MinShade","MaxShade","MinDep","MaxDep","Bsk","Forage","Dist","Food","Drink","NWaste","Feces","O2","Clutch","Fec","CauseDeath","tLay","tEgg","tStg1","tStg2","tStg3","tStg4","tStg5","tStg6","tStg7","tStg8","mStg1","mStg2","mStg3","mStg4","mStg5","mStg6","mStg7","mStg8","surviv","ovip_surviv","fitness","deathstage","cohort")
  cohort<-rep(seq(1, 20) ,20)
  cohort<-cohort[order(cohort)]
  cohort<-rep(cohort, (nrow(data2) / 400))
  data2$cohort <- cohort[1:nrow(data2)]
  colnames(data2) <- yearsout.names
  mask <- raster('../../Aust_Coarse_Mask.tif') # mask for after focal function is applied
  if(stat == "R0"){
   column <- 4
  }
  if(stat == "TT"){
   column <- 5
  }
  if(stat == "rmax"){
   column <- 6
  }
  if(stat == "surviv"){
   column <- 7
  }  
  for(i in 1:20){
    data3 <- subset(data2, data2$cohort == i) # choose cohort
    R0 <- rep(0, nrow(data3) / 20)
    TT <- R0
    rmax <- R0
    lx <- R0+1
    data3$longlat <- paste(data3$LONG, data3$LAT,sep="")
    # calculate life table and rmax
    if(data3$MaxWgt[1] == 0){
      styr <- 2
    }else{
      styr <- 1
    }
    for(j in styr:20){
      yr <- subset(data3, YEAR == j)
      if(act == 1){
        acthrs <- yr$Forage
      }
      if(act == 2){
        acthrs <- yr$Forage + yr$Bsk
      }  
      surviv <- (1 - mi) ^ (8760 - acthrs) * (1 - ma) ^ acthrs
      lx <- lx * surviv
      mx <- yr$Fec / 2
      R0 <- R0 + lx * mx
      TT <- TT + j * lx * mx
    }
    TT[R0 > 0] <- TT[R0 > 0]/R0[R0 > 0]
    rmax[R0 > 0] <- log(R0[R0 > 0]) / TT[R0 > 0]
    yr$R0 <- R0
    yr$TT <- TT
    yr$rmax <- rmax
    yr$surviv2 <- surviv
    aggdata <- paste0("cbind(aggregate(yr[,2:3], by = list(yr$longlat), FUN = max), aggregate(yr[,51:54], by = list(yr$longlat), FUN = max)[,2:5])")
    tomap <- eval(parse(text=aggdata))
    
    # get bounds of region and create grid to hold data
    lat1<-min(tomap[,3])-.025 # min latitude
    lat2<-max(tomap[,3])+.025 # max latitude
    lon1<-min(tomap[,2])-.025 # min longitude
    lon2<-max(tomap[,2])+.025 # max longitude
    quadwid<-(lon2-lon1)/.6
    quadlen<-(lat2-lat1)/.6
    gridout <- raster(ncol=quadwid, nrow=quadlen, xmn=lon1, xmx=lon2, ymn=lat1, ymx=lat2)
    x<-cbind(tomap$LONG,tomap$LAT) # list of co-ordinates
    grid <- rasterize(x, gridout, tomap[,column]) # make raster
    grid <- projectRaster(grid, crs="+proj=longlat +datum=WGS84")

    if(i==1){
      grids<-grid
    }else{
      grids<-stack(grids,grid)
    }
    cat(i,' \n')
  } 
  if(resamp == 1){
    mask2 <- resample(mask, grids[[1]], method='bilinear') 
    for(ii in 1:20){
      grids[[ii]] <- focal(grids[[ii]], w = matrix(1,3,3), fun = fill.na, pad = TRUE, na.rm = FALSE, NAonly = TRUE ) * mask2  
    }
  }
  return(grids = grids)
}