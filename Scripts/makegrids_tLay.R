makegrids_tLay <- function(data2 = data2, resamp = resamp){
  # append cohort number
  yearsout.names<-c("n","LONG","LAT","YEAR","MaxStg","MaxWgt","MaxLen","Tmax","Tmin","MinRes","MaxDess","MinShade","MaxShade","MinDep","MaxDep","Bsk","Forage","Dist","Food","Drink","NWaste","Feces","O2","Clutch","Fec","CauseDeath","tLay","tEgg","tStg1","tStg2","tStg3","tStg4","tStg5","tStg6","tStg7","tStg8","mStg1","mStg2","mStg3","mStg4","mStg5","mStg6","mStg7","mStg8","surviv","ovip_surviv","fitness","deathstage","cohort")
  cohort<-rep(seq(1, 20) ,20)
  cohort<-cohort[order(cohort)]
  cohort<-rep(cohort, (nrow(data2) / 400))
  data2$cohort <- cohort[1:nrow(data2)]
  colnames(data2) <- yearsout.names
  mask <- raster('../../Aust_Coarse_Mask.tif') # mask for after focal function is applied
  for(i in 1:20){
    data3 <- subset(data2, data2$cohort == i) # choose cohort
    data3$tLay[data3$tLay == 0] <- NA
    data3$tLayMon <- data3$tLay / 365 * 12 + 1
    circ.mean.month <- function(x){
      conv <- pi / 6
      m <- mean(exp(conv * (x) * 1i), na.rm = TRUE)
      dir <- 12 + Arg(m) / conv %% 12 ## 'direction', i.e. average month
      int <- Mod(m) ## 'intensity'
      return(dir)
    }
    data3$longlat <- paste(data3$LONG, data3$LAT,sep="")
    
    # circ.mean <-function (x) 
    # {
    #   conv <- 2*pi/12 ## months -> radians
    #   x <- conv*(x)/conv
    #   sinr <- sum(sin(x), na.rm =TRUE)
    #   cosr <- sum(cos(x), na.rm =TRUE)
    #   circmean <- atan2(sinr, cosr)
    #   (circmean + 12) %% 12 + 1
    # }
    
    tomap <- cbind(aggregate(data3[,2:3], by = list(data3$longlat), FUN = max), aggregate(data3[,50], by = list(data3$longlat), FUN = circ.mean.month))[,c(2:3, 5)]
    tomap$x[is.na(tomap$x)] <- 0
    #tomap$x[tomap$x > 13]<-tomap$x[tomap$x > 13]-12
    # get bounds of region and create grid to hold data
    lat1<-min(tomap[,2])-.025 # min latitude
    lat2<-max(tomap[,2])+.025 # max latitude
    lon1<-min(tomap[,1])-.025 # min longitude
    lon2<-max(tomap[,1])+.025 # max longitude
    quadwid<-(lon2-lon1)/.6
    quadlen<-(lat2-lat1)/.6
    gridout <- raster(ncol=quadwid, nrow=quadlen, xmn=lon1, xmx=lon2, ymn=lat1, ymx=lat2)
    x<-cbind(tomap$LONG,tomap$LAT) # list of co-ordinates
    grid <- rasterize(x, gridout, tomap[,3], na.rm = TRUE) # make raster
    grid <- projectRaster(grid, crs="+proj=longlat +datum=WGS84")
    grid[grid > 13]<-grid[grid > 13]-12
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