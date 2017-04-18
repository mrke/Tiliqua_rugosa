makegrids_yearsout <- function(data2, variab, stat, resamp){
  # append cohort number
  yearsout.names<-c("n","LONG","LAT","YEAR","MaxStg","MaxWgt","MaxLen","Tmax","Tmin","MinRes","MaxDesic","MinShade","MaxShade","MinDep","MaxDep","Bsk","Forage","Dist","Food","Drink","NWaste","Feces","O2","Clutch","Fec","CauseDeath","tLay","tEgg","tStg1","tStg2","tStg3","tStg4","tStg5","tStg6","tStg7","tStg8","mStg1","mStg2","mStg3","mStg4","mStg5","mStg6","mStg7","mStg8","surviv","ovip_surviv","fitness","deathstage","cohort")
  cohort<-rep(seq(1, 20) ,20)
  cohort<-cohort[order(cohort)]
  cohort<-rep(cohort, (nrow(data2) / 400))
  data2$cohort <- cohort[1:nrow(data2)]
  colnames(data2) <- yearsout.names
  mask <- raster('../../Aust_Coarse_Mask.tif') # mask for after focal function is applied
  
  var <- which(yearsout.names == variab)
  for(i in 1:20){
    data3 <- subset(data2, data2$cohort == i) # choose cohort
    
    # aggregate to get cumulative reproduction
    data3$longlat <- paste(data3$LONG, data3$LAT,sep="")
    aggdata <- paste0("cbind(aggregate(data3[,2:3], by = list(data3$longlat), FUN = max), aggregate(data3[,4:49], by = list(data3$longlat), FUN = ",stat,")[,2:45])")
    tomap <- eval(parse(text=aggdata))
    
    #tomap <- cbind(aggregate(data3[,2:3], by = list(data3$longlat), FUN = max), aggregate(data3[,4:49], by = list(data3$longlat), FUN = sum)[,2:45]) 
    
    # get bounds of region and create grid to hold data
    lat1<-min(tomap[,3])-.025 # min latitude
    lat2<-max(tomap[,3])+.025 # max latitude
    lon1<-min(tomap[,2])-.025 # min longitude
    lon2<-max(tomap[,2])+.025 # max longitude
    quadwid<-(lon2-lon1)/.6
    quadlen<-(lat2-lat1)/.6
    gridout <- raster(ncol=quadwid, nrow=quadlen, xmn=lon1, xmx=lon2, ymn=lat1, ymx=lat2)
    x<-cbind(tomap$LONG,tomap$LAT) # list of co-ordinates
    grid <- rasterize(x, gridout, tomap[,var]) # make raster
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