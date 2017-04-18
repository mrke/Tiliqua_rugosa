makegrids_yearout <- function(data2, variab, stat, resamp){
  # append cohort number
  yearout.names<-c('year','LONG','LAT','DEVTIME','BIRTHDAY','BIRTHMASS','MONMATURE','MONREPRO','SVLREPRO','FECUNDITY','CLUTCHES','ANNUALACT','MINRESERVE','LASTFOOD','TOTFOOD','MINTB','MAXTB','Pct_Desic','LifeSpan','GenTime','R0','rmax','SVL')
  colnames(data2) <- yearout.names
  #data2<-subset(data2,MaxWgt>0)
  mask <- raster('../../Aust_Coarse_Mask.tif') # mask for after focal function is applied
  
  data2$longlat <- paste(data2$LONG, data2$LAT,sep="")

  var <- which(yearout.names == variab)
  for(i in 1:20){
    data3 <- subset(data2, data2$year == i) # choose cohort
    aggdata <- paste0("cbind(aggregate(data3[,2:3], by = list(data3$longlat), FUN = max), aggregate(data3[,4:24], by = list(data3$longlat), FUN = ",stat,")[,2:21])")
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
  names(grids) <- seq(1990,2009)
  return(grids = grids)
}
