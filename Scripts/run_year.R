# function to choose specific time period of environmental data
runyear <- function(yr=yr, yst = yst, yfin = yfin, micro = micro){

  ystart <- yst# start year
  yfinish <- yfin# end year
  nyears<-yfinish-ystart+1# integer, number of years for which to run the microclimate model
  # append dates
  tzone <- paste("Etc/GMT-", 10, sep = "") # doing it this way ignores daylight savings!
  dates <- seq(ISOdate(ystart, 1, 1, tz = tzone) - 3600 * 12, ISOdate((ystart + nyears), 1, 1, tz = tzone) - 3600*13, by = "hours")
  dates <- subset(dates, format(dates, "%m/%d") != "02/29") # remove leap years
  dates2 <- seq(ISOdate(ystart, 1, 1, tz = tzone) - 3600 * 12, ISOdate((ystart + nyears), 1, 1, tz = tzone) - 3600*13, by = "days") 
  dates2 <- subset(dates2, format(dates2, "%m/%d")!= "02/29") # remove leap years
  
  if(yr >= 2){
    # subset years
    ystart <- yst - 1 + yr# start year
    yfinish <- yfinish# end year
    nyears <- yfinish-ystart+1# integer, number of years for which to run the microclimate model
    ind <- which(dates %in% subset(dates, dates>=as.POSIXct(paste(ystart,"-01-01 00:00:00",sep=""),format="%Y-%m-%d %H:%M:%S", tz=tzone) & dates<as.POSIXct(paste(yfinish+1,"-01-01 00:00:00",sep=""),format="%Y-%m-%d %H:%M:%S", tz=tzone)))
    ind2 <- which(dates2 %in% subset(dates2, dates2>=as.POSIXct(paste(ystart,"-01-01",sep=""),format="%Y-%m-%d", tz=tzone) & dates2<=as.POSIXct(paste(yfinish+1,"-01-01",sep=""),format="%Y-%m-%d", tz=tzone)))
    micro$metout <- micro$metout[ind, ]
    micro$shadmet <- micro$shadmet[ind, ]
    micro$soil <- micro$soil[ind, ]
    micro$shadsoil <- micro$shadsoil[ind, ]
    micro$soilmoist <- micro$soilmoist[ind, ]
    micro$shadmoist <- micro$shadmoist[ind, ]
    micro$soilpot <- micro$soilpot[ind, ]
    micro$shadpot <- micro$shadpot[ind, ]
    micro$humid <- micro$humid[ind, ]
    micro$shadhumid <- micro$shadhumid[ind, ]
    micro$RAINFALL <- micro$RAINFALL[ind2]
    micro$MAXSHADES <- micro$MAXSHADES[ind2]
    micro$nyears <- nyears
    micro$dim <- length(ind2)
    # append dates
    tzone <- paste("Etc/GMT+",10,sep="") # doing it this way ignores daylight savings!
    dates <- seq(ISOdate(ystart,1,1,tz=tzone)-3600*12, ISOdate((ystart+nyears),1,1,tz=tzone)-3600*13, by="hours")
    dates <- subset(dates, format(dates, "%m/%d")!= "02/29") # remove leap years
    dates2 <- seq(ISOdate(ystart,1,1,tz=tzone)-3600*12, ISOdate((ystart+nyears),1,1,tz=tzone)-3600*13, by="days") 
    dates2 <- subset(dates2, format(dates2, "%m/%d")!= "02/29") # remove leap years
  }
  
   return(list(dates=dates,dates2=dates2,micro=micro,ystart=ystart,yfinish=yfinish,nyears=nyears))
} # end of function to choose period for environmental data