aggwaddle <- function(sleepy){
  # aggregate sleepy lizard data by hour
  agglist <- list(paste(sleepy$Year, "_", sleepy$Month, "_", sleepy$Day, "_", sleepy$Hours, sep = ""))
  date_waddle <- aggregate(sleepy$date_waddle, by = agglist, min)
  doy <- aggregate(sleepy$doy, by = agglist,max)
  Tb <- aggregate(sleepy$Temperature, by = agglist,mean)
  steps <- aggregate(sleepy$Steps, by = agglist,sum)
  year <- aggregate(sleepy$Year, by = agglist,max)
  month <- aggregate(sleepy$Month, by = agglist,max)
  day <- aggregate(sleepy$Day, by = agglist,max)
  hours <- aggregate(sleepy$Hours, by = agglist,max)
  sleepy <- as.data.frame(cbind(as.data.frame(date_waddle[,2]),doy[,2],Tb[,2],steps[,2],year[,2],month[,2],day[,2],hours[,2]))
  colnames(sleepy) = c('date_waddle','doy','Temperature','Steps','Year','Month','Day','Hours')
  
  # round the times to get rid of occasional 1 min add-ons
  x <- sleepy$date_waddle
  r <-  60 * 60
  H <- as.integer(format(x, "%H"))
  M <- as.integer(format(x, "%M"))
  S <- as.integer(format(x, "%S"))
  D <- format(x, "%Y-%m-%d")
  secs <- 3600 * H + 60 * M + S
  x <- as.POSIXct(round(secs / r) * r, origin = D) - 11 * 3600
  sleepy$date_waddle <- x
  return(sleepy)
}