get_waddle <- function(i = 1, sleepy_id = NULL, year = NULL){

  waddlefiles <- read.csv("waddleometer/waddle_files_all.csv")
  sex <- read.csv("waddleometer/sex.csv",stringsAsFactors = FALSE)
  if(is.null(sleepy_id)){
   sleepy_id <- waddlefiles[i, 2] # get lizard ID
  }else{
    i <- which(waddlefiles$id %in% sleepy_id & waddlefiles$year %in% year)
  }
  sleepy_id <- waddlefiles[i, 2] # get lizard ID
  sexlizard <- as.character(subset(sex, Liz==sleepy_id)[2]) # get lizard sex
  if(sexlizard == "u" | sexlizard == "character(0)"){
    sexlizard <- 'unknown'
  }
  if(sexlizard == 2){
    sexlizard <- 'F'
  }
  if(sexlizard == 3){
    sexlizard <- 'M'
  }
  
  sleepy <- as.data.frame(read.table(file = paste('waddle/',waddlefiles[i, 3],sep = ""), sep = ",", head = TRUE, stringsAsFactors = FALSE)) # read in data
  
  if(is.null(sleepy$Hours)){ # one off creation of hour and minute columns
    Hours <- as.numeric(substr(sleepy$Time, 1, 2))
    Minutes <- as.numeric(substr(sleepy$Time, 4, 5))
    sleepy <- as.data.frame(cbind(sleepy, Hours, Minutes))
    write.table(sleepy, file = paste('waddle/',waddlefiles[i, 3],sep = ""),row.names = F, sep = ",") 
  }
  curyear <- max(sleepy$Year, na.rm = TRUE) # get year of this individual
  
  
  date_waddle1 <- with(sleepy, ISOdatetime(Year, Month, Day, Hours, Minutes, 0)) # create date
  date_waddle <- date_waddle1 + 60*60 # adjust for Central Time
  doy <- strptime(format(date_waddle, "%y/%m/%d"), "%y/%m/%d")$yday + 1 # get waddle day of year
  sleepy <- cbind(date_waddle, doy, sleepy) # append dates and day of year to sleepy data

  
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
  return(list(curyear = curyear, sexlizard = sexlizard, sleepy = sleepy, date_waddle = date_waddle, sleepy_id = sleepy_id))
}