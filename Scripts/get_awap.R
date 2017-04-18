get_awap <- function(loc, datestart, datefinish){
  library(RODBC)
  library(chron)
  channel <- odbcConnect("AWAPDaily")
  
  if(is.numeric(loc)==FALSE){ # use geocode to get location from site name via googlemaps
    if (!requireNamespace("dismo", quietly = TRUE)) {
      stop("dismo needed for the place name geocode function to work. Please install it.",
        call. = FALSE)
    }
    if (!requireNamespace("XML", quietly = TRUE)) {
      stop("XML needed for the place name geocode function to work. Please install it.",
        call. = FALSE)
    }
    longlat <- dismo::geocode(loc)[3:4] # assumes first geocode match is correct
    if(nrow(longlat>1)){longlat<-longlat[1,]}
  }else{
    longlat <- loc
  }
  
  lat<-longlat[2]
  lon<-longlat[1]
  lat1<-lat-0.025
  lat2<-lat+0.025
  lon1<-lon-0.025
  lon2<-lon+0.025
  
  datestart<-strptime(datestart, "%d/%m/%Y", tz = "UTC") # convert to date format
  datefinish<-strptime(datefinish, "%d/%m/%Y", tz = "UTC") # convert to date format
  yearstart<-as.numeric(format(datestart, "%Y")) # yet year start
  yearfinish<-as.numeric(format(datefinish, "%Y")) # yet year finish
  years<-seq(yearstart,yearfinish,1) # get sequence of years to do
  juldaystart<-datestart$yday+1 # get Julian day of year at start
  juldayfinish<-datefinish$yday+1 # get Julian day of year at finish
  
  for(i in 1:length(years)){ # start loop through years
    # syntax for query
    
    if(length(years)==1){ # doing a period within a year
      query<-paste("SELECT a.latitude, a.longitude, b.*
    FROM [AWAPDaily].[dbo].[latlon] as a
  , [AWAPDaily].[dbo].[",years[i],"] as b
  where (a.id = b.id) and (a.latitude between ",lat1," and ",lat2,") and (a.longitude between ",lon1," and ",lon2,") and (b.day between ",juldaystart," and ",juldayfinish,")
  order by b.day",sep="")
    }else{
      if(i==1){ # doing first year, start at day requested
        query<-paste("SELECT a.latitude, a.longitude, b.*
  FROM [AWAPDaily].[dbo].[latlon] as a
  , [AWAPDaily].[dbo].[",years[i],"] as b
  where (a.id = b.id) and (a.latitude between ",lat1," and ",lat2,") and (a.longitude between ",lon1," and ",lon2,") and (b.day >= ",juldaystart,")
  order by b.day",sep="")
      }else{
        if(i==length(years)){ # doing last year, only go up to last day requested
          query<-paste("SELECT a.latitude, a.longitude, b.*
  FROM [AWAPDaily].[dbo].[latlon] as a
  , [AWAPDaily].[dbo].[",years[i],"] as b
  where (a.id = b.id) and (a.latitude between ",lat1," and ",lat2,") and (a.longitude between ",lon1," and ",lon2,") and (b.day <= ",juldayfinish,")
  order by b.day",sep="")
        }else{ # doing in between years, so get all data for this year
          query<-paste("SELECT a.latitude, a.longitude, b.*
  FROM [AWAPDaily].[dbo].[latlon] as a
  , [AWAPDaily].[dbo].[",years[i],"] as b
  where (a.id = b.id) and (a.latitude between ",lat1," and ",lat2,") and (a.longitude between ",lon1," and ",lon2,")
  order by b.day",sep="")
        }}}
    
    
    # exectue query and concatenate if necessary
    if(i==1){
      output<- sqlQuery(channel,query)
    }else{
      output<-rbind(output,sqlQuery(channel,query))
    }
  } # end loop through years
  output$sol<-as.numeric(as.character(output$sol))
  # plot data
  dates<-seq(datestart,datefinish,"days", tzone = "UTC")
  output<-cbind(dates,output)
  return(output)
}