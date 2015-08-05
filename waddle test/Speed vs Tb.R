
waddlefiles<-read.csv("c:/NicheMapR_Working/projects/Sleepy Lizards/waddle_files_all.csv")
sex<-read.csv("c:/NicheMapR_Working/projects/Sleepy Lizards/Waddle/sex.csv")

threshold.act<-50
k<-0
# use individual 9 (burrows) or 115 (dam)
for(i in 1:121){
  
  sleepy_id<-waddlefiles[i,2]
  sexliz<-subset(sex,Liz==sleepy_id)
  sexliz<-sexliz[2]
  sexlizard<-as.character(sexliz)
  if(sexlizard=="integer(0)"){
    sexlizard<-'unknown'
  }
  if(sexlizard==2){
    sexlizard<-'F'
  }
  if(sexlizard==3){
    sexlizard<-'M'
  }
  
  
  #sleepy<-as.data.frame(read.table(file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',sleepy_id,'_2009_ALL.csv',sep=""), sep = ",", head=TRUE))
  sleepy<-as.data.frame(read.table(file = paste('c:/NicheMapR_Working/projects/sleepy lizards/waddle/',waddlefiles[i,3],sep=""), sep = ",", head=TRUE, stringsAsFactors = FALSE))
  if(is.null(sleepy$Hours)){
    Hours<-as.numeric(substr(sleepy$Time,1,2))
    Minutes<-as.numeric(substr(sleepy$Time,4,5))
    sleepy<-as.data.frame(cbind(sleepy,Hours,Minutes))
    write.table(sleepy,file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',waddlefiles[i,3],sep=""),row.names=F,sep=",") 
  }
  curyear<-max(sleepy$Year,na.rm=TRUE)
 
    date_waddle1<-with(sleepy,ISOdatetime(Year,Month,Day,Hours,Minutes,0))
    date_waddle<-date_waddle1+60*60 # adjust for Central Time
    doy<-strptime(format(date_waddle, "%y/%m/%d"), "%y/%m/%d")$yday+1
    #doy2<-strptime(format(plotrainfall$dates, "%y/%m/%d"), "%y/%m/%d")$yday+1
    sleepy<-cbind(date_waddle,doy,sleepy)
    #plotrainfall2<-cbind(doy2,plotrainfall)
    #colnames(plotrainfall2)<-c("JULDAY","dates","RAINFALL")
 
    date_waddle<-aggregate(sleepy$date_waddle,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),min)
    doy<-aggregate(sleepy$doy,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    Tb<-aggregate(sleepy$Temperature,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),mean)
    steps<-aggregate(sleepy$Steps,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),sum)
    year<-aggregate(sleepy$Year,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    month<-aggregate(sleepy$Month,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    day<-aggregate(sleepy$Day,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    hours<-aggregate(sleepy$Hours,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    sleepy<-as.data.frame(cbind(as.data.frame(date_waddle[,2]),doy[,2],Tb[,2],steps[,2],year[,2],month[,2],day[,2],hours[,2]))
    colnames(sleepy)<-c('date_waddle','doy','Temperature','Steps','Year','Month','Day','Hours')
    
    
    
    # round the times to get rid of occasional 1 min addons
    x<-sleepy$date_waddle
    r <-  60*60
    
    H <- as.integer(format(x, "%H"))
    M <- as.integer(format(x, "%M"))
    S <- as.integer(format(x, "%S"))
    D <- format(x, "%Y-%m-%d")
    secs <- 3600*H + 60*M + S
    x<-as.POSIXct(round(secs/r)*r, origin=D)-11*3600
    sleepy$date_waddle<-x
    # end rounding times
    
    sleepy$RSL<-0.0102*sleepy$Steps/60+2.282 # relative stride length
    HLL<-60 # hind limb length, mm for a 290 mm individual measured by Dale Burzacott on 4/8/2015
    sleepy$speed<-(sleepy$RSL*(sleepy$Steps/60)*HLL)/1000 # sleepy speed in km/h - using formula in Kerr et al 2004 for distance as a function of relative stride length, stride frequency and hind limb length
    sleepy2<-as.data.frame(as.data.frame(cbind(date_waddle[,2],doy[,2],Tb[,2],steps[,2],year[,2],month[,2],day[,2],hours[,2],date_waddle$Group.1)))
    colnames(sleepy2)<-c('date_waddle','doy','Temperature','Steps','Year','Month','Day','Hours','merge')
    
    
    sleepy2$Temperature<-as.factor(sleepy$Temperature)
    
    
    if(k==1){
    with(sleepy,plot(speed~Temperature,type='p',cex=.3,xlim=c(5,45),ylim=c(0,10),ylab="speed (m/h)",xlab='body temperature (deg C)'))
    }else{
      with(sleepy,points(speed~Temperature,type='p',cex=.3,xlim=c(0,45),ylim=c(0,3.5),ylab="m/h"))
    }
    #title(main=paste(sleepy_id," sex=",sexliz$Sex," ",curyear,sep=" "))
    

} #end loop through lizards

Pamula<-read.csv('waddle test/Pamula Field Tb.csv')
Pamula$mean<-(Pamula$F+Pamula$M)/2
points(Pamula$mean/max(Pamula$mean)*2.0~Pamula$Tb,type='h',lwd=3,col='red',xlim=c(0,45),ylim=c(0,3.5))
