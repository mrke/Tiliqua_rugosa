# analyse mass change in 2009 during drought period

# first with all lizards

# get mass data for 2009 (has dry period column of 0 or 1)
massdata <- read.csv('waddleometer/Church_site_mass_length.csv')
massdata2009 <- subset(massdata, year==2009)

# aggregate mass data per ID and per dry period
massagg <- aggregate(massdata2009$wgt, by = list(paste(massdata2009$lizard_ID, massdata2009$dryperiod, sep = "_")), mean)
massdry <- aggregate(massdata2009$dryperiod, by = list(paste(massdata2009$lizard_ID, massdata2009$dryperiod, sep = "_")), mean)
massid <- aggregate(massdata2009$lizard_ID, by = list(paste(massdata2009$lizard_ID, massdata2009$dryperiod, sep = "_")), mean)
massagg_all <- as.data.frame(cbind(massdry$x, massid$x, massagg$x))
colnames(massagg_all) <- c('dry', 'id', 'mass')

# separate into dry and wet periods and merge to have each individual per row, with two observations of mass
massagg1 <- subset(massagg_all, dry == 1)
massagg0 <- subset(massagg_all, dry == 0)
massagg_merge <- merge(massagg1, massagg0, by = 'id')

# get the difference between the masses, express as percentage change
diff <- massagg_merge$mass.x - massagg_merge$mass.y
pctdes <- (diff* - 1) / massagg_merge$mass.y * 100
hist(pctdes)
pctdesic <- subset(pctdes, pctdes > 0)
mean(pctdesic, na.rm = TRUE)
mean(pctdes, na.rm = TRUE)
mean(diff, na.rm = TRUE)

# merge into a single table, do a paired t-test
massagg_merge <- cbind(massagg_merge, diff, pctdes)
with(massagg_merge, {t.test(mass.x, mass.y, paired = TRUE)})

# plot the differences
s <- seq(length(massagg_merge$mass.x))
par(bty = "l")
boxplot(massagg_merge$mass.x,massagg_merge$mass.y,xlab = "Time",ylab = "Measure",names = c("desiccated","hydrated"),col = c("lightblue","lightgreen"),ylim = c(400,1000))
stripchart(list(massagg_merge$mass.x,massagg_merge$mass.y),vertical = T,pch = 16,cex = 0.5,add = T)
segments(rep(0.95,length(massagg_merge$mass.x))[s],massagg_merge$mass.x[s],rep(2,length(massagg_merge$mass.x))[s],massagg_merge$mass.y[s],col = 'light grey',lwd = 0.5)

# second, repeat excluding animals that remained active
activelizards <- c(9532,9372,9364,9363,9310,40044,40012,12847,12434,11885,11505,10509)
massdata2009 <- subset(massdata, year==2009 & !(lizard_ID%in%activelizards))
massagg <- aggregate(massdata2009$wgt, by = list(paste(massdata2009$lizard_ID,massdata2009$dryperiod,sep = "_")),mean)
massdry <- aggregate(massdata2009$dryperiod, by = list(paste(massdata2009$lizard_ID,massdata2009$dryperiod,sep = "_")),mean)
massid <- aggregate(massdata2009$lizard_ID, by = list(paste(massdata2009$lizard_ID,massdata2009$dryperiod,sep = "_")),mean)
massagg_all <- as.data.frame(cbind(massdry$x,massid$x,massagg$x))
colnames(massagg_all) <- c('dry','id','mass')
massagg1 <- subset(massagg_all,dry==1)
massagg0 <- subset(massagg_all,dry==0)
massagg_merge <- merge(massagg1,massagg0,by = 'id')
diff <- massagg_merge$mass.x-massagg_merge$mass.y
pctdes <- (diff*-1)/massagg_merge$mass.y*100
mean(pctdesic, na.rm = TRUE)
mean(pctdes, na.rm = TRUE)
mean(diff, na.rm = TRUE)
hist(pctdes)
hist(diff)
pctdesic <- subset(pctdes,pctdes>0)
mean(pctdesic,na.rm = TRUE)
massagg_merge <- cbind(massagg_merge,diff,pctdes)
with(massagg_merge,{t.test(mass.x,mass.y,paired = TRUE)})

s <- seq(length(massagg_merge$mass.x))
par(bty = "l")
boxplot(massagg_merge$mass.x,massagg_merge$mass.y,xlab = "Time",ylab = "Measure",names = c("desiccated","hydrated"),col = c("lightblue","lightgreen"),ylim = c(400,1000))
stripchart(list(massagg_merge$mass.x,massagg_merge$mass.y),vertical = T,pch = 16,cex = 0.5,add = T)
segments(rep(0.95,length(massagg_merge$mass.x))[s],massagg_merge$mass.x[s],rep(2,length(massagg_merge$mass.x))[s],massagg_merge$mass.y[s],col = 'light grey',lwd = 0.5)

# third, only including animals that remained active
massdata2009 <- subset(massdata,year==2009 & lizard_ID%in%activelizards)
massagg <- aggregate(massdata2009$wgt,by = list(paste(massdata2009$lizard_ID,massdata2009$dryperiod,sep = "_")),mean)
massdry <- aggregate(massdata2009$dryperiod,by = list(paste(massdata2009$lizard_ID,massdata2009$dryperiod,sep = "_")),mean)
massid <- aggregate(massdata2009$lizard_ID,by = list(paste(massdata2009$lizard_ID,massdata2009$dryperiod,sep = "_")),mean)
massagg_all <- as.data.frame(cbind(massdry$x,massid$x,massagg$x))
colnames(massagg_all) = c('dry','id','mass')
massagg1 <- subset(massagg_all,dry==1)
massagg0 <- subset(massagg_all,dry==0)
massagg_merge <- merge(massagg1,massagg0,by = 'id')
diff <- massagg_merge$mass.x-massagg_merge$mass.y
pctdes <- (diff*-1)/massagg_merge$mass.y*100
hist(pctdes)
pctdesic <- subset(pctdes,pctdes>0)
mean(pctdesic,na.rm = TRUE)
mean(pctdes,na.rm = TRUE)
massagg_merge <- cbind(massagg_merge,diff,pctdes)
with(massagg_merge,{t.test(mass.x,mass.y,paired = TRUE)})

s <- seq(length(massagg_merge$mass.x))
par(bty = "l")
boxplot(massagg_merge$mass.x,massagg_merge$mass.y,xlab = "Time",ylab = "Measure",names = c("desiccated","hydrated"),col = c("lightblue","lightgreen"),ylim = c(400,1000))
stripchart(list(massagg_merge$mass.x,massagg_merge$mass.y),vertical = T,pch = 16,cex = 0.5,add = T)
segments(rep(0.95,length(massagg_merge$mass.x))[s],massagg_merge$mass.x[s],rep(2,length(massagg_merge$mass.x))[s],massagg_merge$mass.y[s],col = 'light grey',lwd = 0.5)
