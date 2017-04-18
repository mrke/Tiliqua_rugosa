plot_growth <- function(lizard = lizard, debout = debout, debout_drk = debout_drk, dates = dates, dates_drk = dates_drk, limx = limx, dates3 = dates3, foodwaters3 = foodwaters3, RAINFALL3 = RAINFALL3){
  
  hydration_mass_male <- debout$WETMASS - (debout$CUMREPRO + debout$CUMBATCH) / mu_E * 23.9 / 0.3 - debout$WETMASS * (debout$Body_cond / 100)
  hydration_mass_female <- debout$WETMASS - debout$WETMASS * (debout$Body_cond / 100)
  hydration_mass_male_drk <- debout_drk$WETMASS - (debout_drk$CUMREPRO + debout_drk$CUMBATCH) / mu_E * 23.9 / 0.3 - debout_drk$WETMASS * (debout_drk$Body_cond / 100)
  hydration_mass_female_drk <- debout_drk$WETMASS - debout_drk$WETMASS * (debout_drk$Body_cond / 100)

  liz <- subset(sleepydata, LIZN == lizard)
  if(limx == 1){
    xlims <- c(min(liz$date, na.rm = TRUE), max(liz$date, na.rm = TRUE))
  }else{
    xlims <- c(min(dates, na.rm = TRUE), max(dates, na.rm = TRUE))
  }
  if("M" %in% liz$SEX){
    hydration_mass <- hydration_mass_male
    hydration_mass_drk <- hydration_mass_male_drk
    sex = "male"
  }else{
    hydration_mass <- hydration_mass_female
    hydration_mass_drk <- hydration_mass_female_drk
    sex = "female"
  }
  wgtlims1 <- c(min(subset(hydration_mass, dates >= xlims[1] & dates <= xlims[2]), na.rm = TRUE), max(subset(hydration_mass_drk, dates >= xlims[1] & dates <= xlims[2]), na.rm = TRUE))
  wgtlims2 <- c(min(liz$WGT, na.rm = TRUE), max(liz$WGT, na.rm = TRUE))
  
  lenlims1 <- c(min(subset(debout$SVL, dates >= xlims[1] & dates <= xlims[2]), na.rm = TRUE), max(subset(debout_drk$SVL, dates >= xlims[1] & dates <= xlims[2]), na.rm = TRUE)) / 10
  lenlims2 <- c(min(liz$LGTH, na.rm = TRUE), max(liz$LGTH, na.rm = TRUE))
  if(limx == 1){
    wgtlims <- c(min(wgtlims1, wgtlims2) * 0.9, max(wgtlims1, wgtlims2) * 1.1)
    lenlims <- c(min(lenlims1, lenlims2) * 0.9, max(lenlims1, lenlims2) * 1.1)
  }else{
    wgtlims <- c(0, 1000)
    lenlims <- c(10, 35)
  } 
  if(limx == 1){
    par(mfrow = c(2, 2)) # set up for 2 plots in 2 columns
    par(oma = c(3, 2, 2, 2) + 0.1) # margin spacing stuff
    par(mar = c(4, 5, 1, 1) + 0.1) # margin spacing stuff
    par(mgp = c(3, 1, 0) ) # margin spacing stuff
  }
  year_vals <- subset(debout, as.character(dates, format = '%d/%m') == "01/01")
  year_vals <- subset(year_vals, as.character(year_vals$dates, format = '%H') == "00") # get midnight
  RAINFALL3[RAINFALL3 < 3.5] <- 0
  RAINFALL3[RAINFALL3 > 0] <- 1
  plot(hydration_mass ~ dates, type = "l", xlab = "year", ylab = "wet mass (g)", col = 'black', ylim = wgtlims, xlim = xlims, cex = 1.25, cex.axis = 1.5, cex.lab = 1.5, yaxs = "i", xaxs = "i")
  if(limx == 0){
    points(RAINFALL3 * 200 ~ subset(dates, format(dates, "%H") == "12"), type = 'h', col = 'light blue')  #title(main = paste0("weight vs. time, lizard #", lizard, " ", sex), cex = 1.5)
    points(foodwaters3 * 200 ~ subset(dates, format(dates, "%H") == "12"), type = 'h', col = 'dark green')  #title(main = paste0("weight vs. time, lizard #", lizard, " ", sex), cex = 1.5)
  }
  points(hydration_mass ~ dates, type = "l", xlab = "year", ylab = "wet mass (g)", col = 'black', ylim = wgtlims, xlim = xlims, cex = 1.25, cex.axis = 1.5, cex.lab = 1.5, yaxs = "i", xaxs = "i")
  abline(v = year_vals$dates, col = 'grey',lty = 2) # add lines to show beginning of each year
  points(hydration_mass_drk ~ dates_drk, type = "l", xlab = "year", ylab = "wet mass (g)", col = 'grey', ylim = wgtlims, xlim = xlims, cex = 1.25, cex.axis = 1.5, cex.lab = 1.5)
  with(liz,points(WGT~date,col='red', pch=16, cex = 1.25, cex.axis = 1.5, cex.lab = 1.5))
  box()
  with(debout, {plot(SVL / 10 ~ dates,type = "l",xlab = "year",ylab = "snout vent length (mm)",col='black',ylim = lenlims, xlim = xlims, cex = 1.25, cex.axis = 1.5, cex.lab = 1.5, yaxs = "i", xaxs = "i")})
  if(limx == 0){
    points(RAINFALL3 * 5 + 10 ~ subset(dates, format(dates, "%H") == "12"), type = 'h', col = 'light blue')  #title(main = paste0("weight vs. time, lizard #", lizard, " ", sex), cex = 1.5)
    points(foodwaters3 * 5 + 10 ~ subset(dates, format(dates, "%H") == "12"), type = 'h', col = 'dark green')  #title(main = paste0("weight vs. time, lizard #", lizard, " ", sex), cex = 1.5)
  } 
  with(debout, {points(SVL / 10 ~ dates,type = "l",xlab = "year",ylab = "snout vent length (mm)",col='black',ylim = lenlims, xlim = xlims, cex = 1.25, cex.axis = 1.5, cex.lab = 1.5, yaxs = "i", xaxs = "i")})
  abline(v = year_vals$dates, col = 'grey',lty = 2) # add lines to show beginning of each year
  with(debout_drk, {points(SVL / 10 ~ dates_drk,type = "l",xlab = "year",ylab = "snout vent length (mm)",col='grey',ylim = lenlims, xlim = xlims, cex = 1.25, cex.axis = 1.5, cex.lab = 1.5)})
  with(liz,points(LGTH~date,col='red', pch=16, cex = 1.25, cex.axis = 1.5, cex.lab = 1.5))
  box()
  #title(main = paste0("length vs. time, lizard #", lizard, " ", sex), cex = 1.5)
  
  dailymass <- aggregate(cbind(hydration_mass, debout$SVL), by = list(format(debout$dates, "%Y-%m-%d")), FUN = mean)
  lizagg <- aggregate(liz[,14:15], by = list(format(liz$date, "%Y-%m-%d")), FUN = mean)
  correl <- cbind(lizagg, dailymass[which(dailymass$Group.1 %in% as.character(liz$date)),2:3])
  colnames(correl)[4:5] <- c("predMass", "predSVL")
  correl$predSVL <- correl$predSVL / 10
  
  dailymass_drk <- aggregate(cbind(hydration_mass_drk, debout_drk$SVL), by = list(format(debout_drk$dates_drk, "%Y-%m-%d")), FUN = mean)
  lizagg_drk <- aggregate(liz[,14:15], by = list(format(liz$date, "%Y-%m-%d")), FUN = mean)
  correl_drk <- cbind(lizagg_drk, dailymass_drk[which(dailymass_drk$Group.1 %in% as.character(liz$date)),2:3])
  colnames(correl_drk)[4:5] <- c("predMass", "predSVL")
  correl_drk$predSVL <- correl_drk$predSVL / 10
  
  # if(limx == 1){
  #   par(mfrow = c(2, 1)) # set up for 2 plots in 2 columns
  #   par(oma = c(3, 2, 2, 2) + 0.1) # margin spacing stuff
  #   par(mar = c(4, 5, 1, 1) + 0.1) # margin spacing stuff 
  #   par(mgp = c(3, 1, 0) ) # margin spacing stuff
  # }
  
  # plot
  if(limx == 1){  
    x <- 100:1400
    plot(x,x, type = 'l', ylab = "observed mass, g", xlab = "predicted mass, g", ylim = c(100,1400), cex = 1.25, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
    points(correl$WGT ~ correl$predMass, col = "black", pch = 16, cex = 1.25, cex.axis = 1.5, cex.lab = 1.5)
    points(correl_drk$WGT ~ correl_drk$predMass, col = "grey", pch = 16, cex = 1.25, cex.axis = 1.5, cex.lab = 1.5)
    
    # compute correlation coefficient and root mean square deviation
    r <- cor(correl$WGT, correl$predMass, use = 'pairwise')
    p <- cor.test(correl$WGT, correl$predMass, use = 'pairwise')$p.value
    rmsd <- sqrt(mean(((correl$WGT-correl$predMass) ^ 2),na.rm = TRUE))
    r_drk <- cor(correl_drk$WGT, correl_drk$predMass, use = 'pairwise')
    p_drk <- cor.test(correl_drk$WGT, correl_drk$predMass, use = 'pairwise')$p.value
    rmsd_drk <- sqrt(mean(((correl_drk$WGT-correl_drk$predMass) ^ 2),na.rm = TRUE))
    if(limx == 1){
      write.table(t(c(lizard, sex, r, rmsd, p, r_drk, rmsd_drk, p_drk)), sep = ',', file = 'correl_field_mass.csv', col.names = F, append = TRUE)
    }
    # append to plot
    text(350 + 9.5 + 60, 1350, "    r     rmsd    p", cex = 1.75)
    text(330 + 30.5 + 60, 1250, paste0(round(r, 3),'   ',round(rmsd, 2),'   ',round(p, 4)), cex = 1.75)
    text(330 + 30.5 + 60, 1150, paste0(round(r_drk, 3),'   ',round(rmsd_drk, 2),'   ',round(p_drk, 4)), cex = 1.75, col = 'grey')
    
    # plot
    x <- 10:35
    plot(x,x, type = 'l', ylab = "observed SVL, cm", xlab = "predicted svl, cm", ylim = c(10,35), cex = 1.25, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
    points(correl$LGTH ~ correl$predSVL, col = "black", pch = 16, cex = 1.25, cex.axis = 1.5, cex.lab = 1.5)
    points(correl_drk$LGTH ~ correl_drk$predSVL, col = "grey", pch = 16, cex = 1.25, cex.axis = 1.5, cex.lab = 1.5)
    
    # compute correlation coefficient and root mean square deviation
    r <- cor(correl$LGTH, correl$predSVL, use = 'pairwise')
    rmsd <- sqrt(mean(((correl$LGTH-correl$predSVL) ^ 2),na.rm = TRUE))
    p <- cor.test(correl$LGTH, correl$predSVL, use = 'pairwise')$p.value
    r_drk <- cor(correl_drk$LGTH, correl_drk$predSVL, use = 'pairwise')
    rmsd_drk <- sqrt(mean(((correl_drk$LGTH-correl_drk$predSVL) ^ 2),na.rm = TRUE))
    p_drk <- cor.test(correl_drk$LGTH, correl_drk$predSVL, use = 'pairwise')$p.value
    if(limx == 1){
      write.table(t(c(lizard, sex, r, rmsd, p, r_drk, rmsd_drk, p_drk)), sep = ',', file = 'correl_field_length.csv', col.names = F, append = TRUE)
    }
    # append to plot
    text(11 + 4,34, "     r     rmsd    p", cex = 1.75)
    text(11 + 4.5,32, paste0(round(r, 3),'   ',round(rmsd, 2),'   ',round(p, 4)), cex = 1.75)
    text(11 + 4.5,30, paste0(round(r_drk, 3),'   ',round(rmsd_drk, 2),'   ',round(p_drk, 4)), cex = 1.75, col = 'grey')
  }
}