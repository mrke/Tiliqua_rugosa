plot_components_cv <- function(scenario, filename){

pdf(filename,paper="A4r",width=15,height=4.5) # doing this means you're going to make a pdf - comment this line out if you want to see them in R
  
par(mfrow = c(2, 5))
par(oma = c(0, 0, 0, 3) + 0.1) # margin spacing stuff
par(mar = c(1, 1, 1, 3)) # margin spacing stuff 
par(mgp = c(3, 1, 0) ) # margin spacing stuff 

scen <- stack(Bsks[[scenario]]) + stack(Forages[[scenario]])
brks <- seq(0, 2, .2)
colfunc <- colorRampPalette(c("white", "orange", "red"))
plot(calc(scen, sd)/calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = FALSE, breaks = brks, col = colfunc(length(brks)-1), zlim = c(min(brks), max(brks)), main = "Activity (h)")
plot(aust_bound, col="black", lwd = 1.0, add = TRUE) 
plot(sleepy_bound, col="black", lwd = 2, add = TRUE, lty = 1) 
plot(calc(scen, sd)/calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, breaks = brks, col = colfunc(length(brks)-1), zlim = c(min(brks), max(brks)), main = "Activity (h)", legend.only=TRUE, legend.width=2, legend.shrink=0.6, legend.args=list(text = "",side=4, font=2, line=2.5, cex=0.8))

scen <- stack(MaxDeps[[scenario]])
brks <- seq(0, 1, 0.2)
colfunc <- colorRampPalette(c("white", "orange", "red"))
plot(calc(scen, sd)/calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = FALSE, breaks = brks, col = colfunc(length(brks)-1), zlim = c(min(brks), max(brks)), main = "Depth (cm)")
plot(aust_bound, col="black", lwd = 1.0, add = TRUE) 
plot(sleepy_bound, col="black", lwd = 1.0, add = TRUE) 
plot(sleepy_bound, col="black", lwd = 2, add = TRUE, lty = 1) 
plot(calc(scen, sd)/calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = TRUE, breaks = brks, col = colfunc(length(brks)-1), zlim = c(min(brks), max(brks)), main = "Depth (cm)", legend.only=TRUE, legend.width=2, legend.shrink=0.6, legend.args=list(text = "",side=4, font=2, line=2.5, cex=0.8))

scen <- stack(MaxDesics[[scenario]])
brks <- seq(0, 0.6, 0.05)
colfunc <- colorRampPalette(c("white", "orange", "red"))
plot(calc(scen, sd)/calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = FALSE, breaks = brks, col = colfunc(length(brks)-1), zlim = c(min(brks), max(brks)), main = "Desiccation (%)")
plot(aust_bound, col="black", lwd = 1.0, add = TRUE) 
plot(sleepy_bound, col="black", lwd = 2, add = TRUE, lty = 1) 
plot(calc(scen, sd)/calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = TRUE, breaks = brks, col = colfunc(length(brks)-1), zlim = c(min(brks), max(brks)), main = "Desiccation (%)", legend.only=TRUE, legend.width=2, legend.shrink=0.6, legend.args=list(text = "",side=4, font=2, line=2.5, cex=0.8))

debpars=as.data.frame(read.csv('DEB model/DEB_pars_Tiliqua_rugosa.csv',header=FALSE))$V1 # read in DEB pars
E_m <- ((debpars[16]/24)*debpars[8]/debpars[14])/(debpars[13]/24)
scen <- stack(MINRESERVES[[scenario]]) / E_m * 100
brks <- seq(0, 1.3, 0.15)
colfunc <- colorRampPalette(c("white", "orange", "red"))
plot(calc(scen, sd)/calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = FALSE, breaks = brks, col = colfunc(length(brks)-1), zlim = c(min(brks), max(brks)), main = "Starvation (%)")
plot(aust_bound, col="black", lwd = 1.0, add = TRUE) 
plot(sleepy_bound, col="black", lwd = 2, add = TRUE, lty = 1) 
plot(calc(scen, sd)/calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = TRUE, breaks = brks, col = colfunc(length(brks)-1), zlim = c(min(brks), max(brks)), main = "Starvation (%)", legend.only=TRUE, legend.width=2, legend.shrink=0.6, legend.args=list(text = "",side=4, font=2, line=2.5, cex=0.8))

scen <- stack(SVLs[[scenario]]) 
#scen[scen < 270] <- 260
brks <- seq(0, 4.5, 0.5)
colfunc <- colorRampPalette(c("white", "orange", "red"))
plot(calc(scen, sd)/calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = FALSE, breaks = brks, col = colfunc(length(brks)-1), zlim = c(min(brks), max(brks)), main = "Length (mm)")
plot(aust_bound, col="black", lwd = 1.0, add = TRUE) 
plot(sleepy_bound, col="black", lwd = 2, add = TRUE, lty = 1) 
plot(calc(scen, sd)/calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = TRUE, breaks = brks, col = colfunc(length(brks)-1), zlim = c(min(brks), max(brks)), main = "Length (mm)", legend.only=TRUE, legend.width=2, legend.shrink=0.6, legend.args=list(text = "",side=4, font=2, line=2.5, cex=0.8))

scen <- stack(MaxWgts[[scenario]])/1000
scen[scen <- 0.1] <- 0
brks <- seq(0, 1.1, 0.1)
colfunc <- colorRampPalette(c("white", "orange", "red"))
plot(calc(scen, sd)/calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = FALSE, breaks = brks, col = colfunc(length(brks)-1), zlim = c(min(brks), max(brks)), main = "Weight (kg)")
plot(aust_bound, col="black", lwd = 1.0, add = TRUE) 
plot(sleepy_bound, col="black", lwd = 2, add = TRUE, lty = 1) 
# plot(dingo1, col="white", lwd = 2.0, add = TRUE) 
# plot(dingo3, col="white", lwd = 2.0, add = TRUE) 
plot(calc(scen, sd)/calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = TRUE, breaks = brks, col = colfunc(length(brks)-1), zlim = c(min(brks), max(brks)), main = "Weight (kg)", legend.only=TRUE, legend.width=2, legend.shrink=0.6, legend.args=list(text = "",side=4, font=2, line=2.5, cex=0.8))

scen <- stack(yrRepros[[scenario]])
brks <- seq(0, 4.2, 0.5)
colfunc <- colorRampPalette(c("white", "orange", "red"))
plot(calc(scen, sd)/calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = FALSE, breaks = brks, col = colfunc(length(brks)-1), zlim = c(min(brks), max(brks)), main = "1st Reproduction (yr)")
plot(aust_bound, col="black", lwd = 1.0, add = TRUE) 
plot(sleepy_bound, col="black", lwd = 2, add = TRUE, lty = 1) 
plot(calc(scen, sd)/calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = TRUE, breaks = brks, col = colfunc(length(brks)-1), zlim = c(min(brks), max(brks)), main = "1st Reproduction (yr)", legend.only=TRUE, legend.width=2, legend.shrink=0.6, legend.args=list(text = "",side=4, font=2, line=2.5, cex=0.8))

library(data.table)
scen <- stack(tLays[[scenario]])
scen2 <- scen
vals_orig <- seq(1, 20)
for(i in 1:20){
vals <- shift(vals_orig, n = i-1, type = "lead", fill=TRUE)
    mergedata <- paste0("merge(scen2[[",vals[1],"]],scen[[",vals[2],"]],scen[[",vals[3],"]],scen[[",vals[4],"]],scen[[",vals[5],"]],scen[[",vals[6],"]],scen[[",vals[7],"]],scen[[",vals[8],"]],scen[[",vals[9],"]],scen[[",vals[10],"]],scen[[",vals[11],"]],scen[[",vals[12],"]],scen[[",vals[13],"]],scen[[",vals[14],"]],scen[[",vals[15],"]],scen[[",vals[16],"]],scen[[",vals[17],"]],scen[[",vals[18],"]],scen[[",vals[19],"]],scen[[",vals[20],"]])")
    scen2[[i]] <- eval(parse(text=mergedata))
}

brks <- seq(0, 1.6, 0.2)
colfunc <- colorRampPalette(c("white", "orange", "red"))
plot(calc(scen2, sd)/calc(scen2, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = FALSE, breaks = brks, col = colfunc(length(brks)-1), zlim = c(min(brks), max(brks)), main = "Parturition (month)")
plot(aust_bound, col="black", lwd = 1.0, add = TRUE) 
plot(sleepy_bound, col="black", lwd = 2, add = TRUE, lty = 1) 
plot(calc(scen, sd)/calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = TRUE, breaks = brks, col = colfunc(length(brks)-1), zlim = c(min(brks), max(brks)), main = "Parturition (month)", legend.only=TRUE, legend.width=2, legend.shrink=0.6, legend.args=list(text = "",side=4, font=2, line=2.5, cex=0.8))

scen <- stack(Fecs[[scenario]])
brks <- seq(0, 4.5, 0.5)
colfunc <- colorRampPalette(c("white", "orange", "red"))
plot(calc(scen, sd)/calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = FALSE, breaks = brks, col = colfunc(length(brks)-1), zlim = c(min(brks), max(brks)), main = "Fecundity (#)")
plot(aust_bound, col="black", lwd = 1.0, add = TRUE) 
plot(sleepy_bound, col="black", lwd = 2, add = TRUE, lty = 1) 
plot(calc(scen, sd)/calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = TRUE, breaks = brks, col = colfunc(length(brks)-1), zlim = c(min(brks), max(brks)), main = "Fecundity (#)", legend.only=TRUE, legend.width=2, legend.shrink=0.6, legend.args=list(text = "",side=4, font=2, line=2.5, cex=0.8))

scen <- stack(survivs[[scenario]])
brks <- seq(0, 0.05, 0.01)
colfunc <- colorRampPalette(c("white", "orange", "red"))
plot(calc(scen, sd)/calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = FALSE, breaks = brks, col = colfunc(length(brks)-1), zlim = c(min(brks), max(brks)), main = "Survival (prob)")
plot(aust_bound, col="black", lwd = 1.0, add = TRUE) 
plot(sleepy_bound, col="black", lwd = 2, add = TRUE, lty = 1) 
plot(calc(scen, sd)/calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = TRUE, breaks = brks, col = colfunc(length(brks)-1), zlim = c(min(brks), max(brks)), main = "Survival (prob)", legend.only=TRUE, legend.width=2, legend.shrink=0.6, legend.args=list(text = "",side=4, font=2, line=2.5, cex=0.8))

dev.off()
}