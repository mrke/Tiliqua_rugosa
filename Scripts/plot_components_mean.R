plot_components_mean <- function(scenario, filename){
pdf(filename,paper="A4r",width=15,height=4.5) # doing this means you're going to make a pdf - comment this line out if you want to see them in R

par(mfrow = c(2, 5))
par(oma = c(0, 0, 0, 3) + 0.1) # margin spacing stuff
par(mar = c(1, 1, 1, 3)) # margin spacing stuff 
par(mgp = c(3, 1, 0) ) # margin spacing stuff 

scen <- stack(Bsks[[scenario]]) + stack(Forages[[scenario]])
brks <- seq(0, 2200, 200)
colfunc <- colorRampPalette(c("blue", "light blue", "yellow", "orange", "red"))
plot(calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = FALSE, breaks = brks, col = colfunc(length(brks)-1), zlim = c(min(brks), max(brks)), main = "Activity (h)")
plot(aust_bound, col="black", lwd = 1.0, add = TRUE) 
plot(sleepy_bound, col="black", lwd = 2, add = TRUE, lty = 1) 
plot(calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, breaks = brks, col = colfunc(length(brks)-1), zlim = c(min(brks), max(brks)), main = "Activity (h)", legend.only=TRUE, legend.width=2, legend.shrink=0.6, legend.args=list(text = "",side=4, font=2, line=2.5, cex=0.8))

scen <- stack(MaxDeps[[scenario]])
scen[scen > 55 & scen < 65] <- 60
brks <- seq(0, 200, 20)
colfunc <- colorRampPalette(c("dark green", "light green", "yellow", "brown"))
plot(calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = FALSE, breaks = brks, col = colfunc(length(brks)-1), zlim = c(min(brks), max(brks)), main = "Depth (cm)")
plot(aust_bound, col="black", lwd = 1.0, add = TRUE) 
plot(sleepy_bound, col="black", lwd = 1.0, add = TRUE) 
plot(sleepy_bound, col="black", lwd = 2, add = TRUE, lty = 1) 
plot(calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = TRUE, breaks = brks, col = colfunc(length(brks)-1), zlim = c(min(brks), max(brks)), main = "Depth (cm)", legend.only=TRUE, legend.width=2, legend.shrink=0.6, legend.args=list(text = "",side=4, font=2, line=2.5, cex=0.8))

scen <- stack(MaxDesics[[scenario]])
brks <- seq(0, 40, 5)
colfunc <- colorRampPalette(c("light blue", "orange", "brown"))
plot(calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = FALSE, breaks = brks, col = c(colfunc(length(brks)-2), 'black'), zlim = c(min(brks), max(brks)), main = "Desiccation (%)")
plot(aust_bound, col="black", lwd = 1.0, add = TRUE) 
plot(sleepy_bound, col="black", lwd = 2, add = TRUE, lty = 1) 
plot(calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = TRUE, breaks = brks, col = c(colfunc(length(brks)-2), 'black'), zlim = c(min(brks), max(brks)), main = "Desiccation (%)", legend.only=TRUE, legend.width=2, legend.shrink=0.6, legend.args=list(text = "",side=4, font=2, line=2.5, cex=0.8))

debpars=as.data.frame(read.csv('DEB model/DEB_pars_Tiliqua_rugosa.csv',header=FALSE))$V1 # read in DEB pars
E_m <- ((debpars[16]/24)*debpars[8]/debpars[14])/(debpars[13]/24)
scen <- (1 - stack(MINRESERVES[[scenario]]) / E_m) * 100
scen[scen < 0] <- 0
brks <- seq(0, 100, 10)
colfunc <- colorRampPalette(c("dark green", "light green", "yellow", "brown"))
plot(calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = FALSE, breaks = brks, col = colfunc(length(brks)-1), zlim = c(min(brks), max(brks)), main = "Starvation (%)")
plot(aust_bound, col="black", lwd = 1.0, add = TRUE) 
plot(sleepy_bound, col="black", lwd = 2, add = TRUE, lty = 1) 
plot(calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = TRUE, breaks = brks, col = colfunc(length(brks)-1), zlim = c(min(brks), max(brks)), main = "Starvation (%)", legend.only=TRUE, legend.width=2, legend.shrink=0.6, legend.args=list(text = "",side=4, font=2, line=2.5, cex=0.8))

scen <- stack(SVLs[[scenario]]) 
scen[scen < 270] <- 260
brks <- seq(260, 340, 10)
colfunc <- colorRampPalette(c("brown", "yellow", "light green", "dark green"))
plot(calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = FALSE, breaks = brks, col = c("grey", colfunc(length(brks)-2)), zlim = c(min(brks), max(brks)), main = "Length (mm)")
plot(aust_bound, col="black", lwd = 1.0, add = TRUE) 
plot(sleepy_bound, col="black", lwd = 2, add = TRUE, lty = 1) 
plot(calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = TRUE, breaks = brks, col = c("grey", colfunc(length(brks)-2)), zlim = c(min(brks), max(brks)), main = "Length (mm)", legend.only=TRUE, legend.width=2, legend.shrink=0.6, legend.args=list(text = "",side=4, font=2, line=2.5, cex=0.8))

scen <- stack(MaxWgts[[scenario]])/1000
scen[scen <- 0.1] <- 0
brks <- seq(0, 2.2, 0.2)
scen[scen > 2.2] <- 2.2
colfunc <- colorRampPalette(c("brown", "yellow", "light green", "dark green"))
plot(calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = FALSE, breaks = brks, col = c("grey", colfunc(length(brks)-2)), zlim = c(min(brks), max(brks)), main = "Weight (kg)")
plot(aust_bound, col="black", lwd = 1.0, add = TRUE) 
plot(sleepy_bound, col="black", lwd = 2, add = TRUE, lty = 1) 
# plot(dingo1, col="white", lwd = 2.0, add = TRUE) 
# plot(dingo3, col="white", lwd = 2.0, add = TRUE) 
plot(calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = TRUE, breaks = brks, col = c("grey", colfunc(length(brks)-2)), zlim = c(min(brks), max(brks)), main = "Weight (kg)", legend.only=TRUE, legend.width=2, legend.shrink=0.6, legend.args=list(text = "",side=4, font=2, line=2.5, cex=0.8))

scen <- stack(yrRepros[[scenario]])
brks <- c(0, 1, 2, 3, seq(4, 16, 2))
colfunc <- colorRampPalette(c("red", "orange", "yellow", "light blue", "blue"))
plot(calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = FALSE, breaks = brks, col = c("grey", colfunc(length(brks)-2)), zlim = c(min(brks), max(brks)), main = "1st Reproduction (yr)")
plot(aust_bound, col="black", lwd = 1.0, add = TRUE) 
plot(sleepy_bound, col="black", lwd = 2, add = TRUE, lty = 1) 
plot(calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = TRUE, breaks = brks, col = c("grey", colfunc(length(brks)-2)), zlim = c(min(brks), max(brks)), main = "1st Reproduction (yr)", legend.only=TRUE, legend.width=2, legend.shrink=0.6, legend.args=list(text = "",side=4, font=2, line=2.5, cex=0.8))

scen <- stack(tLays[[scenario]])
brks <- seq(0, 13, 1)
cols <- c("grey", "firebrick3", "orange2", "orange1", "orange3", "dodgerblue2", "dodgerblue1", "dodgerblue3", "chartreuse2", "chartreuse1", "chartreuse3", "firebrick1", "firebrick2")
plot(calc(scen, mean, na.rm = TRUE), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = FALSE, breaks = brks, col = cols, zlim = c(min(brks), max(brks)), main = "Parturition (month)")
plot(aust_bound, col="black", lwd = 1.0, add = TRUE) 
plot(sleepy_bound, col="black", lwd = 2, add = TRUE, lty = 1) 
plot(calc(scen, mean, na.rm = TRUE), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = TRUE, breaks = brks, col = cols, zlim = c(min(brks), max(brks)), main = "Parturition (month)", legend.only=TRUE, legend.width=2, legend.shrink=0.6, legend.args=list(text = "",side=4, font=2, line=2.5, cex=0.8))

scen <- stack(Fecs[[scenario]])
brks <- c(0, 1, 2, seq(5, 45, 5))
scen[scen > 45] <- 45
colfunc <- colorRampPalette(c("brown", "yellow", "light green", "dark green"))
plot(calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = FALSE, breaks = brks, col = c("grey", colfunc(length(brks)-2)), zlim = c(min(brks), max(brks)), main = "Fecundity (#)")
plot(aust_bound, col="black", lwd = 1.0, add = TRUE) 
plot(sleepy_bound, col="black", lwd = 2, add = TRUE, lty = 1) 
plot(calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = TRUE, breaks = brks, col = c("grey", colfunc(length(brks)-2)), zlim = c(min(brks), max(brks)), main = "Fecundity (#)", legend.only=TRUE, legend.width=2, legend.shrink=0.6, legend.args=list(text = "",side=4, font=2, line=2.5, cex=0.8))

scen <- stack(survivs[[scenario]])
scen[scen > 1] <- 1
brks <- seq(0.8, 1, 0.02)
colfunc <- colorRampPalette(c("brown", "yellow", "light green", "dark green"))
plot(calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = FALSE, breaks = brks, col = c("grey", colfunc(length(brks)-2)), zlim = c(min(brks), max(brks)), main = "Survival (prob)")
plot(aust_bound, col="black", lwd = 1.0, add = TRUE) 
plot(sleepy_bound, col="black", lwd = 2, add = TRUE, lty = 1) 
plot(calc(scen, mean), ylim=c(-45, -10),xlim=c(105, 155),axes = F, box = FALSE, legend = TRUE, breaks = brks, col = c("grey", colfunc(length(brks)-2)), zlim = c(min(brks), max(brks)), main = "Survival (prob)", legend.only=TRUE, legend.width=2, legend.shrink=0.6, legend.args=list(text = "",side=4, font=2, line=2.5, cex=0.8))

dev.off()

}