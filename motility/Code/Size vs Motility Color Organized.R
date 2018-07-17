sizeData$log10max_vol <- log10(sizeData$max_vol)

# making the plot not cut off


plot(NA, xlab="Geologic Time (Ma)", ylab=expression(paste("Body Size log"[10],"mm"^3)), main="Body Size Evolution of Marine Genera with Motility Level 1", xlim=c(550,0), ylim=c(-2,12))
segments(motility1$fad_age,motility1$log10max_vol,motility1$lad_age,motility1$log10max_vol, col="darkorange")

plot(NA, xlab="Geologic Time (Ma)", ylab=expression(paste("Body Size log"[10],"mm"^3)), main="Body Size Evolution of Marine Genera with Motility Level 2", xlim=c(550,0), ylim=c(-2,12))
segments(motility2$fad_age,motility2$log10max_vol,motility2$lad_age,motility2$log10max_vol, col="dodgerblue")

plot(NA, xlab="Geologic Time (Ma)", ylab=expression(paste("Body Size log"[10],"mm"^3)), main="Body Size Evolution of Marine Genera with Motility Level 3", xlim=c(550,0), ylim=c(-2,12))
segments(motility3$fad_age,motility3$log10max_vol,motility3$lad_age,motility3$log10max_vol, col="yellow")

plot(NA, xlab="Geologic Time (Ma)", ylab=expression(paste("Body Size log"[10],"mm"^3)), main="Body Size Evolution of Marine Genera with Motility Level 4", xlim=c(550,0), ylim=c(-2,12))
segments(motility4$fad_age,motility4$log10max_vol,motility4$lad_age,motility4$log10max_vol, col="forestgreen")

plot(NA, xlab="Geologic Time (Ma)", ylab=expression(paste("Body Size log"[10],"mm"^3)), main="Body Size Evolution of Marine Genera with Motility Level 5", xlim=c(550,0), ylim=c(-2,12))
segments(motility5$fad_age,motility5$log10max_vol,motility5$lad_age,motility5$log10max_vol, col="red")

plot(NA, xlab="Geologic Time (Ma)", ylab=expression(paste("Body Size log"[10],"mm"^3)), main="Body Size Evolution of Marine Genera with Motility Level 6", xlim=c(550,0), ylim=c(-2,12))
segments(motility6$fad_age,motility6$log10max_vol,motility6$lad_age,motility6$log10max_vol, col="brown")

#mean
meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
 	meanVector[i] <- mean(sizeData$log10max_vol[sizeData$fad_age > timescale$age_top[i] & sizeData$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)