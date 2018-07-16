
 time.plot.mult(nrow=2, ncol=3,las=1,top.mar=2.5)
 par(las=1)
plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
 sizeData$log10max_vol <- log10(sizeData$max_vol)
 segments(motility1$fad_age,motility1$log10max_vol,motility1$lad_age,motility1$log10max_vol, col="darkorange")
 title(main="Motility Level 1")
 meanVector <- vector(mode='numeric', length=nrow(timescale))
 for(i in 1:nrow(timescale)) { 
  	meanVector[i] <- mean(motility1$log10max_vol[motility1$fad_age > timescale$age_top[i] & motility1$lad_age < timescale$age_bottom[i]]) }
 lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)
 plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
 segments(motility2$fad_age,motility2$log10max_vol,motility2$lad_age,motility2$log10max_vol, col="dodgerblue")
 meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
  	meanVector[i] <- mean(motility2$log10max_vol[motility2$fad_age > timescale$age_top[i] & motility2$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)
 title(main="Motility Level 2")


plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
segments(motility3$fad_age,motility3$log10max_vol,motility3$lad_age,motility3$log10max_vol, col="gold")
 title(main="Motility Level 3")
 meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
 	meanVector[i] <- mean(motility3$log10max_vol[motility3$fad_age > timescale$age_top[i] & motility3$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
segments(motility4$fad_age,motility4$log10max_vol,motility4$lad_age,motility4$log10max_vol, col="forestgreen")
title(main="Motility Level 4")
 meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
 	meanVector[i] <- mean(motility4$log10max_vol[motility4$fad_age > timescale$age_top[i] & motility4$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
segments(motility5$fad_age,motility5$log10max_vol,motility5$lad_age,motility5$log10max_vol, col="red")
title(main="Motility Level 5")
meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
 	meanVector[i] <- mean(motility5$log10max_vol[motility5$fad_age > timescale$age_top[i] & motility5$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
segments(motility6$fad_age,motility6$log10max_vol,motility6$lad_age,motility6$log10max_vol, col="purple")
title(main="Motility Level 6")
meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
 	meanVector[i] <- mean(motility6$log10max_vol[motility6$fad_age > timescale$age_top[i] & motility6$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)