 sizeData <- read.delim(file="https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/bodySizes.txt")
 timescale <- read.delim(file="https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt")
 source("https://github.com/naheim/paleosizePaper/raw/master/sharedCode/functions.r")
 sizeData$log10max_vol <- log10(sizeData$max_vol)
 motility1 <- sizeData[sizeData[,"motility"]==1 & !is.na(sizeData$motility),]
 motility2 <- sizeData[sizeData[,"motility"]==2 & !is.na(sizeData$motility),]  
 motility3 <- sizeData[sizeData[,"motility"]==3 & !is.na(sizeData$motility),]  
 motility4 <- sizeData[sizeData[,"motility"]==4 & !is.na(sizeData$motility),]  
 motility5 <- sizeData[sizeData[,"motility"]==5& !is.na(sizeData$motility),]  
 motility6 <- sizeData[sizeData[,"motility"]==6& !is.na(sizeData$motility),]  
 myCol <- c("#ff5640","#ffd900","#00ffd7","#ee92ed","#ff00ff","#0000ff")
 

 #FIGURE: Body Size Over Time - Color Coded
 time.plot.mult(nrow=2, ncol=3,las=1, top.mar=2.5)
 par(las=1)
 
plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
 abline(v = c(65, 200, 251.2, 365, 443.8), col="black",lty=5)
 segments(motility1$fad_age,motility1$log10max_vol,motility1$lad_age,motility1$log10max_vol, col="#ff5640")
 mtext(side=3, line=0.5, "Freely, fast", col="black", font=2, cex=1.3)
 meanVector <- vector(mode='numeric', length=nrow(timescale))
 for(i in 1:nrow(timescale)) { 
  	meanVector[i] <- mean(motility1$log10max_vol[motility1$fad_age > timescale$age_top[i] & motility1$lad_age < timescale$age_bottom[i]]) }
 lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)
 
 plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
 segments(motility2$fad_age,motility2$log10max_vol,motility2$lad_age,motility2$log10max_vol, col="#ffd900")
 abline(v = c(65, 200, 251.2, 365, 443.8), col="black",lty=5)
 meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
  	meanVector[i] <- mean(motility2$log10max_vol[motility2$fad_age > timescale$age_top[i] & motility2$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)
 mtext(side=3, line=0.5, "Freely, slow", col="black", font=2, cex=1.3)


plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
segments(motility3$fad_age,motility3$log10max_vol,motility3$lad_age,motility3$log10max_vol, col="#00ffd7")
 mtext(side=3, line=0.5, "Facultative, unattached", col="black", font=2, cex=1.3)
 abline(v = c(65, 200, 251.2, 365, 443.8), col="black",lty=5)
 meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
 	meanVector[i] <- mean(motility3$log10max_vol[motility3$fad_age > timescale$age_top[i] & motility3$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
segments(motility4$fad_age,motility4$log10max_vol,motility4$lad_age,motility4$log10max_vol, col="#ee92ed")
 abline(v = c(65, 200, 251.2, 365, 443.8), col="black",lty=5)
 mtext(side=3, line=0.5, "Facultative, attached", col="black", font=2, cex=1.3)
 meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
 	meanVector[i] <- mean(motility4$log10max_vol[motility4$fad_age > timescale$age_top[i] & motility4$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
segments(motility5$fad_age,motility5$log10max_vol,motility5$lad_age,motility5$log10max_vol, col="#ff00ff")
 abline(v = c(65, 200, 251.2, 365, 443.8), col="black",lty=5)
 mtext(side=3, line=0.5, "Non-motile, unattached", col="black", font=2, cex=1.3)
meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
 	meanVector[i] <- mean(motility5$log10max_vol[motility5$fad_age > timescale$age_top[i] & motility5$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
segments(motility6$fad_age,motility6$log10max_vol,motility6$lad_age,motility6$log10max_vol, col="#0000ff")
 abline(v = c(65, 200, 251.2, 365, 443.8), col="black",lty=5)
 mtext(side=3, line=0.5, "Non-motile, attached", col="black", font=2, cex=1.3)
meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
 	meanVector[i] <- mean(motility6$log10max_vol[motility6$fad_age > timescale$age_top[i] & motility6$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)

#FIGURE: Body Size Over Time - Color Coded Compiled
motile<-rbind(motility1, motility2)
nonmotile<-rbind( motility3, motility4, motility5, motility6)

time.plot.mult(nrow=1, ncol=2,las=1,top.mar=2.5)
par(las=1)

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
sizeData$log10max_vol <- log10(sizeData$max_vol)
 abline(v = c(65, 200, 251.2, 365, 443.8), col="black",lty=5)
segments(motility1$fad_age,motility1$log10max_vol,motility1$lad_age,motility1$log10max_vol, col="#ff5640")
segments(motility2$fad_age,motility2$log10max_vol,motility2$lad_age,motility2$log10max_vol, col="#ff5640")
 mtext(side=3, line=0.5, "Motile", col="black", font=2, cex=1.3)

meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
  	meanVector[i] <- mean(motile$log10max_vol[motile$fad_age > timescale$age_top[i] & motile$lad_age < timescale$age_bottom[i]]) }
  lines(x=timescale$age_mid,y=meanVector, col="white", lwd=3)
 lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
 abline(v = c(65, 200, 251.2, 365, 443.8), col="black",lty=5)
segments(motility3$fad_age,motility3$log10max_vol,motility3$lad_age,motility3$log10max_vol, col="#0000ff")
segments(motility4$fad_age,motility4$log10max_vol,motility4$lad_age,motility4$log10max_vol, col="#0000ff")
segments(motility5$fad_age,motility5$log10max_vol,motility5$lad_age,motility5$log10max_vol, col="#0000ff")
segments(motility6$fad_age,motility6$log10max_vol,motility6$lad_age,motility6$log10max_vol, col="#0000ff")
 mtext(side=3, line=0.5, "Non-motile", col="black", font=2, cex=1.3)
  
 meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
  	meanVector[i] <- mean(nonmotile$log10max_vol[nonmotile$fad_age > timescale$age_top[i] & nonmotile$lad_age < timescale$age_bottom[i]]) }
  	  lines(x=timescale$age_mid,y=meanVector, col="white", lwd=4)
 lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)
 
 #FIGURE: Body Size vs Motility Boxplots
sizeDataMod <- sizeData[sizeData[,"motility"]!=0 & !is.na(sizeData$motility),]
sizeDataMod$motile <- 0
sizeDataMod$motile[(sizeDataMod[,"motility"]==1 | sizeDataMod[,"motility"]==2) & !is.na(sizeDataMod$motility)] <- 1
sizeDataMod$motile <- factor(sizeDataMod$motile, levels=c(1,0))

boxplot(log10(sizeDataMod$max_vol)~sizeDataMod$motile,data=sizeDataMod, xlab="", ylab="", col=c("#ff5640","#0000ff"), names=c("Motile","Non-motile"),notch=TRUE)
mtext(side=3, line=0.5, "Body Size vs Motility Boxplot", col="black", font=2, cex=1.3)
title(ylab=expression(paste("Biovolume log"[10],"mm"^3)), line=2.2)
mtext(side=1, line=3, text="Motility Level",font=2)

par(mar=c(9,4,4,1))
boxplot(log10(sizeDataMod$max_vol)~sizeDataMod$motility,data=sizeDataMod, xlab="", ylab="", col=c("#ff5640", "#ffd900","#00ffd7", "#ee92ed", "#ff00ff", "#0000ff"),notch=TRUE, names=NA)
mtext(side=1, line=0.8, at=1, text="Freely, fast")
mtext(side=1, line=1.8, at=c(3,5), text=c("Facultative, \nunattached", "Non-motile, \nunattached"))
mtext(side=1, line=3, at=2, text="Freely, slow")
mtext(side=1, line=4, at=c(4,6), text=c("Facultative, \nattached", "Non-motile, \nattached"))
mtext(side=1, line=6.5, text="Motility Level",font=2)
mtext(side=3, line=0.5, "Body Size vs Motility Level Boxplot", col="black", font=2, cex=1.3)
title(ylab=expression(paste("Biovolume log"[10],"mm"^3)), line=2.2)

#FIGURE: Proportional Diversity
source("https://raw.githubusercontent.com/naheim/paleosizePaper/master/sharedCode/functions.r")
sizeData <- read.delim(file="https://github.com/naheim/paleosizePaper/raw/master/rawDataFiles/bodySizes.txt")
sizeData <- subset(sizeData, !is.na(motility) & motility > 0)
timescale <- read.delim(file="https://github.com/naheim/paleosizePaper/raw/master/rawDataFiles/timescale.txt") 
nBins <- nrow(timescale)
sizeData$motile <- 0
sizeData$motile[(sizeData[,"motility"]==1 | sizeData[,"motility"]==2) & !is.na(sizeData$motility)] <- 1
motilityLevel <- subset(sizeData, is.element(motile, c(0,1)))
motilityLevel$motile <- factor(motilityLevel$motile)
motilityProp <- data.frame(matrix(NA, nrow=nBins, ncol=2, dimnames=list(timescale$interval_name, levels(motilityLevel$motile))))
for(i in 1:nBins) {
	temp <- subset(motilityLevel, fad_age > timescale$age_top[i] & lad_age < timescale$age_bottom[i]) 
	motilityCounts <- table(temp$motile) 
	prop <- motilityCounts/sum(motilityCounts) 
	motilityProp[i,] <- prop
}
myCols <- c("#ff5640", "#0000ff")
xPoly <- c(timescale$age_mid, rev(timescale$age_mid))
yPoly1 <- c(rep(0, nBins), rev(motilityProp[,"X1"])) 
yPoly2 <- c(motilityProp[,"X1"], rev(motilityProp[,"X1"] + motilityProp[,"X0"]))
time.plot(c(0,1), "Proportion of Genera", mar=c(4,3.5,6,3.5), mgp=c(2.5,0.75,0))
polygon(xPoly, yPoly1, col=myCols[1])
polygon(xPoly, yPoly2, col=myCols[2])
legend("topright", legend=c("Non-motile","Motile"), fill=rev(myCols), bg="white", title="Motility Levels")
mtext(side=3, line=0.5, "Proportion of Motile vs Non-motile Genera", col="black", font=2, cex=1.3)

#FIGURE: Proportions by Motility Level
source("https://raw.githubusercontent.com/naheim/paleosizePaper/master/sharedCode/functions.r")
sizeData <- read.delim(file="https://github.com/naheim/paleosizePaper/raw/master/rawDataFiles/bodySizes.txt")
sizeData <- subset(sizeData, !is.na(motility) & motility > 0)
timescale <- read.delim(file="https://github.com/naheim/paleosizePaper/raw/master/rawDataFiles/timescale.txt") 
nBins <- nrow(timescale)
sizeData$motility <- factor(sizeData$motility)
motilityProp <- matrix(NA, nrow=nBins, ncol=6)
for(i in 1:nBins) {
	temp <- subset(sizeData, sizeData$fad_age > timescale$age_top[i] & sizeData$lad_age < timescale$age_bottom[i]) 
	motilityCounts <- table(temp$motility) 
	motilityProp[i,] <- motilityCounts/sum(motilityCounts) 
}
time.plot(c(0,1), "Proportion of Genera", mar=c(4,3.5,6,3.5), mgp=c(2.5,0.75,0))
myCol <- c("#ff5640","#ffd900","#00ffd7","#ee92ed","#ff00ff","#0000ff")
xPoly <- c(timescale$age_mid, rev(timescale$age_mid))
yPoly1 <- c(rep(0, nBins), rev(motilityProp[,1])) 
polygon(xPoly, yPoly1, col= myCol[1])
yPoly2 <- c(motilityProp[,1], rev(motilityProp[,1] + motilityProp[,2]))
polygon(xPoly, yPoly2, col= myCol[2])
yPoly3 <- c(motilityProp[,1] + motilityProp[,2], rev(motilityProp[,1] + motilityProp[,2] + motilityProp[,3]))
polygon(xPoly, yPoly3, col= myCol[3])
yPoly4 <- c(motilityProp[,1] + motilityProp[,2] + motilityProp[,3], rev(motilityProp[,1] + motilityProp[,2] + motilityProp[,3] + motilityProp[,4]))
polygon(xPoly, yPoly4, col= myCol[4])
yPoly5 <- c(motilityProp[,1] + motilityProp[,2] + motilityProp[,3] + motilityProp[,4], rev(motilityProp[,1] + motilityProp[,2] + motilityProp[,3] + motilityProp[,4] + motilityProp[,5]))
polygon(xPoly, yPoly5, col= myCol[5])
yPoly6 <- c(motilityProp[,1] + motilityProp[,2] + motilityProp[,3] + motilityProp[,4] + motilityProp[,5], rev(motilityProp[,1] + motilityProp[,2] + motilityProp[,3] + motilityProp[,4] + motilityProp[,5] + motilityProp[,6]))
polygon(xPoly, yPoly6, col=myCol[6])
legend(460,1, legend=rev(c("Freely, fast", "Freely, slow", "Facultative, unattached", "Facultative, attached", "Non-motile, unattached", "Non-motile, attached")), fill=rev(myCol), bg="white", title="Motility Levels",cex=0.85)
mtext(side=3, line=0.5, "Proportion of Genera for Each Motility Level", col="black", font=2, cex=1.3)
