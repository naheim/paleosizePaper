 # All the Code for Motility (Shannon & Noah)
 
 # Call in data & subset data into respective motility levels
 sizeData <- read.delim(file="https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/bodySizes.txt")
 timescale <- read.delim(file="https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt")
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

# For each motility level, draw segment (stratigraphic ranges) & mean line
plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
 abline(v = c(65, 200, 251.2, 443.8), col="black",lty=5)
segments(motility1$fad_age,motility1$log10max_vol,motility1$lad_age,motility1$log10max_vol, col="#ff5640")
 title(main="Motility Level 1")
 meanVector <- vector(mode='numeric', length=nrow(timescale))
 for(i in 1:nrow(timescale)) { 
  	meanVector[i] <- mean(motility1$log10max_vol[motility1$fad_age > timescale$age_top[i] & motility1$lad_age < timescale$age_bottom[i]]) }
 lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)
 
 plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
segments(motility2$fad_age,motility2$log10max_vol,motility2$lad_age,motility2$log10max_vol, col="#ffd900")
 abline(v = c(65, 200, 251.2, 443.8), col="black",lty=5)
 meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
  	meanVector[i] <- mean(motility2$log10max_vol[motility2$fad_age > timescale$age_top[i] & motility2$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)
 title(main="Motility Level 2")


plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
segments(motility3$fad_age,motility3$log10max_vol,motility3$lad_age,motility3$log10max_vol, col="#00ffd7")
 title(main="Motility Level 3")
 abline(v = c(65, 200, 251.2, 443.8), col="black",lty=5)
 meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
 	meanVector[i] <- mean(motility3$log10max_vol[motility3$fad_age > timescale$age_top[i] & motility3$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
segments(motility4$fad_age,motility4$log10max_vol,motility4$lad_age,motility4$log10max_vol, col="#ee92ed")
abline(v = c(65, 200, 251.2, 443.8), col="black",lty=5)
title(main="Motility Level 4")
 meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
 	meanVector[i] <- mean(motility4$log10max_vol[motility4$fad_age > timescale$age_top[i] & motility4$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
segments(motility5$fad_age,motility5$log10max_vol,motility5$lad_age,motility5$log10max_vol, col="#ff00ff")
abline(v = c(65, 200, 251.2, 443.8), col="black",lty=5)
title(main="Motility Level 5")
meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
 	meanVector[i] <- mean(motility5$log10max_vol[motility5$fad_age > timescale$age_top[i] & motility5$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
segments(motility6$fad_age,motility6$log10max_vol,motility6$lad_age,motility6$log10max_vol, col="#0000ff")
abline(v = c(65, 200, 251.2, 443.8), col="black",lty=5)
title(main="Motility Level 6")
meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
 	meanVector[i] <- mean(motility6$log10max_vol[motility6$fad_age > timescale$age_top[i] & motility6$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)

#FIGURE: Body Size Over Time - Color Coded Compiled
motile<-rbind(motility1, motility2)
nonmotile<-rbind(motility3, motility4, motility5, motility6)

time.plot.mult(nrow=1, ncol=2,las=1,top.mar=2.5)
par(las=1)

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
sizeData$log10max_vol <- log10(sizeData$max_vol)
abline(v = c(65, 200, 251.2, 443.8), col="black",lty=5)
segments(motility1$fad_age,motility1$log10max_vol,motility1$lad_age,motility1$log10max_vol, col="#ff5640")
title(main="Motile")
segments(motility2$fad_age,motility2$log10max_vol,motility2$lad_age,motility2$log10max_vol, col="#ff5640")

meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
  	meanVector[i] <- mean(motile$log10max_vol[motile$fad_age > timescale$age_top[i] & motile$lad_age < timescale$age_bottom[i]]) }
 lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
abline(v = c(65, 200, 251.2, 443.8), col="black",lty=5)
segments(motility3$fad_age,motility3$log10max_vol,motility3$lad_age,motility3$log10max_vol, col="#0000ff")
segments(motility4$fad_age,motility4$log10max_vol,motility4$lad_age,motility4$log10max_vol, col="#0000ff")
segments(motility5$fad_age,motility5$log10max_vol,motility5$lad_age,motility5$log10max_vol, col="#0000ff")
segments(motility6$fad_age,motility6$log10max_vol,motility6$lad_age,motility6$log10max_vol, col="#0000ff")
title(main="Nonmotile")
 
 meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
  	meanVector[i] <- mean(nonmotile$log10max_vol[nonmotile$fad_age > timescale$age_top[i] & nonmotile$lad_age < timescale$age_bottom[i]]) }
 lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)
 
 #FIGURE: Body Size vs Motility Boxplots
sizeDataMod <- sizeData[sizeData[,"motility"]!=0 & !is.na(sizeData$motility),]
sizeDataMod$motile <- 0
sizeDataMod$motile[(sizeDataMod[,"motility"]==1 | sizeDataMod[,"motility"]==2) & !is.na(sizeDataMod$motility)] <- 1
sizeDataMod$motile <- factor(sizeDataMod$motile, levels=c(1,0))

# Two simplified boxplots
boxplot(log10(sizeDataMod$max_vol)~sizeDataMod$motile,data=sizeDataMod,main = "Body Size vs Motility Boxplot", xlab="Motility Level", ylab="", col=c("#ff5640","#0000ff"), names=c("Motile","Nonmotile"),notch=TRUE)
title(ylab=expression(paste("Biovolume log"[10],"mm"^3)), line=2.2)

# Six individual boxplots
boxplot(log10(sizeDataMod$max_vol)~sizeDataMod$motility,data=sizeDataMod,main = "Body Size vs Motility Level Boxplot", xlab="Motility Level", ylab="", col=c("#ff5640", "#ffd900","#00ffd7", "#ee92ed", "#ff00ff", "#0000ff"),notch=TRUE)
title(ylab=expression(paste("Biovolume log"[10],"mm"^3)), line=2.2)

#FIGURE: Proportional Diversity
source("https://raw.githubusercontent.com/naheim/paleosizePaper/master/sharedCode/functions.r")
sizeData <- read.delim(file="https://github.com/naheim/paleosizePaper/raw/master/rawDataFiles/bodySizes.txt")
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
xPoly <- c(timescale$age_mid, rev(timescale$age_mid))
yPoly1 <- c(rep(0, nBins), rev(motilityProp[,"X0"])) 
yPoly2 <- c(motilityProp[,"X0"], rev(motilityProp[,"X0"] + motilityProp[,"X1"]))
classCols <- c("#0000ff","#ff5640")
time.plot(c(0,1), "Proportion of genera")
polygon(xPoly, yPoly1, col=classCols[1])
polygon(xPoly, yPoly2, col=classCols[2])
legend("topright", legend=rev(c("Nonmotile","Motile")), fill=rev(classCols), bg="white", title="Motility Levels")
title(main="Proportion of Motile vs Nonmotile Genera")


#FIGURE: Extinction Rate
sizeData <- read.delim(file="https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/bodySizes.txt")
timescale <- read.delim(file="https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt")
sizeData$motile <- 0
sizeData$motile[(sizeData[,"motility"]==1 | sizeData[,"motility"]==2) & !is.na(sizeData$motility)] <- 1

rateVector1 <- array(data=NA, dim=nrow(timescale))
rateVector2 <- array(data=NA, dim=nrow(timescale))

for (i in 1:nrow(timescale)) {

	intervalTime <- timescale[i,]

 	interval <- subset(sizeData, fad_age > intervalTime$age_top & lad_age < intervalTime$age_bottom)

 	interval$extinct <- 0

 	interval$extinct[interval$lad_age < intervalTime$age_bottom & interval$lad_age >= intervalTime$age_top] <- 1

	motileRows <- interval[interval$motile==1,]

	motileRowsExt <- motileRows[motileRows$extinct==1,]

	rateVector1[i] <- nrow(motileRowsExt)/nrow(motileRows) }

for (i in 1:nrow(timescale)) {

	intervalTime <- timescale[i,]

 	interval <- subset(sizeData, fad_age > intervalTime$age_top & lad_age < intervalTime$age_bottom)

 	interval$extinct <- 0

 	interval$extinct[interval$lad_age < intervalTime$age_bottom & interval$lad_age >= intervalTime$age_top] <- 1

	nonmotileRows <- interval[interval$motile==0,]

	nonmotileRowsExt <- nonmotileRows[nonmotileRows$extinct==0,]

	rateVector2[i] <- nrow(nonmotileRowsExt)/nrow(nonmotileRows) }
	
time.plot(c(-0.2,1.2), "Extinction Rate")
points(timescale$age_mid,rateVector1, col="#ff5640")
lines(x=timescale$age_mid,y=rateVector1, col="#ff5640", lwd=1.0)
points(timescale$age_mid,rateVector2, col="#0000ff")
lines(x=timescale$age_mid,y=rateVector2, col="#0000ff", lwd=1.0)
classCols <- c("#ff5640", "#0000ff")
title(main="Extinction Rate of Genera Over Time")
legend("topright", legend=rev(c("Motile","Nonmotile")), fill=rev(classCols), bg="white", title="Motility Levels")

#FIGURE: Extinciton Selectivity
sizeData <- read.delim(file="https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/bodySizes.txt")
timescale <- read.delim(file="https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt")
sizeData$motile <- 0
sizeData$motile[(sizeData[,"motility"]==1 | sizeData[,"motility"]==2) & !is.na(sizeData$motility)] <- 1
coeffVector <- array(data=NA, dim=nrow(timescale))
ciMinus <- array(data=NA, dim=nrow(timescale))
ciPlus <- array(data=NA, dim=nrow(timescale))

for (i in 1:nrow(timescale)) {
 	intervalTime <- timescale[i,]
 	interval <- subset(sizeData, fad_age > intervalTime$age_top & lad_age < intervalTime$age_bottom)
 	interval$extinct <- 0
 	interval$extinct[interval$lad_age < intervalTime$age_bottom & interval$lad_age >= intervalTime$age_top] <- 1
 	interval$logVolume <- log10(interval$max_vol)
 	if (sum(interval$extinct) >= 5 & nrow(interval)-sum(interval$extinct) >= 5) {
 		glmEqn <- "extinct ~ logVolume + motile"
 		intervalGlm <- glm(glmEqn, family="binomial", data=interval)
 		coeffVector[i] <- intervalGlm$coefficients[3] 
		ciTemp <- confint(intervalGlm)
		ciMinus[i] <- ciTemp[3,1]
		ciPlus[i] <- ciTemp[3,2]
	} 
}

coeffVector[coeffVector < -3 | coeffVector > 3] <- NA
ciMinus[coeffVector < -3 | coeffVector > 3 | is.na(coeffVector)] <- NA
ciPlus[coeffVector < -3 | coeffVector > 3 | is.na(coeffVector)] <- NA

time.plot(c(-3,3), "Selectivity Coefficient")
points(timescale$age_mid,coeffVector)
segments(timescale$age_mid,ciMinus,timescale$age_mid,ciPlus,col="black")
title(main="Extinction Selectivity Coefficient of Motility Over Time")
