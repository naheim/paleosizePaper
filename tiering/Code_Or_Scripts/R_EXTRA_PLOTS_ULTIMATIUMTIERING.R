#EXTRA PLOTS CREATED FOR PAPER

#COLOR YAY: http://projects.susielu.com/viz-palette?colors=[%22#ff5640%22,%22#ffd900%22,%22#00ffd7%22,%22#ee92ed%22,%22#ff00ff%22,%22#0000ff%22]&backgroundColor=%22white%22&fontColor=%22black%22

**************************************************************************************************************************************************************************************************************************************************************************************************************************************************************

#2: Proportion of Each Tiering Lvl Over Time:

#CHANGE DIRECTORIES!

sizeData <- read.delim("bodySizes.txt") 
sizeData <- subset(sizeData, !is.na(tiering) & tiering > 0)
timescale <- read.delim("timescale.txt") 
sizeData$tiering = factor(sizeData$tiering)
myProp = matrix(NA, nrow = nrow(timescale), ncol = 6)
my.col = c("#ff5640","#ffd900","#00ffd7","#ee92ed","#ff00ff","#0000ff")
for(i in 1:nrow(timescale)){temp <- subset(sizeData, sizeData$fad_age > timescale$age_top[i] & sizeData$lad_age < timescale$age_bottom[i])
counts <- table(temp$tiering)
myProp[i,] <- counts/sum(counts)
}
par(xaxs = "i", yaxs = "i")
source("https://github.com/naheim/paleosizePaper/raw/master/sharedCode/functions.r")
time.plot(c(0,1), "Proportion of Genera", cex.lab = 1.2, mar = c(4.5,4.5,4.5,10.5)+0.1, mgp=c(3, 0.75, 0), cex.axis = 1.25)
#plot(1:10, type = "n", xlim = c(541,0), ylim = c(0,1), xlab="Geologic Time (Ma)", ylab="Porportion of Genera")
myX <- c(timescale$age_mid, rev(timescale$age_mid))
myLast <- c(rep(0, nrow(timescale)), rev(myProp[,6]))
polygon(myX, myLast, col=my.col[6])
my2ndLast <- c(myProp[,6], rev(myProp[,6]+myProp[,5]))
polygon(myX, my2ndLast, col=my.col[5])
my3rdLast <- c(myProp[,6]+myProp[,5], rev(myProp[,6]+myProp[,5]+myProp[,4]))
polygon(myX, my3rdLast, col=my.col[4])
my2ndLast <- c(myProp[,6]+myProp[,5]+myProp[,4], rev(myProp[,6]+myProp[,5]+myProp[,4]+myProp[,3]))
polygon(myX, my2ndLast, col=my.col[3])
my1stLast <- c(myProp[,6]+myProp[,5]+myProp[,4]+myProp[,3], rev(myProp[,6]+myProp[,5]+myProp[,4]+myProp[,3]+myProp[,2]))
polygon(myX, my1stLast, col=my.col[2])
myFirst <- c(myProp[,6]+myProp[,5]+myProp[,4]+myProp[,3]+myProp[,2], rev(myProp[,6]+myProp[,5]+myProp[,4]+myProp[,3]+myProp[,2]+myProp[,1]))
polygon(myX, myFirst, col=my.col[1])
mtext(side=3, line=0.5, "The Change in the Proportions of Tiering Level Over 541 Million Years", col="black", font=4, cex=2)
par(xpd=FALSE)
arrows(-10, 0.93, 0, 0.93, xpd=TRUE)
mtext(side = 4, "Pelagic", at = c(0.93), cex = 1.5, col = "#ff5640", xpd = TRUE, line = 1.2) #ONE
arrows(-10, 0.839, 0, 0.839, xpd=TRUE)
mtext(side = 4, "Erect", at = c(0.839), cex = 1.5, col = "#ffd900", xpd = TRUE, line = 1.2)#TWO
arrows(-10, 0.55, 0, 0.55, xpd=TRUE)
mtext(side = 4, "Surficial", at = c(0.55), cex = 1.5, col = "#00ffd7", xpd = TRUE, line = 1.2)#THREE
arrows(-10, 0.222, 0, 0.222, xpd=TRUE)
mtext(side = 4, "Semi-infaunal", at = c(0.222), cex = 1.5, col = "#ee92ed", xpd = TRUE, line = 1.2)#FOUR
arrows(-10, 0.11, 0, 0.11, xpd=TRUE)
mtext(side = 4, "Shallow infaunal", at = c(0.11), cex = 1.5, col = "#ff00ff", xpd = TRUE, line = 1.2)#FIVE
arrows(-10, 0.011, 0, 0.011, xpd=TRUE)
mtext(side = 4, "Deep infaunal", at = c(0.011), cex = 1.5, col = "#0000ff", xpd = TRUE, line = 1.2)#SIX

**************************************************************************************************************************************************************************************************************************************************************************************************************************************************************
#5: Noel's Chart for Tiering Levels

#CHANGE DIRECTORIES!

sizeData <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/bodySizes.txt')
sizeData <- subset(sizeData, !is.na(tiering) & tiering > 0)
sizeData$log10_volume <- log10(sizeData$max_vol)
timescale <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt')
source("https://github.com/naheim/paleosizePaper/raw/master/sharedCode/functions.r")
time.plot(c(-10, 15), expression(paste("Biovolume (log  "[10]," mm"^3,")")), mar = c(4, 3.5, 4, 3.5)+0.5, mgp = c(2.5, 0.75, 0))
#plot(1:10,1:10, type="n", xlim=c(550,0), ylim=c(-4,12), xlab="Geological time (Ma)", ylab=expression(paste("Biovolume (log  "[10]," cm"^3,")")))
segments(sizeData$fad_age, sizeData$log10_volume, sizeData$lad_age, sizeData$log10_volume)
myMean <- vector(mode="numeric", length=nrow(timescale))
my05 <- myMean
my95 <- myMean
myCols<- c("#ff5640","#ffd900","#00ffd7","#ee92ed","#ff00ff","#0000ff")
sizeData$color<-myCols[sizeData$tiering]
segments(sizeData$fad_age, sizeData$log10_volume, sizeData$lad_age, sizeData$log10_volume, col=sizeData$color)
#legend("topleft", lty = 1, title = "Tiering Levels:",legend = c("Tiering Level 1:\n Pelagic", "Tiering Level 2:\n Erect", "Tiering Level 3:\n Surficial", "Tiering Level 4:\n Semi-infaunal", "Tiering Level 5:\n Shallow infaunal", "Tiering Level 6:\n Deep infaunal"), col = myCols)
, cex = 1)
legend(542, 15, legend=c("Tiering Level 1: Pelagic", "Tiering Level 2: Erect", "Tiering Level 3: Surficial", "Tiering Level 4: Semi-infaunal", "Tiering Level 5: Shallow infaunal", "Tiering Level 6: Deep infaunal"), col = myCols, lty = 1, title="Tiering Levels:", bg = "white", box.col="Black")
mtext(side=3, line=0.5, "Body Size Evolution Catagorized by Tiering Level Over the Phanerozoic", col="black", font=4, cex=1.3)
Num1<-sizeData[which(sizeData$tiering == 1),]
Num2<-sizeData[which(sizeData$tiering == 2),]
Num3<-sizeData[which(sizeData$tiering == 3),]
Num4<-sizeData[which(sizeData$tiering == 4),]
Num5<-sizeData[which(sizeData$tiering == 5),]
Num6<-sizeData[which(sizeData$tiering == 6),]
myMeanNum1 <- vector(mode="numeric", length=nrow(timescale)) 
for(i in 1:nrow(timescale)) {
	temp<-Num1[Num1$fad_age > timescale$age_top[i] & Num1$lad_age < timescale$age_bottom[i], ] 
	myMeanNum1[i]<-mean(temp$log10_volume) 
}
lines(timescale$age_mid, myMeanNum1, col="firebrick4", lwd=5) 
myMeanNum2 <- vector(mode="numeric", length=nrow(timescale))
for(i in 1:nrow(timescale)) {
	temp2<-Num2[Num2$fad_age > timescale$age_top[i] & Num2$lad_age < timescale$age_bottom[i], ]
	myMeanNum2[i]<-mean(temp2$log10_volume)
}
lines(timescale$age_mid, myMeanNum2, col="orange2", lwd=5)
myMeanNum3 <- vector(mode="numeric", length=nrow(timescale))
for(i in 1:nrow(timescale)) {
	temp3<-Num3[Num3$fad_age > timescale$age_top[i] & Num3$lad_age < timescale$age_bottom[i], ]
	myMeanNum3[i]<-mean(temp3$log10_volume)
}
lines(timescale$age_mid, myMeanNum3, col="cyan4", lwd=5)
myMeanNum4 <- vector(mode="numeric", length=nrow(timescale))
for(i in 1:nrow(timescale)) {
	temp4<-Num4[Num4$fad_age > timescale$age_top[i] & Num4$lad_age < timescale$age_bottom[i], ]
	myMeanNum4[i]<-mean(temp4$log10_volume)
}
lines(timescale$age_mid, myMeanNum4, col="deeppink", lwd=5)
myMeanNum5 <- vector(mode="numeric", length=nrow(timescale))
for(i in 1:nrow(timescale)) {
	temp5<-Num5[Num5$fad_age > timescale$age_top[i] & Num5$lad_age < timescale$age_bottom[i], ]
	myMeanNum5[i]<-mean(temp5$log10_volume)
}
lines(timescale$age_mid, myMeanNum5, col="magenta3", lwd=5)
myMeanNum6 <- vector(mode="numeric", length=nrow(timescale))
for(i in 1:nrow(timescale)) {
	temp6<-Num6[Num6$fad_age > timescale$age_top[i] & Num6$lad_age < timescale$age_bottom[i], ]
	myMeanNum6[i]<-mean(temp6$log10_volume)
}
lines(timescale$age_mid, myMeanNum6, col="blue3", lwd=5)

**************************************************************************************************************************************************************************************************************************************************************************************************************************************************************
#7: Stephanie Logistic Regression Combined Tiering Graph 

#CHANGE DIRECTORIES!

nBins <- nrow(timescale)
myCols<- c("red", "orange", "green", "cyan", "magenta", "blue")
tierLabs <- c("Pelagic", "Erect", "Surficial", "Semi-infaunal", "Shallow infaunal", "Deep infaunal")
time.plot.mult(nrow=2, ncol=3, plot.width=10, top.mar=2)
for(j in 1:6){
	tiering <- subset(sizeData, tiering==j)
	tierExtSel <- data.frame(matrix(NA, nrow=nBins, ncol=3, dimnames=list(timescale$interval_name, c('coef','ci.minus','ci.plus'))))
	for(i in 1:nBins) {
		temp <- subset(tiering, fad_age > timescale$age_top[i] & lad_age < timescale$age_bottom[i])
		if(nrow(temp) > 0) {
			temp$extinct <- 0
			temp$extinct[temp$lad_age >= timescale$age_top[i] & temp$lad_age < timescale$age_bottom[i]] <- 1 
			if(sum(temp$extinct) >= 3 & nrow(temp) >= 6) {
				myGlm <- glm(extinct ~ log10(max_vol), data=temp, family=binomial)
				tierExtSel$coef[i] <- myGlm$coefficients[2]
				myCi <- confint(myGlm)
				tierExtSel$ci.minus[i] <- myCi[2,1]
				tierExtSel$ci.plus[i] <- myCi[2,2]
			}
		}
	}
	plot(timescale$age_mid[tierExtSel$coef > -5 & tierExtSel$coef < 5], tierExtSel$coef[tierExtSel$coef > -5 & tierExtSel$coef < 5], pch=16, cex=1.25, xaxt="n", xlab="", xlim=c(541,0), col=myCols[j], ylab=" Log-Odds of Extinction")
	abline(h=0, lty=2)
	segments(timescale$age_mid,tierExtSel$ci.minus,timescale$age_mid,tierExtSel$ci.plus)
	title(main= paste("Tiering ", j, ": ", tierLabs[j], sep=""))
}

**************************************************************************************************************************************************************************************************************************************************************************************************************************************************************











