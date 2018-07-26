#TOTAL GRAPH SINGLE DOC:

#title.adj
#mtext(side=3, line=0.5, "The Change in the Amount of Genera Categorized by Tiering Level Over Million-Years", col="black", font=4, cex=2)
#legend(534, 0.50, legend=c("Tiering Level 1: Pelagic", "Tiering Level 2: Erect", "Tiering Level 3: Surficial", "Tiering Level 4: Semi-infaunal", "Tiering Level 5: Shallow infaunal", "Tiering Level 6: Deep infaunal"), col = my.col, lty = 1, title="Tiering Color Legend", bg = "white", box.col="NA", cex=0.8, text.font = 1)

**************************************************************************************************************************************************************************************************************************************************************************************************************************************************************
#1: # of Genera in Each Tiering Lvl Over Time:

#The Change in the Amount of Genera Categorized by Tiering Level Over Million-Years


#COLOR YAY: http://projects.susielu.com/viz-palette?colors=[%22#ff5640%22,%22#ffd900%22,%22#00ffd7%22,%22#ee92ed%22,%22#ff00ff%22,%22#0000ff%22]&backgroundColor=%22white%22&fontColor=%22black%22
#WORKS: 

sizeData <- read.delim("bodySizes.txt") 
timescale <- read.delim("timescale.txt") 
sizeData = subset(sizeData, !is.na(tiering) & tiering != 0)

n.bins = nrow(timescale)

sizeData$tiering = factor(sizeData$tiering, levels = 1:6)
my.tiering = data.frame()

for(i in 1:nrow(timescale)){temp.data = sizeData$tiering[sizeData$fad_age > timescale$age_top[i] & sizeData$lad_age < timescale$age_bottom[i]]
	temp.count = data.frame(table(temp.data))$Freq
	my.tiering = rbind(my.tiering, temp.count)
	}
table(my.tiering)
par(xaxs = "i", yaxs = "i")
my.col = c("#ff5640","#ffd900","#00ffd7","#ee92ed","#ff00ff","#0000ff")
source("https://github.com/naheim/paleosizePaper/raw/master/sharedCode/functions.r")

time.plot(c(0, max(my.tiering)), "Amount of Genera", cex.lab = 2, mar = c(4.5,4.5,4.5,4.5)+0.1, mgp=c(3, 0.75, 0), cex.axis = 1.25)

#plot(1:10,1:10, type="n", xlim=c(541,-5), ylim=range(my.tiering), pch=21, xlab="Geologic Time (Ma)", ylab="Amount of Genera")
for(i in 1:1:6){lines(timescale$age_bottom, my.tiering[,i], col=my.col[i], lwd = 3)}
abline(v = c(65, 200, 251.2, 443.8), col="black", lty = 5)
mtext(side=3, line=0.5, "The Change in the Amount of Genera Categorized by Tiering Level Over Million-Years", col="black", font=4, cex=2)
legend(540, 1197, legend=c("Tiering Level 1: Pelagic", "Tiering Level 2: Erect", "Tiering Level 3: Surficial", "Tiering Level 4: Semi-infaunal", "Tiering Level 5: Shallow infaunal", "Tiering Level 6: Deep infaunal"), col = my.col, lty = 1, title="Tiering Levels:", bg = "white", box.col=NA, cex=1)

**************************************************************************************************************************************************************************************************************************************************************************************************************************************************************

#2: Proportion of Each Tiering Lvl Over Time:

#CHANGE DIRECTORIES!

#WORKS:

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
time.plot(c(0,1), "Proportion of Genera", cex.lab = 2, mar = c(4.5,4.5,4.5,10.5)+0.1, mgp=c(3, 0.75, 0), cex.axis = 1.25)
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

#3: Mean Biovolume of Each Tiering Lvl Over Time

#CHANGE DIRECTORY!

bodySize <- read.delim("bodySizes.txt") 
timescale <- read.delim("timescale.txt") 
bodySize <- subset(sizeData, !is.na(tiering) & tiering != 0)

library(paleoTS)

n.bins <- nrow(timescale)

my.mean <- matrix(NA, nrow=n.bins, ncol=6)
my.var <- matrix(NA, nrow=n.bins, ncol=6)
my.n <- matrix(NA, nrow=n.bins, ncol=6)
my.time <- timescale$age_bottom


names(my.mean) <- timescale$interval_name
names(my.var) <- timescale$interval_name
names(my.n) <- timescale$interval_name
names(my.time) <- timescale$interval_name

for (i in 1:n.bins) {for (j in 1:6) {
temp.data <- log10(bodySize$max_vol[bodySize$fad_age > timescale$age_top[i] & bodySize$lad_age < timescale$age_bottom[i] & bodySize$tiering == j])
my.mean[i,j] <- mean(temp.data)
my.var[i,j] <- var(temp.data)
my.n[i,j] <- length(temp.data)
}
}
par(col="black")
source("https://github.com/naheim/paleosizePaper/raw/master/sharedCode/functions.r")
time.plot(c(0, 8), expression(paste("Biovolume (log  "[10]," mm"^3,")")), cex.lab = 2, mar = c(4.5,4.5,4.5,4.5)+1, mgp=c(3, 0.75, 0), cex.axis = 1.25)

#plot(timescale$age_bottom, my.mean[,3], type="n", pch=16, xlab="Geologic Time (Ma)", xlim=c(541, 0), ylab="Mean Size", ylim=c(1.2,6.5), main="Mean expression(paste("Biovolume (log"[10]," cm"^3)"))

my.col = c("#ff5640","#ffd900","#00ffd7","#ee92ed","#ff00ff","#0000ff")
for(i in 1:6) {
  my.ts <- as.paleoTS(mm=my.mean[!is.na(my.var[,i]), i], vv=my.var[!is.na(my.var[,i]), i], nn=my.n[!is.na(my.var[,i]), i], tt=my.time[!is.na(my.var[,i])], oldest="last")
  fit3models(my.ts, method="Joint", pool=FALSE)
  #par(col=my.mean$color[k]); par(col="deepskyblue3")
  lines(timescale$age_mid, my.mean[, i], col=my.col[i], lwd = 3)
}
mtext(side=3, line=0.5, "The Change in the Mean Biovolume of Tiering Levels Over Million Years", col="black", font=4, cex=2)
par(col="black")
abline(v = c(65, 200, 251.2, 443.8), col="black", lty = 5)
legend(540, 7.98, legend=c("Tiering Level 1: Pelagic", "Tiering Level 2: Erect", "Tiering Level 3: Surficial", "Tiering Level 4: Semi-infaunal", "Tiering Level 5: Shallow infaunal", "Tiering Level 6: Deep infaunal"), col = my.col, lty = 1, title="Tiering Level", bg = "white", box.col=NA, cex=1)
#END

**************************************************************************************************************************************************************************************************************************************************************************************************************************************************************

#4: Body Size Box Plot for Each Tier/Biovolume vs. Tiering Level

par(col="black")
my.col = c("#ff5640","#ffd900","#00ffd7","#ee92ed","#ff00ff","#0000ff")
par(mar = c(4, 3.5, 4, 3.5)+0.5)
par(mgp = c(2.5, 1.5, 0))
boxplot(log10(max_vol)~tiering, bodySize, xlab="Tiering Level", ylab=expression(paste("Biovolume (log  "[10]," mm"^3,")")), col=my.col, names=c("Tiering Level 1:\n Pelagic", "Tiering Level 2:\n Erect", "Tiering Level 3:\n Surficial", "Tiering Level 4:\n Semi-infaunal", "Tiering Level 5:\n Shallow infaunal", "Tiering Level 6:\n Deep infaunal"))
mtext(side=3, line=0.5, "Biovolume vs. Tiering Level", col="black", font=4, cex=2)

**************************************************************************************************************************************************************************************************************************************************************************************************************************************************************

#5: Stephanie's Reuse of Noel's Chart for Tiering Levels with Wrong Colors

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
mtext(side=3, line=0.5, "Body Size Evolution Catagorized by Tiering Level Over 541 Million Years", col="black", font=4, cex=1.3)
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

#MultiGraph with Previous Stephanie Graph

sizeData <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/bodySizes.txt')
sizeData <- subset(sizeData, !is.na(tiering) & tiering > 0)
sizeData$log10_volume <- log10(sizeData$max_vol)

timescale <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt')
source("https://github.com/naheim/paleosizePaper/raw/master/sharedCode/functions.r")
myCols<- c("#ff5640","#ffd900","#00ffd7","#ee92ed","#ff00ff","#0000ff")
sizeData$log10max_vol <- log10(sizeData$max_vol)

Num1<-sizeData[which(sizeData$tiering == 1),]
Num2<-sizeData[which(sizeData$tiering == 2),]
Num3<-sizeData[which(sizeData$tiering == 3),]
Num4<-sizeData[which(sizeData$tiering == 4),]
Num5<-sizeData[which(sizeData$tiering == 5),]
Num6<-sizeData[which(sizeData$tiering == 6),]

#"Tiering Level 1: Pelagic", "Tiering Level 2: Erect", "Tiering Level 3: Surficial", "Tiering Level 4: Semi-infaunal", "Tiering Level 5: Shallow infaunal", "Tiering Level 6: Deep infaunal"

time.plot.mult(nrow=2, ncol=3,las=1,top.mar=2.5)
par(las=1)
plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log  "[10]," mm"^3*")")))
 sizeData$log10max_vol <- log10(sizeData$max_vol)
 segments(Num1$fad_age,Num1$log10max_vol,Num1$lad_age,Num1$log10max_vol, col=myCols[1])
 title(main="Tiering Level 1: Pelagic")
 meanVector <- vector(mode='numeric', length=nrow(timescale))
 for(i in 1:nrow(timescale)) { 
  	meanVector[i] <- mean(Num1$log10max_vol[Num1$fad_age > timescale$age_top[i] & Num1$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=4)

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log  "[10]," mm"^3*")")))
segments(Num2$fad_age,Num2$log10max_vol,Num2$lad_age,Num2$log10max_vol, col=myCols[2])
meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
  	meanVector[i] <- mean(Num2$log10max_vol[Num2$fad_age > timescale$age_top[i] & Num2$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=4)
 title(main="Tiering Level 2: Erect")

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log  "[10]," mm"^3*")")))
segments(Num3$fad_age,Num3$log10max_vol,Num3$lad_age,Num3$log10max_vol, col=myCols[3])
 title(main="Tiering Level 3: Surficial")
 meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
 	meanVector[i] <- mean(Num3$log10max_vol[Num3$fad_age > timescale$age_top[i] & Num3$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=4)

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
segments(Num4$fad_age,Num4$log10max_vol,Num4$lad_age,Num4$log10max_vol, col=myCols[4])
title(main="Tiering Level 4: Semi-infaunal")
 meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
 	meanVector[i] <- mean(Num4$log10max_vol[Num4$fad_age > timescale$age_top[i] & Num4$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=4)

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log  "[10]," mm"^3*")")))
segments(Num5$fad_age,Num5$log10max_vol,Num5$lad_age,Num5$log10max_vol, col=myCols[5])
title(main="Tiering Level 5: Shallow infaunal")
meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
 	meanVector[i] <- mean(Num5$log10max_vol[Num5$fad_age > timescale$age_top[i] & Num5$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=4)

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log  "[10]," mm"^3*")")))
segments(Num6$fad_age,Num6$log10max_vol,Num6$lad_age,Num6$log10max_vol, col=myCols[6])
title(main="Tiering Level 6: Deep infaunal")
meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
 	meanVector[i] <- mean(Num6$log10max_vol[Num6$fad_age > timescale$age_top[i] & Num6$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=4)

**************************************************************************************************************************************************************************************************************************************************************************************************************************************************************

#Stephanie Logistic Regression Combined Tiering Graph 

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

#PaleoTS Akike Walks 

bodySize <- read.delim("bodySizes.txt") 
timescale <- read.delim("timescale.txt") 
bodySize <- subset(bodySize, !is.na(tiering) & tiering !=0)

library(paleoTS)

n.bins <- nrow(timescale)

my.mean <- matrix(NA, nrow=n.bins, ncol=6)
my.var <- matrix(NA, nrow=n.bins, ncol=6)
my.n <- matrix(NA, nrow=n.bins, ncol=6)
my.time <- timescale$age_bottom


names(my.mean) <- timescale$interval_name
names(my.var) <- timescale$interval_name
names(my.n) <- timescale$interval_name
names(my.time) <- timescale$interval_name

for (i in 1:n.bins) {for (j in 1:6) {
temp.data <- log10(bodySize$max_vol[bodySize$fad_age > timescale$age_top[i] & bodySize$lad_age < timescale$age_bottom[i] & bodySize$tiering == j])
my.mean[i,j] <- mean(temp.data)
my.var[i,j] <- var(temp.data)
my.n[i,j] <- length(temp.data)
}
}

my.ts <- as.paleoTS(mm=my.mean, vv=my.var, nn=my.n, tt=my.time, oldest="last")
fit3models(my.ts, method="Joint", pool=FALSE)


