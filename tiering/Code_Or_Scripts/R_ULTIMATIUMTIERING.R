#TOTAL CODE SINGLE DOC:

**************************************************************************************************************************************************************************************************************************************************************************************************************************************************************
#1: # of Gentra in Each Tiering Lvl Over Time:

#The Change in the Amount of Gentra Categorized by Tiering Level Over Million-Years

#WORKS: 

sizeData <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/bodySizes.txt') 
timescale <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt') 
sizeData = subset(sizeData, !is.na(tiering) & tiering != 0)

n.bins = nrow(timescale)

sizeData$tiering = factor(sizeData$tiering, levels = 1:6)
my.tiering = data.frame()

for(i in 1:nrow(timescale)){temp.data = sizeData$tiering[sizeData$fad_age > timescale$age_top[i] & sizeData$lad_age < timescale$age_bottom[i]]
	temp.count = data.frame(table(temp.data))$Freq
	my.tiering = rbind(my.tiering, temp.count)
	}

par(xaxs = "i", yaxs = "i")
my.col = c("red", "orange", "green", "cyan", "magenta", "blue")
source("https://github.com/naheim/paleosizePaper/raw/master/sharedCode/functions.r")
time.plot(c(0,range(my.tiering)), "Amount of Genera", mar = c(6, 3.5, 6, 3.5)+0.1, mgp = c(2.5, 0.75, 0))
#plot(1:10,1:10, type="n", xlim=c(541,-5), ylim=range(my.tiering), pch=21, xlab="Geologic Time (Ma)", ylab="Amount of Gentra")
for(i in 1:1:6){lines(timescale$age_bottom, my.tiering[,i], col=my.col[i], lwd = 3)}
abline(v = c(65, 200, 251.2, 443.8), col="black", lty = 5)
mtext(side=3, line=0.5, "The Change in the Amount of Gentra Categorized by Tiering Level Over Million-Years", col="black", font=4, cex=1.3)
legend(540, 1197, legend=c("Tiering Level 1: Pelagic", "Tiering Level 2: Erect", "Tiering Level 3: Surficial", "Tiering Level 4: Semi-infaunal", "Tiering Level 5: Shallow infaunal", "Tiering Level 6: Deep infaunal"), col = my.col, lty = 1, title="Tiering Levels:", bg = "white", box.col=NA, title.adj = 0.31)

**************************************************************************************************************************************************************************************************************************************************************************************************************************************************************

#2: Proportion of Each Tiering Lvl Over Time Stephanie Graph:

#CHANGE DIRECTORIES!

#WORKS:

sizeData <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/bodySizes.txt') 
sizeData <- subset(sizeData, !is.na(tiering) & tiering > 0)
timescale <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt') 
sizeData$tiering = factor(sizeData$tiering)
myProp = matrix(NA, nrow = nrow(timescale), ncol = 6)
my.col = c("red", "orange", "green", "cyan", "magenta", "blue")

for(i in 1:nrow(timescale)){temp <- subset(sizeData, sizeData$fad_age > timescale$age_top[i] & sizeData$lad_age < timescale$age_bottom[i])
counts <- table(temp$tiering)
myProp[i,] <- counts/sum(counts)
}

par(xaxs = "i", yaxs = "i")
source("https://github.com/naheim/paleosizePaper/raw/master/sharedCode/functions.r")
time.plot(c(0,1), "Proportion of Genera", mar = c(4, 3.5, 6, 3.5)+0.1, mgp = c(2.5, 0.75, 0))
#plot(1:10, type = "n", xlim = c(541,0), ylim = c(0,1), xlab="Geologic Time (Ma)", ylab="Porportion of Genera")
myX <- c(timescale$age_mid, rev(timescale$age_mid))
myRed <- c(rep(0, nrow(timescale)), rev(myProp[,1]))
polygon(myX, myRed, col="red")
myOrange <- c(myProp[,1], rev(myProp[,1]+myProp[,2]))
polygon(myX, myOrange, col="orange")
myGreen <- c(myProp[,1]+myProp[,2], rev(myProp[,1]+myProp[,2]+myProp[,3]))
polygon(myX, myGreen, col="green")
myCyan <- c(myProp[,1]+myProp[,2]+myProp[,3], rev(myProp[,1]+myProp[,2]+myProp[,3]+myProp[,4]))
polygon(myX, myCyan, col="cyan")
myMagenta <- c(myProp[,1]+myProp[,2]+myProp[,3]+myProp[,4], rev(myProp[,1]+myProp[,2]+myProp[,3]+myProp[,4]+myProp[,5]))
polygon(myX, myMagenta, col="magenta")
myBlue <- c(myProp[,1]+myProp[,2]+myProp[,3]+myProp[,4]+myProp[,5], rev(myProp[,1]+myProp[,2]+myProp[,3]+myProp[,4]+myProp[,5]+myProp[,6]))
polygon(myX, myBlue, col="blue")
mtext(side=3, line=0.5, "The Change in the Proportions of Gentra Categorized by Tiering Level Over Million-Years", col="black", font=4, cex=1.3)
my.col = c("red", "orange", "green", "cyan", "magenta", "blue")
par(xpd=TRUE)
legend(550, 1.16, legend=c("Tiering Level 1: Pelagic", "Tiering Level 2: Erect", "Tiering Level 3: Surficial", "Tiering Level 4: Semi-infaunal", "Tiering Level 5: Shallow infaunal", "Tiering Level 6: Deep infaunal"), col = my.col, lty = 1, title="Tiering Color Legend", bg = "NA", box.col=NA, title.adj = 0.26, cex=0.55, text.font = 4)

**************************************************************************************************************************************************************************************************************************************************************************************************************************************************************

#3: Mean Biovolume of Each Tiering Lvl Over Time

#CHANGE DIRECTORY!

bodySize <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/bodySizes.txt') 
timescale <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt') 
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
<<<<<<< HEAD
time.plot(c(0, 8), expression(paste("Biovolume (log  "[10]," cm"^3,")")), mar = c(3.5, 3.5, 3.5, 3.5)+0.1, mgp = c(2, 0.75, 0))
#plot(timescale$age_bottom, my.mean[,3], type="n", pch=16, xlab="Geologic Time (Ma)", xlim=c(541, 0), ylab="Mean Size", ylim=c(1.2,6.5), main="Mean expression(paste("Biovolume (log"[10]," mm"^3)"))
=======
time.plot(c(0, 8), expression(paste("Biovolume (log  "[10]," mm"^3,")")), mar = c(3.5, 3.5, 3.5, 3.5)+0.1, mgp = c(2, 0.75, 0))
#plot(timescale$age_bottom, my.mean[,3], type="n", pch=16, xlab="Geologic Time (Ma)", xlim=c(541, 0), ylab="Mean Size", ylim=c(1.2,6.5), main="Mean expression(paste("Biovolume (log"[10]," cm"^3)"))
>>>>>>> 03f296261301d7fe8a933f0329b14d87e1493cef
my.col = c("red", "orange", "green", "cyan", "magenta", "blue")
for(i in 1:6) {
  my.ts <- as.paleoTS(mm=my.mean[!is.na(my.var[,i]), i], vv=my.var[!is.na(my.var[,i]), i], nn=my.n[!is.na(my.var[,i]), i], tt=my.time[!is.na(my.var[,i])], oldest="last")
  fit3models(my.ts, method="Joint", pool=FALSE)
  #par(col=my.mean$color[k]); par(col="deepskyblue3")
  lines(timescale$age_mid, my.mean[, i], col=my.col[i])
}
mtext(side=3, line=0.5, "The Change in Mean Biovolume of Gentra Categorized by Tiering Level Over Million-Years", col="black", font=4, cex=1.3)
abline(v = c(65, 200, 251.2, 443.8), col="black")
par(col="black")
legend("topleft", legend=c("Tiering Level 1: Pelagic", "Tiering Level 2: Erect", "Tiering Level 3: Surficial", "Tiering Level 4: Semi-infaunal", "Tiering Level 5: Shallow infaunal", "Tiering Level 6: Deep infaunal"), col = my.col, lty = 1, title="Tiering Level", bg = "white", title.adj = 0.31, cex=1)

**************************************************************************************************************************************************************************************************************************************************************************************************************************************************************

#4: Body Size Box Plot for Each Tier/Biovolume vs. Tiering Level

par(col="black")
my.col = c("red", "orange", "green", "cyan", "magenta", "blue")
my.opp = c("chartreuse3", "dodgerblue2", "red", "darkorange1", "forestgreen", "orange2")
par(mar = c(5.5, 5.5, 2, 2)+0.1)
par(mgp = c(3, 1.7, 0))
boxplot(log10(max_vol)~tiering, bodySize, xlab="Tiering Level", ylab=expression(paste("Biovolume (log  "[10]," mm"^3,")")), col=my.col, names=c("Tiering Level 1:\n Pelagic", "Tiering Level 2:\n Erect", "Tiering Level 3:\n Surficial", "Tiering Level 4:\n Semi-infaunal", "Tiering Level 5:\n Shallow infaunal", "Tiering Level 6:\n Deep infaunal"), border = my.opp)
mtext(side=3, line=0.5, "Biovolume vs. Tiering Level", col="black", font=4, cex=1.3)

**************************************************************************************************************************************************************************************************************************************************************************************************************************************************************

#5: Stephanie's Reuse of Noel's Chart for Tiering Levels with Wrong Colors

sizeData <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/bodySizes.txt')
sizeData <- subset(sizeData, !is.na(tiering) & tiering > 0)
sizeData$log10_volume <- log10(sizeData$max_vol)
dim(sizeData)
head(sizeData)

timescale <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt')
source("https://github.com/naheim/paleosizePaper/raw/master/sharedCode/functions.r")

time.plot(c(-10, 15), expression(paste("Biovolume (log  "[10]," mm"^3,")")), mar=c(4,4,4,4), mgp = c(2, 0.75, 0))

#plot(1:10,1:10, type="n", xlim=c(550,0), ylim=c(-4,12), xlab="Geological time (Ma)", ylab=expression(paste("Biovolume (log  "[10]," cm"^3,")")))

segments(sizeData$fad_age, sizeData$log10_volume, sizeData$lad_age, sizeData$log10_volume)
myMean <- vector(mode="numeric", length=nrow(timescale))
my05 <- myMean
my95 <- myMean
myCols<- c("red", "orange", "green", "cyan", "magenta", "blue")
sizeData$color<-myCols[sizeData$tiering]
head(sizeData)
segments(sizeData$fad_age, sizeData$log10_volume, sizeData$lad_age, sizeData$log10_volume, col=sizeData$color)
#legend("topleft", lty = 1, title = "Tiering Levels:",legend = c("Tiering Level 1:\n Pelagic", "Tiering Level 2:\n Erect", "Tiering Level 3:\n Surficial", "Tiering Level 4:\n Semi-infaunal", "Tiering Level 5:\n Shallow infaunal", "Tiering Level 6:\n Deep infaunal"),col = c("red", "orange", "green", "cyan", "magenta", "blue"), cex = 1)
legend(540, 14.9, legend=c("Tiering Level 1: Pelagic", "Tiering Level 2: Erect", "Tiering Level 3: Surficial", "Tiering Level 4: Semi-infaunal", "Tiering Level 5: Shallow infaunal", "Tiering Level 6: Deep infaunal"), col = my.col, lty = 1, title="Tiering Levels:", bg = "white", box.col=NA, title.adj = 0.31)
mtext(side=3, line=0.5, "Body Size Evolution Catagorized by Tiering Level Over 541 Million-Years", col="black", font=4, cex=1.3)

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
lines(timescale$age_mid, myMeanNum1, col="red4", lwd=6) 

myMeanNum2 <- vector(mode="numeric", length=nrow(timescale))
for(i in 1:nrow(timescale)) {
	temp2<-Num2[Num2$fad_age > timescale$age_top[i] & Num2$lad_age < timescale$age_bottom[i], ]
	myMeanNum2[i]<-mean(temp2$log10_volume)
}
lines(timescale$age_mid, myMeanNum2, col="orange3", lwd=6)

myMeanNum3 <- vector(mode="numeric", length=nrow(timescale))
for(i in 1:nrow(timescale)) {
	temp3<-Num3[Num3$fad_age > timescale$age_top[i] & Num3$lad_age < timescale$age_bottom[i], ]
	myMeanNum3[i]<-mean(temp3$log10_volume)
}
lines(timescale$age_mid, myMeanNum3, col="forestgreen", lwd=6)

myMeanNum4 <- vector(mode="numeric", length=nrow(timescale))
for(i in 1:nrow(timescale)) {
	temp4<-Num4[Num4$fad_age > timescale$age_top[i] & Num4$lad_age < timescale$age_bottom[i], ]
	myMeanNum4[i]<-mean(temp4$log10_volume)
}
lines(timescale$age_mid, myMeanNum4, col="cyan3", lwd=6)

myMeanNum5 <- vector(mode="numeric", length=nrow(timescale))
for(i in 1:nrow(timescale)) {
	temp5<-Num5[Num5$fad_age > timescale$age_top[i] & Num5$lad_age < timescale$age_bottom[i], ]
	myMeanNum5[i]<-mean(temp5$log10_volume)
}
lines(timescale$age_mid, myMeanNum5, col="magenta3", lwd=6)

myMeanNum6 <- vector(mode="numeric", length=nrow(timescale))
for(i in 1:nrow(timescale)) {
	temp6<-Num6[Num6$fad_age > timescale$age_top[i] & Num6$lad_age < timescale$age_bottom[i], ]
	myMeanNum6[i]<-mean(temp6$log10_volume)
}
lines(timescale$age_mid, myMeanNum6, col="blue3", lwd=6)

**************************************************************************************************************************************************************************************************************************************************************************************************************************************************************

#MultiGraph with Previous Stephanie Graph

sizeData <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/bodySizes.txt')
sizeData <- subset(sizeData, !is.na(tiering) & tiering > 0)
sizeData$log10_volume <- log10(sizeData$max_vol)
dim(sizeData)
head(sizeData)

timescale <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt')
source("https://github.com/naheim/paleosizePaper/raw/master/sharedCode/functions.r")

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
 segments(Num1$fad_age,Num1$log10max_vol,Num1$lad_age,Num1$log10max_vol, col="red")
 title(main="Tiering Level 1: Pelagic")
 meanVector <- vector(mode='numeric', length=nrow(timescale))
 for(i in 1:nrow(timescale)) { 
  	meanVector[i] <- mean(Num1$log10max_vol[Num1$fad_age > timescale$age_top[i] & Num1$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=4)

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log  "[10]," mm"^3*")")))
segments(Num2$fad_age,Num2$log10max_vol,Num2$lad_age,Num2$log10max_vol, col="orange")
meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
  	meanVector[i] <- mean(Num2$log10max_vol[Num2$fad_age > timescale$age_top[i] & Num2$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=4)
 title(main="Tiering Level 2: Erect")

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log  "[10]," mm"^3*")")))
segments(Num3$fad_age,Num3$log10max_vol,Num3$lad_age,Num3$log10max_vol, col="green")
 title(main="Tiering Level 3: Surficial")
 meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
 	meanVector[i] <- mean(Num3$log10max_vol[Num3$fad_age > timescale$age_top[i] & Num3$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=4)

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
segments(Num4$fad_age,Num4$log10max_vol,Num4$lad_age,Num4$log10max_vol, col="cyan")
title(main="Tiering Level 4: Semi-infaunal")
 meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
 	meanVector[i] <- mean(Num4$log10max_vol[Num4$fad_age > timescale$age_top[i] & Num4$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=4)

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log  "[10]," mm"^3*")")))
segments(Num5$fad_age,Num5$log10max_vol,Num5$lad_age,Num5$log10max_vol, col="magenta")
title(main="Tiering Level 5: Shallow infaunal")
meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
 	meanVector[i] <- mean(Num5$log10max_vol[Num5$fad_age > timescale$age_top[i] & Num5$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=4)

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log  "[10]," mm"^3*")")))
segments(Num6$fad_age,Num6$log10max_vol,Num6$lad_age,Num6$log10max_vol, col="blue")
title(main="Tiering Level 6: Deep infaunal")
meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
 	meanVector[i] <- mean(Num6$log10max_vol[Num6$fad_age > timescale$age_top[i] & Num6$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=4)

**************************************************************************************************************************************************************************************************************************************************************************************************************************************************************

#Stephanie Logistic Regression Combined Tiering Graph 

source("https://github.com/naheim/paleosizePaper/raw/master/sharedCode/functions.r")
sizeData <- read.delim(file="https://github.com/naheim/paleosizePaper/raw/master/rawDataFiles/bodySizes.txt")
timescale <- read.delim(file="https://github.com/naheim/paleosizePaper/raw/master/rawDataFiles/timescale.txt")
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
	plot(timescale$age_mid[tierExtSel$coef > -5 & tierExtSel$coef < 5], tierExtSel$coef[tierExtSel$coef > -5 & tierExtSel$coef < 5], pch=16, cex=1.25, xaxt="n", xlab="", xlim=c(541,0), ylim=c(-2.5,2.5), col=myCols[j], ylab=" Log-Odds of Extinction")
	abline(h=0, lty=2)
	segments(timescale$age_mid,tierExtSel$ci.minus,timescale$age_mid,tierExtSel$ci.plus)
	title(main= paste("Tiering ", j, ": ", tierLabs[j], sep=""))
}

**************************************************************************************************************************************************************************************************************************************************************************************************************************************************************

#Tiering Akike Walks 

bodySize <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/bodySizes.txt') 
timescale <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt') 
bodySize <- subset(sizeData, !is.na(tiering) & tiering != 0)
=======
#Final Graph/Data

bodySize <- read.delim("bodySizes.txt") 
timescale <- read.delim("timescale.txt") 
bodySize <- subset(bodySize, !is.na(tiering) & tiering != 0)
>>>>>>> 03f296261301d7fe8a933f0329b14d87e1493cef

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
allTS <- list()
for (i in 1:1:6){my.ts <- as.paleoTS(mm=my.mean[!is.na(my.var[,i]),i], vv=my.var[!is.na(my.var[,i]),i], nn=my.n[!is.na(my.var[,i]),i], tt=my.time[!is.na(my.var[,i])], oldest="last")  
fit3models(my.ts, method="Joint", pool=FALSE)
allTS[[i]] <- my.ts
}

**************************************************************************************************************************************************************************************************************************************************************************************************************************************************************
