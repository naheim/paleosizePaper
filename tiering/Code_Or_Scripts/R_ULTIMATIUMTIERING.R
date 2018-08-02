#TOTAL GRAPH SINGLE SCRIPT W/O EXTRA GRAPHS:

**************************************************************************************************************************************************************************************************************************************************************************************************************************************************************
#1: # of Genera in Each Tiering Lvl Over Time:

#CHANGE DIRECTORIES!

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

time.plot(c(0, max(my.tiering)), "Amount of Genera", cex.lab = 1.2, mar = c(4.5,4.5,4.5,4.5)+0.1, mgp=c(3, 0.75, 0), cex.axis = 1.25)

#plot(1:10,1:10, type="n", xlim=c(541,-5), ylim=range(my.tiering), pch=21, xlab="Geologic Time (Ma)", ylab="Amount of Genera")
for(i in 1:1:6){lines(timescale$age_bottom, my.tiering[,i], col=my.col[i], lwd = 3)}
abline(v = c(443.8, 358.9, 251, 200, 65.5), col="azure4", lty = 5)
mtext(side=3, line=0.5, "The Change in Amount of Genera Categorized by Tiering Level Over 541 Million Years", col="black", font=4, cex=2)
legend(540, 1197, legend=c("Tiering Level 1: Pelagic", "Tiering Level 2: Erect", "Tiering Level 3: Surficial", "Tiering Level 4: Semi-infaunal", "Tiering Level 5: Shallow infaunal", "Tiering Level 6: Deep infaunal"), col = my.col, lty = 1, title="Tiering Levels:", bg = "white", box.col=NA, cex=1)

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
#3: Mean Biovolume of Each Tiering Lvl Over Time

#CHANGE DIRECTORY!

bodySize <- read.delim(file="bodySizes.txt")
timescale <- read.delim("timescale.txt") 
bodySize <- subset(bodySize, !is.na(tiering) & tiering != 0)
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
time.plot(c(0, 8), expression(paste("Biovolume (log  "[10]," mm"^3,")")), cex.lab = 1.2, mar = c(4.5,4.5,4.5,4.5)+1, mgp=c(3, 0.75, 0), cex.axis = 1.25)
my.col = c("#ff5640","#ffd900","#00ffd7","#ee92ed","#ff00ff","#0000ff")
for(i in 1:6) {
  my.ts <- as.paleoTS(mm=my.mean[!is.na(my.var[,i]), i], vv=my.var[!is.na(my.var[,i]), i], nn=my.n[!is.na(my.var[,i]), i], tt=my.time[!is.na(my.var[,i])], oldest="last")
  fit3models(my.ts, method="Joint", pool=FALSE)
  #par(col=my.mean$color[k]); par(col="deepskyblue3")
  lines(timescale$age_mid, my.mean[, i], col=my.col[i], lwd = 3)
}
mtext(side=3, line=0.5, "The Change in the Mean Biovolume of Tiering Levels Over the Phanerozoic", col="black", font=4, cex=2)
par(col="black")
abline(v = c(443.8, 359.2, 251, 199.5, 65.5), col="azure4", lty = 5)
legend(540, 7.98, legend=c("Tiering Level 1: Pelagic", "Tiering Level 2: Erect", "Tiering Level 3: Surficial", "Tiering Level 4: Semi-infaunal", "Tiering Level 5: Shallow infaunal", "Tiering Level 6: Deep infaunal"), col = my.col, lty = 1, title="Tiering Level", bg = "white", box.col=NA, cex=1)

**************************************************************************************************************************************************************************************************************************************************************************************************************************************************************
#4: Body Size Box Plot for Each Tier/Biovolume vs. Tiering Level

#CHANGE DIRECTORIES!

sizeData <- read.delim("bodySizes.txt") 
timescale <- read.delim("timescale.txt") 
sizeData = subset(sizeData, !is.na(tiering) & tiering != 0)
par(col="black")
my.col = c("#ff5640","#ffd900","#00ffd7","#ee92ed","#ff00ff","#0000ff")
par(mar = c(5, 5, 5, 5)+0.1)
par(mgp = c(2.5, 1.5, 0))
boxplot(log10(max_vol)~tiering, bodySize, col=my.col, names=c("Pelagic", "Erect", "Surficial", "Semi-infaunal", "Shallow infaunal", "Deep infaunal"), notch=TRUE)
mtext(side=1, line=3.2, "Tiering Level", col="black", font=1, cex=1.2)
mtext(side=2, line=3.2, expression(paste("Biovolume (log  "[10]," mm"^3,")")), col="black", font=4, cex=1.2)
mtext(side=3, line=0.5, "Biovolume Box & Whisker Plot Catagorized by Tiering Level", col="black", font=4, cex=2)

**************************************************************************************************************************************************************************************************************************************************************************************************************************************************************

#6: Stephanie's MultiGraph Plotting Mean Biovolume Over Time

#CHANGE DIRECTORIES!

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
#8: PaleoTS Akike Walks 

#CHANGE DIRECTORIES!

sizeData <- read.delim("bodySizes.txt") 
timescale <- read.delim("timescale.txt") 
sizeData = subset(sizeData, !is.na(tiering) & tiering != 0)
library(paleoTS)
n.bins <- nrow(timescale)
my.mean <- vector(mode="numeric", length=n.bins) 
my.var <- vector(mode="numeric", length=n.bins)
my.n <- vector(mode="numeric", length=n.bins)
my.time <- timescale$age_bottom
names(my.mean) <- timescale$interval_name
names(my.var) <- timescale$interval_name
names(my.n) <- timescale$interval_name
names(my.time) <- timescale$interval_name

#Tiering Level 1
T1Data <- sizeData[which(sizeData["tiering"] == 1),]
for(i in 1:n.bins) { 
	temp.data <- log10(T1Data$max_vol[T1Data$fad_age > timescale$age_top[i] & T1Data$lad_age < timescale$age_bottom[i]])
	my.mean[i] <- mean(temp.data)  
	my.var[i] <- var(temp.data)  
	my.n[i] <- length(temp.data)  
}
my.ts <- as.paleoTS(mm=my.mean[!is.na(my.var)], vv=my.var[!is.na(my.var)], nn=my.n[!is.na(my.var)], tt=my.time[!is.na(my.var)], oldest="last") 
fit3models(my.ts, method="Joint", pool=FALSE)

#Tiering Level 2
T2Data <- sizeData[which(sizeData["tiering"] == 2),]
for(i in 1:n.bins) { 
	temp.data <- log10(T2Data$max_vol[T2Data$fad_age > timescale$age_top[i] & T2Data$lad_age < timescale$age_bottom[i]])
	my.mean[i] <- mean(temp.data)  
	my.var[i] <- var(temp.data)  
	my.n[i] <- length(temp.data)  
}
my.ts <- as.paleoTS(mm=my.mean[!is.na(my.var)], vv=my.var[!is.na(my.var)], nn=my.n[!is.na(my.var)], tt=my.time[!is.na(my.var)], oldest="last") 
fit3models(my.ts, method="Joint", pool=FALSE)


#Tiering Level 3
T3Data <- sizeData[which(sizeData["tiering"] == 3),]
for(i in 1:n.bins) { 
	temp.data <- log10(T3Data$max_vol[T3Data$fad_age > timescale$age_top[i] & T3Data$lad_age < timescale$age_bottom[i]])
	my.mean[i] <- mean(temp.data)  
	my.var[i] <- var(temp.data)  
	my.n[i] <- length(temp.data)  
}
my.ts <- as.paleoTS(mm=my.mean[!is.na(my.var)], vv=my.var[!is.na(my.var)], nn=my.n[!is.na(my.var)], tt=my.time[!is.na(my.var)], oldest="last") 
fit3models(my.ts, method="Joint", pool=FALSE)


#Tiering Level 4
T4Data <- sizeData[which(sizeData["tiering"] == 4),]
for(i in 1:n.bins) { 
	temp.data <- log10(T4Data$max_vol[T4Data$fad_age > timescale$age_top[i] & T4Data$lad_age < timescale$age_bottom[i]])
	my.mean[i] <- mean(temp.data)  
	my.var[i] <- var(temp.data)  
	my.n[i] <- length(temp.data)  
}
my.ts <- as.paleoTS(mm=my.mean[!is.na(my.var)], vv=my.var[!is.na(my.var)], nn=my.n[!is.na(my.var)], tt=my.time[!is.na(my.var)], oldest="last") 
fit3models(my.ts, method="Joint", pool=FALSE)

#Tiering Level 5
T5Data <- sizeData[which(sizeData["tiering"] == 5),]
for(i in 1:n.bins) { 
	temp.data <- log10(T5Data$max_vol[T5Data$fad_age > timescale$age_top[i] & T5Data$lad_age < timescale$age_bottom[i]])
	my.mean[i] <- mean(temp.data)  
	my.var[i] <- var(temp.data)  
	my.n[i] <- length(temp.data)  
}
my.ts <- as.paleoTS(mm=my.mean[!is.na(my.var)], vv=my.var[!is.na(my.var)], nn=my.n[!is.na(my.var)], tt=my.time[!is.na(my.var)], oldest="last") 
fit3models(my.ts, method="Joint", pool=FALSE)

#Tiering Level 6
T6Data <- sizeData[which(sizeData["tiering"] == 6),]
for(i in 1:n.bins) { 
	temp.data <- log10(T6Data$max_vol[T6Data$fad_age > timescale$age_top[i] & T6Data$lad_age < timescale$age_bottom[i]])
	my.mean[i] <- mean(temp.data)  
	my.var[i] <- var(temp.data)  
	my.n[i] <- length(temp.data)  
}
my.ts <- as.paleoTS(mm=my.mean[!is.na(my.var)], vv=my.var[!is.na(my.var)], nn=my.n[!is.na(my.var)], tt=my.time[!is.na(my.var)], oldest="last") 
fit3models(my.ts, method="Joint", pool=FALSE)

