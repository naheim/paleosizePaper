# Akaike Weights

library(paleoTS) # setting up paleoTS functions
sizeData<-read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/bodySizes.txt') #reading in data set
sizeData$log10_volume<-log10(sizeData$max_vol) #adding column to dataset to make log volume of data
sizeData$combined_resp<-paste(sizeData$fluid, sizeData$respOrgan, sizeData$circ) #combining resp mode
sizeData <- subset(sizeData, is.element(combined_resp, c("air dedicated closed","water dedicated closed","water dedicated open","water multi open")))
sizeData$combined_resp <- factor(sizeData$combined_resp) #taking out data that has small sample size
timescale <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt')
nBins <- nrow(timescale)
myMean <- vector(mode="numeric", length=nBins)
myVar <- myMean
myN <- myVar
myTime <- timescale$age_bottom

names(myVar) <- timescale$interval_name
names(myN) <- timescale$interval_name
names(myTime) <- timescale$interval_name

WaDeCl<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="closed"),]
WaDeOp<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="open"),]
WaMuOp<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="multi" & sizeData[,"circ"]=="open"),]
AiDeCl<-sizeData[which(sizeData[,"fluid"]=="air" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="closed"),]

for(i in 1:nBins) {
	temp<-WaDeCl[WaDeCl$fad_age > timescale$age_top[i] & WaDeCl$lad_age < timescale$age_bottom[i], ]
	myMean[i]<-mean(temp$log10_volume)
	myVar[i]<-var(temp$log10_volume) 
	myN[i]<-length(temp$log10_volume)
}
myTS <- as.paleoTS(mm=myMean[!is.na(myVar)], vv=myVar[!is.na(myVar)], nn=myN[!is.na(myVar)], tt=myTime[!is.na(myVar)], oldest="last")
fit3models(myTS, method="Joint", pool=FALSE)

myMean1 <- vector(mode="numeric", length=nBins)
myVar1 <- myMean1
myN1 <- myVar1
names(myVar1) <- timescale$interval_name
names(myN1) <- timescale$interval_name
for(i in 1:nBins) {
	temp1<-WaDeOp[WaDeOp$fad_age > timescale$age_top[i] & WaDeOp$lad_age < timescale$age_bottom[i], ]
	myMean1[i]<-mean(temp1$log10_volume)
	myVar1[i]<-var(temp1$log10_volume) 
	myN1[i]<-length(temp1$log10_volume)
}
myTS1 <- as.paleoTS(mm=myMean1[!is.na(myVar1)], vv=myVar1[!is.na(myVar1)], nn=myN1[!is.na(myVar1)], tt=myTime[!is.na(myVar1)], oldest="last")
fit3models(myTS1, method="Joint", pool=FALSE)

myMean2 <- vector(mode="numeric", length=nBins)
myVar2 <- myMean2
myN2 <- myVar2
names(myVar2) <- timescale$interval_name
names(myN2) <- timescale$interval_name
for(i in 1:nBins) {
	temp2<-WaMuOp[WaMuOp$fad_age > timescale$age_top[i] & WaMuOp$lad_age < timescale$age_bottom[i], ]
	myMean2[i]<-mean(temp2$log10_volume)
	myVar2[i]<-var(temp2$log10_volume) 
	myN2[i]<-length(temp2$log10_volume)
}
myTS2 <- as.paleoTS(mm=myMean2[!is.na(myVar2)], vv=myVar2[!is.na(myVar2)], nn=myN2[!is.na(myVar2)], tt=myTime[!is.na(myVar2)], oldest="last")
fit3models(myTS2, method="Joint", pool=FALSE)

myMean3 <- vector(mode="numeric", length=nBins)
myVar3 <- myMean3
myN3 <- myVar3
names(myVar3) <- timescale$interval_name
names(myN3) <- timescale$interval_name
for(i in 1:nBins) {
	temp3<-AiDeCl[AiDeCl$fad_age > timescale$age_top[i] & AiDeCl$lad_age < timescale$age_bottom[i], ]
	myMean3[i]<-mean(temp3$log10_volume)
	myVar3[i]<-var(temp3$log10_volume) 
	myN3[i]<-length(temp3$log10_volume)
}
myTS3 <- as.paleoTS(mm=myMean3[!is.na(myVar3)], vv=myVar3[!is.na(myVar3)], nn=myN3[!is.na(myVar3)], tt=myTime[!is.na(myVar3)], oldest="last")
fit3models(myTS3, method="Joint", pool=FALSE)

#
#

# Proportion Graph

source("https://github.com/naheim/paleosizePaper/raw/master/sharedCode/functions.r")
sizeData <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/bodySizes.txt')
timescale <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt')
nBins <- nrow(timescale)
sizeData$log10_volume<-log10(sizeData$max_vol)
sizeData$combined_resp<-paste(sizeData$fluid, sizeData$respOrgan, sizeData$circ)
sizeData <- subset(sizeData, is.element(combined_resp, c("air dedicated closed","water dedicated closed","water dedicated open","water multi open")))
sizeData$combined_resp <- factor(sizeData$combined_resp)

WaDeCl<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="closed"),]
WaDeOp<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="open"),]
WaMuOp<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="multi" & sizeData[,"circ"]=="open"),]
AiDeCl<-sizeData[which(sizeData[,"fluid"]=="air" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="closed"),]

sysProp <- data.frame(matrix(NA, nrow=nBins, ncol=4, dimnames=list(timescale$interval_name, levels(sizeData$combined_resp))))
for(i in 1:nBins) {
	temp <- sizeData[sizeData$fad_age > timescale$age_top[i] & sizeData$lad_age < timescale$age_bottom[i], ]
	respCounts <- table(temp$combined_resp)
	respProp <- respCounts/sum(respCounts)
	sysProp[i,] <- respProp
}

xPoly <- c(timescale$age_mid, rev(timescale$age_mid))
yPoly1 <- c(rep(0, nBins), rev(sysProp[,"water.dedicated.closed"]))
yPoly2 <- c(sysProp[,"water.dedicated.closed"], rev(sysProp[,"water.dedicated.closed"] + sysProp[,"water.dedicated.open"]))
yPoly3 <- c(sysProp[,"water.dedicated.closed"] + sysProp[,"water.dedicated.open"], rev(sysProp[,"water.dedicated.closed"] + sysProp[,"water.dedicated.open"] + sysProp[,"water.multi.open"]))
yPoly4 <- c(sysProp[,"water.dedicated.closed"] + sysProp[,"water.dedicated.open"] + sysProp[,"water.multi.open"], rev(sysProp[,"air.dedicated.closed"] + sysProp[,"water.dedicated.closed"] + sysProp[,"water.dedicated.open"] + sysProp[,"water.multi.open"]))

classCols <- c("#ff5640","#ffd900","#00ffd7","#ee92ed")
time.plot(c(0,1), "Proportion of Genera", main="Time Series of Proportion of Respiratory Classes", mar=c(3.5,3.5,4,4))

polygon(xPoly,yPoly1, col=classCols[1])
polygon(xPoly,yPoly2, col=classCols[2])
polygon(xPoly,yPoly3, col=classCols[3])
polygon(xPoly,yPoly4, col=classCols[4])
abline(v=443.8, lty=5, col="azure4")
abline(v=358.9, lty=5, col="azure4")
abline(v=252.17, lty=5, col="azure4")
abline(v=201.3, lty=5, col="azure4")
abline(v=66, lty=5, col="azure4")
legend(479,0.96, legend=c("Water, Dedicated organ, Closed system", "Water, Dedicated organ, Open system", "Water, Multi-organ, Open system", "Air, Dedicated organ, Closed system"), fill=c("#ff5640", "#ffd900", "#00ffd7", "#ee92ed"), title="Respiratory System Types", bg="white", cex=0.63, box.col="white")

#
#

# Stratigraphic Mean Plot

source("https://github.com/naheim/paleosizePaper/raw/master/sharedCode/functions.r")
sizeData<-read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/bodySizes.txt') #reading in data set
sizeData$log10_volume<-log10(sizeData$max_vol) #adding column to dataset to make log volume of data
sizeData$combined_resp<-paste(sizeData$fluid, sizeData$respOrgan, sizeData$circ) #adding a combined column to dataset to sort out respiration types
sizeData <- subset(sizeData, is.element(combined_resp, c("air dedicated closed","water dedicated closed","water dedicated open","water multi open")))
sizeData$combined_resp <- factor(sizeData$combined_resp) #taking out data that isn't sorted into any category and taking out organisms that have respiration systems: water, multi organ, closed, because there aren't enough examples to make definite conclusions
# table(sizeData$combined_resp) shows how many different variations there are with how many values in each category
timescale <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt') #reading in timescale
time.plot(c(-2,12), "", main="Body Size Evolution as Divided by Respiratory Types", mar=c(3.5,3.5,4,4))
#plot(1:10,1:10, type="n", xlim=c(550,0), ylim=c(-2,12), xlab="Geological time (Ma)", ylab="", main="Body Size Evolution as Divided by Respiratory Types") #setting up graph plot

title(ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")), line=1.5) #offsetting y-axis label in plot bc superscript is cut off
# expression(paste("Biovolume (log"[10]," cm"^3*")"))

#making subsets of different types of usable respiration combinations
WaDeCl<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="closed"),]
WaDeOp<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="open"),]
WaMuOp<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="multi" & sizeData[,"circ"]=="open"),]
AiDeCl<-sizeData[which(sizeData[,"fluid"]=="air" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="closed"),]

#adding segments to show max volume in log of different species plotted as y. x is plotted as when each species started and ended
segments(WaDeCl$fad_age, WaDeCl$log10_volume, WaDeCl$lad_age, WaDeCl$log10_volume, col="#ff5640", lwd=0.25)
segments(WaDeOp$fad_age, WaDeOp$log10_volume, WaDeOp$lad_age, WaDeOp$log10_volume, col="#ffd900", lwd=0.25)
segments(WaMuOp$fad_age, WaMuOp$log10_volume, WaMuOp$lad_age, WaMuOp$log10_volume, col="#00ffd7", lwd=0.25)
segments(AiDeCl$fad_age, AiDeCl$log10_volume, AiDeCl$lad_age, AiDeCl$log10_volume, col="#ee92ed", lwd=0.25)

#calculating mean of each subset at each time interval

myMeanWDC <- vector(mode="numeric", length=nrow(timescale)) #creates empty vector that can be filled with time interval - mean data
for(i in 1:nrow(timescale)) {
	temp<-WaDeCl[WaDeCl$fad_age > timescale$age_top[i] & WaDeCl$lad_age < timescale$age_bottom[i], ] #makes temporary subset of data for each time interval
	myMeanWDC[i]<-mean(temp$log10_volume) #adds calculated mean for each time interval progressively to empty vector
}
lines(timescale$age_mid, myMeanWDC, col="#ff5640", lwd=4) #adds line of mean sizes at each time interval

myMeanWDO <- vector(mode="numeric", length=nrow(timescale)) #loop as indicated above has to be repeated for each category of respiration
for(i in 1:nrow(timescale)) {
	temp1<-WaDeOp[WaDeOp$fad_age > timescale$age_top[i] & WaDeOp$lad_age < timescale$age_bottom[i], ]
	myMeanWDO[i]<-mean(temp1$log10_volume)
}
lines(timescale$age_mid, myMeanWDO, col="#ffd900", lwd=4)

myMeanWMO <- vector(mode="numeric", length=nrow(timescale))
for(i in 1:nrow(timescale)) {
	temp2<-WaMuOp[WaMuOp$fad_age > timescale$age_top[i] & WaMuOp$lad_age < timescale$age_bottom[i], ]
	myMeanWMO[i]<-mean(temp2$log10_volume)
}
lines(timescale$age_mid, myMeanWMO, col="#00ffd7", lwd=4)

myMeanADC <- vector(mode="numeric", length=nrow(timescale))
for(i in 1:nrow(timescale)) {
	temp3<-AiDeCl[AiDeCl$fad_age > timescale$age_top[i] & AiDeCl$lad_age < timescale$age_bottom[i], ]
	myMeanADC[i]<-mean(temp3$log10_volume)
}
lines(timescale$age_mid, myMeanADC, col="#ee92ed", lwd=4)

abline(v=443.8, lty=5, col="azure4")
abline(v=358.9, lty=5, col="azure4")
abline(v=252.17, lty=5, col="azure4")
abline(v=201.3, lty=5, col="azure4")
abline(v=66, lty=5, col="azure4")
legend(520, 11.44, legend=c("Water, Dedicated organ, Closed system", "Water, Dedicated organ, Open system", "Water, Multi-organ, Open system", "Air, Dedicated organ, Closed system"), fill=c("#ff5640", "#ffd900", "#00ffd7", "#ee92ed"), title="Repiratory System Types", cex=0.8, bg="white", box.col="white") #makes legend for each respiration type

#
#

# Boxplot

source("https://github.com/naheim/paleosizePaper/raw/master/sharedCode/functions.r")
sizeData <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/bodySizes.txt')
timescale <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt')
nBins <- nrow(timescale)
sizeData$log10_volume<-log10(sizeData$max_vol)
sizeData$combined_resp<-paste(sizeData$fluid, sizeData$respOrgan, sizeData$circ)
sizeData <- subset(sizeData, is.element(combined_resp, c("air dedicated closed","water dedicated closed","water dedicated open","water multi open")))
sizeData$combined_resp <- factor(sizeData$combined_resp)
table(sizeData$combined_resp)
quartz()
boxplot(sizeData$log10_volume~sizeData$combined_resp, main="Body Size vs Respiratory Mode Boxplot", xlab="", notch=TRUE, ylab="", names=NA, col=c("#ee92ed","#ff5640", "#ffd900", "#00ffd7"))
mtext(side=1, line=0.8, at=1, text="Air, Dedicated, Closed")
mtext(side=1, line=2.8, at=2, text="Water, Dedicated, Closed")
mtext(side=1, line=0.8, at=3, text="Water, Dedicated, Open")
mtext(side=1, line=2.8, at=4, text="Water, Multi-function, Open")
title(ylab=expression(paste("Biovolume (log"[10]," mm"^3,")")), line=2.2)

#