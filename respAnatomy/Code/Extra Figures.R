# Extinction Selectivity


# 	Combined Graph

source("https://github.com/naheim/paleosizePaper/raw/master/sharedCode/functions.r")
sizeData<-read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/bodySizes.txt') #read in data set
sizeData$log10_volume<-log10(sizeData$max_vol) #add colummn to data set to take log of max volume of all species
sizeData$combined_resp<-paste(sizeData$fluid, sizeData$respOrgan, sizeData$circ) #adding a combined column to dataset to sort out respiration types
sizeData$combined_resp <- factor(sizeData$combined_resp)
sizeData <- subset(sizeData, is.element(combined_resp, c("air dedicated closed","water dedicated closed","water dedicated open","water multi open")))
timescale <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt') #reading in timescale

#making subsets of different types of usable respiration combinations
WaDeCl<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="closed"),]
WaDeOp<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="open"),]
WaMuOp<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="multi" & sizeData[,"circ"]=="open"),]
AiDeCl<-sizeData[which(sizeData[,"fluid"]=="air" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="closed"),]

time.plot(c(-2,1), "Slope of Regression Coeffecient for Extinction Estimated by Volume", main="Time Series of Extinction Selectivity as Estimated by Body Size", x.axis.pct=18, mar=c(3,2.75,2,1))
	#plot(1:10,1:10, type="n", xlim=c(550,0), ylim=c(-2,1), xlab="Geological time (Ma)", ylab="Slope of Regression Coeffecient for Extinction Estimated by Volume", main="Time Series of Extinction Selectivity as Estimated by Body Size") #setting up graph plot

myRegWDC <- vector(mode="numeric", length=nrow(timescale)) #making empty vector that can be filled with correlation coefficient for y~x being extinct~bodysize, where extinct = 1 means it goes extinct. a negative value means that as body size increases, organisms are less likely to go extinct. vice versa for positive value
myPropWDC <- vector(mode="numeric", length=nrow(timescale)) #empty vector that can be filled with proportion of genera that go extinct in a time period
for(i in 1:nrow(timescale)) { #making loop for filling in vector
	temp<-WaDeCl[WaDeCl$fad_age > timescale$age_top[i] & WaDeCl$lad_age < timescale$age_bottom[i], ] #creates temp data that subsets all organisms contained in a time period
	temp$extinct <- 0 #creates a new column called extinct where every organism is set to extant
	temp$extinct[temp$lad_age < timescale$age_bottom[i] & temp$lad_age >= timescale$age_top[i]] <- 1 #filters through all organisms of the interval that become extinct and sets them to 1 in the extinct column
	if(sum(temp$extinct) >= 3 & nrow(temp)-sum(temp$extinct) >= 3) { #insures that if there is not enough data in an interval, it is not calculated and the correlation calculation is skipped over. Prevents huge correlation coefficients that aren't statistically significant
		glmEqn <- glm(extinct ~ log10_volume, family="binomial", data=temp) #does regression calculation for extinct as estimated by volume for time interval
		myRegWDC[i]<- glmEqn$coefficients[2] #inserts value for coefficient into vector
	}
	myPropWDC[i] <- sum(temp$extinct)/nrow(temp) #calculating the proprtion of genera that go extinct in time period
}
lines(timescale$age_mid, myRegWDC, col="#ff5640", lwd=2) #adds line of vector above

myRegWDO <- vector(mode="numeric", length=nrow(timescale))
myPropWDO <- vector(mode="numeric", length=nrow(timescale))
for(i in 1:nrow(timescale)) {
	temp1<-WaDeOp[WaDeOp$fad_age > timescale$age_top[i] & WaDeOp$lad_age < timescale$age_bottom[i], ]
	temp1$extinct <- 0
	temp1$extinct[temp1$lad_age < timescale$age_bottom[i] & temp1$lad_age >= timescale$age_top[i]] <- 1
	if(sum(temp1$extinct) >= 3 & nrow(temp1)-sum(temp1$extinct) >= 3) {
		glmEqn1 <- glm(extinct ~ log10_volume, family="binomial", data=temp1)
		myRegWDO[i]<- glmEqn1$coefficients[2]
		}
	myPropWDO[i] <- sum(temp1$extinct)/nrow(temp1)
}
lines(timescale$age_mid, myRegWDO, col="#ffd900", lwd=2)

myRegWMO <- vector(mode="numeric", length=nrow(timescale))
myPropWMO <- vector(mode="numeric", length=nrow(timescale))
for(i in 1:nrow(timescale)) {
	temp2<-WaMuOp[WaMuOp$fad_age > timescale$age_top[i] & WaMuOp$lad_age < timescale$age_bottom[i], ]
	temp2$extinct <- 0
	temp2$extinct[temp2$lad_age < timescale$age_bottom[i] & temp2$lad_age >= timescale$age_top[i]] <- 1
	if(sum(temp2$extinct) >= 3 & nrow(temp2)-sum(temp2$extinct) >= 3) {
		glmEqn2 <- glm(extinct ~ log10_volume, family="binomial", data=temp2)
		myRegWMO[i]<- glmEqn2$coefficients[2]
	}
	myPropWMO[i] <- sum(temp2$extinct)/nrow(temp2)
}
lines(timescale$age_mid, myRegWMO, col="#00ffd7", lwd=2)

myRegADC <- vector(mode="numeric", length=nrow(timescale))
myPropADC <- vector(mode="numeric", length=nrow(timescale))
for(i in 1:nrow(timescale)) {
	temp3<-AiDeCl[AiDeCl$fad_age > timescale$age_top[i] & AiDeCl$lad_age < timescale$age_bottom[i], ]
	if(nrow(temp3) > 0){
		temp3$extinct <- 0
		temp3$extinct[temp3$lad_age < timescale$age_bottom[i] & temp3$lad_age >= timescale$age_top[i]] <- 1
		if(sum(temp3$extinct) >= 3 & nrow(temp3)-sum(temp3$extinct) >= 3) {
			glmEqn3 <- glm(extinct ~ log10_volume, family="binomial", data=temp3)
			myRegADC[i]<- glmEqn3$coefficients[2]
		}
	}
	myPropADC[i] <- sum(temp3$extinct)/nrow(temp3)
}
lines(timescale$age_mid, myRegADC, col="#ee92ed", lwd=2)
legend(520, -1.38, legend=c("Water, Dedicated organ, Closed system", "Water, Dedicated organ, Open system", "Water, Multi-organ, Open system", "Air, Dedicated organ, Closed system"), fill=c("#ff5640", "#ffd900", "#00ffd7", "#ee92ed"), title="Repiratory System Types", cex=0.8) #makes legend for each respiration type

#	Separate Graphs

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

wdcExtSel <- data.frame(matrix(NA, nrow=nBins, ncol=3, dimnames=list(timescale$interval_name, c('coef','ci.minus','ci.plus'))))
for(i in 1:nBins) {
	temp <- WaDeCl[WaDeCl$fad_age > timescale$age_top[i] & WaDeCl$lad_age < timescale$age_bottom[i], ]
	if(nrow(temp) > 0) {
		temp$extinct <- 0
		temp$extinct[temp$lad_age < timescale$age_bottom[i] & temp$lad_age >= timescale$age_top[i]] <- 1	
		if(sum(temp$extinct) >= 3 & nrow(temp) >= 6) {
			myGlm <- glm(extinct ~ log10_volume, data=temp, family=binomial)
			wdcExtSel$coef[i] <- myGlm$coefficients[2]
			myCi <- confint(myGlm)
			wdcExtSel$ci.minus[i] <- myCi[2,1]
			wdcExtSel$ci.plus[i] <- myCi[2,2]
		}
	}
}
time.plot(c(-2,2), "Log-odds of extinction", main="Time Series of Extinction Selectivity as Estimated by Body Size")
abline(h=0, lty=2)
points(timescale$age_mid, wdcExtSel$coef, pch=16, cex=1.25, col="black")
segments(timescale$age_mid,wdcExtSel$ci.minus,timescale$age_mid,wdcExtSel$ci.plus, col="#ff5640")
legend(250, 1.84, legend=c("Water, Dedicated organ, Closed system"), fill=c("#ff5640"), title="Repiratory System Type", cex=0.7)

wdoExtSel <- data.frame(matrix(NA, nrow=nBins, ncol=3, dimnames=list(timescale$interval_name, c('coef','ci.minus','ci.plus'))))
for(i in 1:nBins) {
	temp1 <- WaDeOp[WaDeOp$fad_age > timescale$age_top[i] & WaDeOp$lad_age < timescale$age_bottom[i], ]
	if(nrow(temp1) > 0) {
		temp1$extinct <- 0
		temp1$extinct[temp1$lad_age < timescale$age_bottom[i] & temp1$lad_age >= timescale$age_top[i]] <- 1	
		if(sum(temp1$extinct) >= 3 & nrow(temp1) >= 6) {
			myGlm1 <- glm(extinct ~ log10_volume, data=temp1, family=binomial)
			wdoExtSel$coef[i] <- myGlm1$coefficients[2]
			myCi1 <- confint(myGlm1)
			wdoExtSel$ci.minus[i] <- myCi1[2,1]
			wdoExtSel$ci.plus[i] <- myCi1[2,2]
		}
	}
}
time.plot(c(-2,2), "Log-odds of extinction", main="Time Series of Extinction Selectivity as Estimated by Body Size")
abline(h=0, lty=2)
points(timescale$age_mid, wdoExtSel$coef, pch=16, cex=1.25, col="black")
segments(timescale$age_mid,wdoExtSel$ci.minus,timescale$age_mid,wdoExtSel$ci.plus, col="#ffd900")
legend(520, 1.84, legend=c("Water, Dedicated organ, Open system"), fill=c("#ffd900"), title="Repiratory System Type", cex=0.7)

wmoExtSel <- data.frame(matrix(NA, nrow=nBins, ncol=3, dimnames=list(timescale$interval_name, c('coef','ci.minus','ci.plus'))))
for(i in 1:nBins) {
	temp2 <- WaMuOp[WaMuOp$fad_age > timescale$age_top[i] & WaMuOp$lad_age < timescale$age_bottom[i], ]
	if(nrow(temp2) > 0) {
		temp2$extinct <- 0
		temp2$extinct[temp2$lad_age < timescale$age_bottom[i] & temp2$lad_age >= timescale$age_top[i]] <- 1	
		if(sum(temp2$extinct) >= 3 & nrow(temp2) >= 6) {
			myGlm2 <- glm(extinct ~ log10_volume, data=temp2, family=binomial)
			wmoExtSel$coef[i] <- myGlm2$coefficients[2]
			myCi2 <- confint(myGlm2)
			wmoExtSel$ci.minus[i] <- myCi2[2,1]
			wmoExtSel$ci.plus[i] <- myCi2[2,2]
		}
	}
}
time.plot(c(-2,2), "Log-odds of extinction", main="Time Series of Extinction Selectivity as Estimated by Body Size")
abline(h=0, lty=2)
points(timescale$age_mid, wmoExtSel$coef, pch=16, cex=1.25, col="black")
segments(timescale$age_mid,wmoExtSel$ci.minus,timescale$age_mid,wmoExtSel$ci.plus, col="#00ffd7")
legend(520, 1.84, legend=c("Water, Multi-organ, Open system"), fill=c("#00ffd7"), title="Repiratory System Type", cex=0.7)

adcExtSel <- data.frame(matrix(NA, nrow=nBins, ncol=3, dimnames=list(timescale$interval_name, c('coef','ci.minus','ci.plus'))))
for(i in 1:nBins) {
	temp3 <- AiDeCl[AiDeCl$fad_age > timescale$age_top[i] & AiDeCl$lad_age < timescale$age_bottom[i], ]
	if(nrow(temp3) > 0) {
		temp3$extinct <- 0
		temp3$extinct[temp3$lad_age < timescale$age_bottom[i] & temp3$lad_age >= timescale$age_top[i]] <- 1	
		if(sum(temp3$extinct) >= 3 & nrow(temp3) >= 6) {
			myGlm3 <- glm(extinct ~ log10_volume, data=temp3, family=binomial)
			adcExtSel$coef[i] <- myGlm3$coefficients[2]
			myCi3 <- confint(myGlm3)
			adcExtSel$ci.minus[i] <- myCi3[2,1]
			adcExtSel$ci.plus[i] <- myCi3[2,2]
		}
	}
}
time.plot(c(-2,2), "Log-odds of extinction", main="Time Series of Extinction Selectivity as Estimated by Body Size")
abline(h=0, lty=2)
points(timescale$age_mid, adcExtSel$coef, pch=16, cex=1.25, col="black")
segments(timescale$age_mid,adcExtSel$ci.minus,timescale$age_mid,adcExtSel$ci.plus, col="#ee92ed")
legend(520, 1.84, legend=c("Air, Dedicated organ, Closed system"), fill=c("#ee92ed"), title="Repiratory System Type", cex=0.7)

#	Extinction Rate

time.plot.mult(nrow=2, ncol=2, las=1, top.mar=2.5)
plot(1:10, type="n", ylab="Extinction Rate", main="Water, Dedicated organ, Closed System", xlim=c(541,0), xlab="", ylim=c(0,1), xaxt="n")
lines(timescale$age_mid, myPropWDC, col="#ff5640", lwd=2) #adds lines of extinction rate for each genera
plot(1:10, type="n", ylab="Extinction Rate", main="Water, Dedicated organ, Open System", xlim=c(541,0), xlab="", ylim=c(0,1), xaxt="n")
lines(timescale$age_mid, myPropWDO, col="#ffd900", lwd=2)
plot(1:10, type="n", ylab="Extinction Rate", main="Water, Multi-organ, Open System", xlim=c(541,0), xlab="", ylim=c(0,1), xaxt="n")
lines(timescale$age_mid, myPropWMO, col="#00ffd7", lwd=2)
plot(1:10, type="n", ylab="Extinction Rate", main="Air, Dedicated organ, Closed System", xlim=c(541,0), xlab="", ylim=c(0,1), xaxt="n")
lines(timescale$age_mid, myPropADC, col="#ee92ed", lwd=2)

#