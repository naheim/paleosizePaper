# set root base for proper file direction
init.dir <- getwd()
if(is.element(Sys.info()["nodename"], c("es-naheim.local","sr12-cf96e71ca1.stanford.edu"))) {
	my.root <- "/Volumes/Blastoid/noelheim_data"
} else {
	my.root <- "/Users/noelheim"
}
Sys.setenv(TZ="America/Los_Angeles")
t0 <- Sys.time()
setwd(paste(my.root,"/Box Sync/git/paleosizePaper/final",sep=""))

source("https://github.com/naheim/paleosizePaper/raw/master/sharedCode/functions.r")

library(paleoTS)
library(MuMIn)

timescale <- read.delim(file="../rawDataFiles/timescale.txt")
nBins <- nrow(timescale)

sizeData <- read.delim(file="../rawDataFiles/bodySizes.txt")
sizeData$tiering[sizeData$tiering == 0] <- NA
sizeData$motility[sizeData$motility == 0] <- NA
sizeData$feeding[sizeData$feeding == 0] <- NA

# binary motile
sizeData$motile <- NA
sizeData$motile[!is.na(sizeData$motility) & sizeData$motility <= 2] <- 1
sizeData$motile[!is.na(sizeData$motility) & sizeData$motility > 2] <- 0

# binary pelagic
sizeData$pelagic <- NA
sizeData$pelagic[!is.na(sizeData$tiering) & sizeData$tiering == 1] <- 1
sizeData$pelagic[!is.na(sizeData$tiering) & sizeData$tiering > 1] <- 0

# binary predator
sizeData$predator <- NA
sizeData$predator[!is.na(sizeData$feeding) & sizeData$feeding == 5] <- 1
sizeData$predator[!is.na(sizeData$feeding) & sizeData$feeding != 5] <- 0

# binary circulation
sizeData$closedCirc <- NA
sizeData$closedCirc[!is.na(sizeData$circ) & sizeData$circ == 'open'] <- 0
sizeData$closedCirc[!is.na(sizeData$circ) & sizeData$circ == 'closed'] <- 1

# binary air
sizeData$air <- NA
sizeData$air[!is.na(sizeData$fluid) & sizeData$fluid == 'water'] <- 0
sizeData$air[!is.na(sizeData$fluid) & sizeData$fluid == 'air'] <- 1


sizeData$physioMode <- paste(sizeData$circ, sizeData$motile, sizeData$pelagic, sizeData$predator, sep="")
sizeData$physioMode[grepl('NA', sizeData$physioMode)] <- NA
sizeData$physioMode[!is.na(sizeData$physioMode) & !grepl('open', sizeData$physioMode) & !grepl('closed', sizeData$physioMode)] <- NA
sizeData$physioMode <- factor(sizeData$physioMode)
table(sizeData$physioMode)

sizeData$ecoMode <- paste(sizeData$motile, sizeData$pelagic, sizeData$predator, sep="")
sizeData$ecoMode[grepl('NA', sizeData$ecoMode)] <- NA
sizeData$ecoMode <- factor(sizeData$ecoMode)
table(sizeData$ecoMode)

open000 <- data.frame(matrix(NA, nrow=nBins, ncol=4, dimnames=list(timescale$interval_name, c('n','mean','ciMinus','ciPlus','max'))))
closed111 <- open000
otherPhysio <- open000
eco000 <- open000
eco111 <- open000
otherEco <- open000
openCirc <- open000
closedCirc <- open000
overallMean <- vector(mode="numeric", length=nBins)
for(i in 1:nBins) {
	temp <- subset(sizeData, !is.na(fluid) & fluid != 'air' & fad_age > timescale$age_top[i] & lad_age < timescale$age_bottom[i])
	overallMean[i] <- mean(log10(temp$max_vol))
		
	temp <- subset(sizeData, !is.na(physioMode) & physioMode == 'open000' & fad_age > timescale$age_top[i] & lad_age < timescale$age_bottom[i])
	open000$n <- nrow(temp)
	open000$mean[i] <- mean(log10(temp$max_vol))
	open000$ciMinus[i] <- mean(log10(temp$max_vol)) - (1.96*sd(log10(temp$max_vol)))
	open000$ciPlus[i] <- mean(log10(temp$max_vol)) + (1.96*sd(log10(temp$max_vol)))
	
	
	temp <- subset(sizeData, !is.na(physioMode) & physioMode == 'closed111' & fad_age > timescale$age_top[i] & lad_age < timescale$age_bottom[i])
	closed111$n <- nrow(temp)
	closed111$mean[i] <- mean(log10(temp$max_vol))
	closed111$ciMinus[i] <- mean(log10(temp$max_vol)) - (1.96*sd(log10(temp$max_vol)))
	closed111$ciPlus[i] <- mean(log10(temp$max_vol)) + (1.96*sd(log10(temp$max_vol)))
	
	
	temp <- subset(sizeData, !is.na(physioMode) & physioMode != 'closed111' & physioMode != 'open000' & fad_age > timescale$age_top[i] & lad_age < timescale$age_bottom[i])
	otherPhysio$n <- nrow(temp)
	otherPhysio$mean[i] <- mean(log10(temp$max_vol))
	otherPhysio$ciMinus[i] <- mean(log10(temp$max_vol)) - (1.96*sd(log10(temp$max_vol)))
	otherPhysio$ciPlus[i] <- mean(log10(temp$max_vol)) + (1.96*sd(log10(temp$max_vol)))
	
	
	temp <- subset(sizeData, !is.na(ecoMode) & ecoMode == '000' & fad_age > timescale$age_top[i] & lad_age < timescale$age_bottom[i])
	eco000$n <- nrow(temp)
	eco000$mean[i] <- mean(log10(temp$max_vol))
	eco000$ciMinus[i] <- mean(log10(temp$max_vol)) - (1.96*sd(log10(temp$max_vol)))
	eco000$ciPlus[i] <- mean(log10(temp$max_vol)) + (1.96*sd(log10(temp$max_vol)))
	
	temp <- subset(sizeData, !is.na(ecoMode) & ecoMode == '111' & fad_age > timescale$age_top[i] & lad_age < timescale$age_bottom[i])
	eco111$n <- nrow(temp)
	eco111$mean[i] <- mean(log10(temp$max_vol))
	eco111$ciMinus[i] <- mean(log10(temp$max_vol)) - (1.96*sd(log10(temp$max_vol)))
	eco111$ciPlus[i] <- mean(log10(temp$max_vol)) + (1.96*sd(log10(temp$max_vol)))
	
	temp <- subset(sizeData, !is.na(ecoMode) & ecoMode != '000' & ecoMode != '111' & fad_age > timescale$age_top[i] & lad_age < timescale$age_bottom[i])
	otherEco$n <- nrow(temp)
	otherEco$mean[i] <- mean(log10(temp$max_vol))
	otherEco$ciMinus[i] <- mean(log10(temp$max_vol)) - (1.96*sd(log10(temp$max_vol)))
	otherEco$ciPlus[i] <- mean(log10(temp$max_vol)) + (1.96*sd(log10(temp$max_vol)))
	
	
	temp <- subset(sizeData, !is.na(circ) & circ == 'open' & fad_age > timescale$age_top[i] & lad_age < timescale$age_bottom[i])
	openCirc$n <- nrow(temp)
	openCirc$mean[i] <- mean(log10(temp$max_vol))
	openCirc$ciMinus[i] <- mean(log10(temp$max_vol)) - (1.96*sd(log10(temp$max_vol)))
	openCirc$ciPlus[i] <- mean(log10(temp$max_vol)) + (1.96*sd(log10(temp$max_vol)))
	
	temp <- subset(sizeData, !is.na(circ) & circ == 'closed' & fad_age > timescale$age_top[i] & lad_age < timescale$age_bottom[i])
	closedCirc$n <- nrow(temp)
	closedCirc$mean[i] <- mean(log10(temp$max_vol))
	closedCirc$ciMinus[i] <- mean(log10(temp$max_vol)) - (1.96*sd(log10(temp$max_vol)))
	closedCirc$ciPlus[i] <- mean(log10(temp$max_vol)) + (1.96*sd(log10(temp$max_vol)))
	closedCirc$max[i] <- max(log10(temp$max_vol[!is.na(temp$fluid) & temp$fluid != 'air']))
}


quartz(height=5, width=16)
par(pch=16, mfrow=c(1,3))
plot(1:10, type="n", xlim=c(541,0), ylim=c(1,8))
lines(timescale$age_mid, eco000$mean, type="o", col='blue', lwd=2)
lines(timescale$age_mid, eco111$mean, type="o", col='red', lwd=2)
lines(timescale$age_mid, otherEco$mean, type="o", col='black', lwd=2)

plot(1:10, type="n", xlim=c(541,0), ylim=c(1,8))
lines(timescale$age_mid, open000$mean, type="o", col='blue', lwd=2)
lines(timescale$age_mid, closed111$mean, type="o", col='red', lwd=2)
lines(timescale$age_mid, otherPhysio$mean, type="o", col='black', lwd=2)

plot(1:10, type="n", xlim=c(541,0), ylim=c(1,8))
lines(timescale$age_mid, openCirc$mean, type="o", col='blue', lwd=2)
lines(timescale$age_mid, closedCirc$mean, type="o", col='red', lwd=2)
lines(timescale$age_mid, overallMean, type="o", col='black', lwd=2)




quartz(height=5, width=16)
par(pch=16, mfrow=c(1,2))
plot(1:10, type="n", xlim=c(541,0), ylim=c(-2,12))
temp <- subset(sizeData, !is.na(fluid) & fluid != 'air')
segments(temp$fad_age, log10(temp$max_vol), temp$lad_age, log10(temp$max_vol), col="gray")
lines(timescale$age_mid, overallMean, type="o", col='black', lwd=2)
lines(timescale$age_mid, closedCirc$max, type="o", col='red', lwd=2)

plot(1:10, type="n", xlim=c(541,0), ylim=c(-2,12))
#lines(timescale$age_mid, openCirc$mean, type="o", col='blue', lwd=2)
temp <- subset(sizeData, !is.na(circ) & circ == 'closed')
segments(temp$fad_age, log10(temp$max_vol), temp$lad_age, log10(temp$max_vol), col="gray")
lines(timescale$age_mid, closedCirc$mean, type="o", col='red', lwd=2)


tempData <- subset(sizeData, !is.na(closedCirc) & !is.na(motile) & !is.na(predator) & !is.na(pelagic) & !is.na(air)) 
myGlm <- glm(log10(max_vol) ~ closedCirc + motile + predator + pelagic + air, data=tempData, na.action='na.fail')
modelComb <- dredge(myGlm)

# average across all models, to use only those with, e.g., delta.aicc < 4, add: subset = delta < 4
# in output, full is averaging across all models for all coefficients, subset is the avg for each parameter where the value is > 0
avgMod <- model.avg(modelComb, fit=TRUE)

avgMod

# get the confidence intervals on coefficients
ci <- confint(avgMod, full=TRUE)

x <- avgMod$coefficients[1,-1]
y <- ci[-1,]

quartz()
par(pch=16, las=1)
plot(x, ylim=range(y), xaxt="n", xlab="", ylab="coefficient")
axis(side=1, at=1:length(x), labels=rownames(y))
abline(h=0, lty=2)
segments(1:length(x), y[,1], 1:length(x), y[,2])




















