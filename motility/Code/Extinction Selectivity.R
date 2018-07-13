sizeData <- read.delim(file="https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/bodySizes.txt")
timescale <- read.delim(file="https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt")

sizeData$motile <- 0 #non-motile
sizeData$motile[(sizeData[,"motility"]==1 | sizeData[,"motility"]==2) & !is.na(sizeData$motility)] <- 1 #motile

#vector for selectivity points
coeffVector <- array(data=NA, dim=nrow(timescale))
ciMinus <- array(data=NA, dim=nrow(timescale))
ciPlus <- array(data=NA, dim=nrow(timescale))

# loop goes through each row of timescale dataset
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

plot(timescale$age_mid,coeffVector)
plot(timescale$age_mid,coeffVector, xlab = "Geologic Time (Ma)", ylab="Selectivity Coefficient", xlim=c(550,0), ylim=c(-3,3))
plot(timescale$age_mid,coeffVector, main = "Extinction Selectivity Coefficient of Motility Over Time", xlab = "Geologic Time (Ma)", ylab="Selectivity Coefficient", xlim=c(550,0), ylim=c(-3,3))

segments(timescale$age_mid,ciMinus,timescale$age_mid,ciPlus,col="black")

# line through selectivity coefficients
#construct error bars


#vector for extinction rate
rateVector <- array(data=NA, dim=nrow(timescale))

# loop goes through each row of timescale dataset, ratio of extinct-motile/motile
for (i in 1:nrow(timescale)) {
	intervalTime <- timescale[i,]
 	interval <- subset(sizeData, fad_age > intervalTime$age_top & lad_age < intervalTime$age_bottom)
 	interval$extinct <- 0
 	interval$extinct[interval$lad_age < intervalTime$age_bottom & interval$lad_age >= intervalTime$age_top] <- 1
	motileRows <- interval[interval$motile==1,]
	motileRowsExt <- motileRows[motileRows$extinct==1,]
	rateVector[i] <- nrow(motileRowsExt)/nrow(motileRows) }

# nonmotileRows <- sizeData[sizeData$motile==0]
for (i in 1:nrow(timescale)) {
	intervalTime <- timescale[i,]
 	interval <- subset(sizeData, fad_age > intervalTime$age_top & lad_age < intervalTime$age_bottom)
 	interval$extinct <- 0
 	interval$extinct[interval$lad_age < intervalTime$age_bottom & interval$lad_age >= intervalTime$age_top] <- 1
	nonmotileRows <- interval[interval$motile==0,]
	nonmotileRowsExt <- nonmotileRows[nonmotileRows$extinct==0,]
	rateVector[i] <- nrow(nonmotileRowsExt)/nrow(nonmotileRows) }

plot(timescale$age_mid,rateVector)
plot(timescale$age_mid,rateVector, main = "Extinction Rate of Nonmotile Marine Genera", xlab = "Geologic Time (Ma)", ylab="Extinction Rate", xlim=c(550,0), ylim=c(0,1))
lines(x=timescale$age_mid,y=rateVector, col="black", lwd=1.0)
