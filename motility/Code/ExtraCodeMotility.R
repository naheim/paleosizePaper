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
