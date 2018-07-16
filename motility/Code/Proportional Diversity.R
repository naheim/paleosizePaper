source("https://github.com/naheim/paleosizePaper/raw/master/sharedCode/functions.r")
sizeData <- read.delim(file="https://github.com/naheim/paleosizePaper/raw/master/rawDataFiles/bodySizes.txt")
timescale <- read.delim(file="https://github.com/naheim/paleosizePaper/raw/master/rawDataFiles/timescale.txt")
nBins <- nrow(timescale)
sizeData$motile <- 0 #nonmotile
sizeData$motile[(sizeData[,"motility"]==1 | sizeData[,"motility"]==2) & !is.na(sizeData$motility)] <- 1 #motile
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
classCols <- c("red","blue")
time.plot(c(0,1), "Proportion of genera")
polygon(xPoly, yPoly1, col=classCols[1])
polygon(xPoly, yPoly2, col=classCols[2])
legend("topright", legend=rev(c("Nonmotile","Motile")), fill=rev(classCols), bg="white", title="Motility Levels")
title(main="Proportion of Motile vs Nonmotile Genera")


