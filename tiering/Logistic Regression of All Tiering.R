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
