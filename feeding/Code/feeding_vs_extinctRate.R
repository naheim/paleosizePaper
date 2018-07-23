## MAKE SURE TO SET YOUR WORKING DIRECTORY!!!

# source() loads a file that contains custom functions
# this particular file only contains two for making a geologic timescales
source("https://github.com/naheim/paleosizePaper/raw/master/sharedCode/functions.r")

# read in size data & timescale
sizeData <- read.delim(file="https://github.com/naheim/paleosizePaper/raw/master/rawDataFiles/bodySizes.txt")
timescale <- read.delim(file="https://github.com/naheim/paleosizePaper/raw/master/rawDataFiles/timescale.txt")
nBins <- nrow(timescale) # a variable of convenience for when the number of stages is used

# for this example plot, we're going to plot the size selectivity of the animals with feeding type = 1 to 6
feedingType <- c("Suspension", "Surface Deposit", "Mining", "Grazing", "Predatory", "Other")
feedColors <- c("blue1", "chartreuse2", "orange3", "darkorchid1", "deeppink1", "lightskyblue")

sizeData$feeding[sizeData$feeding == 0] <- NA
sizeData$feeding <- factor(sizeData$feeding)
# Calculate the extintion rate for each type of feeder
extRate <- data.frame(matrix(NA, nrow=nrow(timescale), ncol=6, dimnames=list(timescale$interval_name, paste("feeding", 1:6))))
meanSize <- extRate

for(i in 1:nBins) {
	temp <- subset(sizeData, fad_age > timescale$age_top[i] & lad_age < timescale$age_bottom[i])
	tempSize <- tapply(log10(temp$max_vol), temp$feeding, mean)
	meanSize[i,] <- as.numeric(tempSize)
	tempExt <- table(temp$feeding)
	totalDiv <- nrow(temp)
        extRate[i,] <- as.numeric(tempExt) / as.numeric(totalDiv)
}

# Now plot the extRate of the Genera over the time scale
time.plot(c(-0.2,1.2), "Extinction Rate")
# since there are only six feeding types, we can afford to plot of them with out
# a loop. We can pick and choose colors and other parameters one by one.
points(timescale$age_mid, extRate[,1], col=feedColors[1], pch='*')
lines(x=timescale$age_mid,y=extRate[,1],col=feedColors[1], lwd=1.0)

points(timescale$age_mid, extRate[,2], col=feedColors[2], pch='*')
lines(x=timescale$age_mid,y=extRate[,2],col=feedColors[2], lwd=1.0)

points(timescale$age_mid, extRate[,3], col=feedColors[3], pch='*')
lines(x=timescale$age_mid,y=extRate[,3],col=feedColors[3], lwd=1.0)

points(timescale$age_mid, extRate[,4], col=feedColors[4], pch='*')
lines(x=timescale$age_mid,y=extRate[,4],col=feedColors[4], lwd=1.0)

points(timescale$age_mid, extRate[,5], col=feedColors[5], pch='*')
lines(x=timescale$age_mid,y=extRate[,5],col=feedColors[5], lwd=1.0)

points(timescale$age_mid, extRate[,6], col=feedColors[6], pch='*')
lines(x=timescale$age_mid,y=extRate[,6],col=feedColors[6], lwd=1.0)

legend("topright", legend=(feedingType), fill=(feedColors), bg="white", title="Feeding Type")
