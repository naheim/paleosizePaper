sizeData <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/bodySizes.txt')
sizeData <- subset(sizeData, !is.na(tiering) & tiering > 0)
sizeData$log10_volume <- log10(sizeData$max_vol)
dim(sizeData)
head(sizeData)
timescale <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt')
par(mar=c(5,4.5,2,2), xaxs="i")
plot(1:10,1:10, type="n", xlim=c(550,0), ylim=c(-2,12), xlab="Geological time (Ma)", ylab=expression(paste("Biovolume (log"[10]," cm"^3,")")))
segments(sizeData$fad_age, sizeData$log10_volume, sizeData$lad_age, sizeData$log10_volume)
myMean <- vector(mode="numeric", length=nrow(timescale))
my05 <- myMean
my95 <- myMean
myCols<- c("red", "orange", "green", "cyan", "magenta", "blue")
sizeData$color<-myCols[sizeData$tiering]
head(sizeData)
segments(sizeData$fad_age, sizeData$log10_volume, sizeData$lad_age, sizeData$log10_volume, col=sizeData$color)
legend("topright", 
legend = c("Pelagic","Erect","Surficial","Semi-Infaunal", "Shallow Infaunal", "Deep Infaunal"),
col = c("red", "orange", "green", "cyan", "magenta", "blue"),
pch=16) 
title(main=  "Body Size Evolution of Each Tiering Level Across the Past 542 Million Years")
