# Body Size vs Geologic Time with Color-coded Motility Level

# reads in sizeData and timescale datasets
sizeData <- read.delim(file="https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/bodySizes.txt")
timescale <- read.delim(file="https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt")

# creates empty plot
plot(NA, xlab="Geologic Time (Ma)", ylab=expression(paste("Body Size log"[10],"mm"^3)), xlim=c(550,0), ylim=c(-2,12))  
motility1 <- sizeData[sizeData[,"motility"]==1 & !is.na(sizeData$motility),]
motility2 <- sizeData[sizeData[,"motility"]==2 & !is.na(sizeData$motility),]  
motility3 <- sizeData[sizeData[,"motility"]==3 & !is.na(sizeData$motility),]  
motility4 <- sizeData[sizeData[,"motility"]==4 & !is.na(sizeData$motility),]  
motility5 <- sizeData[sizeData[,"motility"]==5& !is.na(sizeData$motility),]  
motility6 <- sizeData[sizeData[,"motility"]==6& !is.na(sizeData$motility),]  
 
# draws colored segments for each motility level 
segments(motility1$fad_age,motility1$max_vol,motility1$lad_age,motility1$max_vol, col="purple")
segments(motility2$fad_age,motility2$max_vol,motility2$lad_age,motility2$max_vol, col="blue")
segments(motility3$fad_age,motility3$max_vol,motility3$lad_age,motility3$max_vol, col="yellow")
segments(motility4$fad_age,motility4$max_vol,motility4$lad_age,motility4$max_vol, col="green")
segments(motility5$fad_age,motility5$max_vol,motility5$lad_age,motility5$max_vol, col="red")
segments(motility6$fad_age,motility6$max_vol,motility6$lad_age,motility6$max_vol, col="brown")

# creates empty vectors
meanVector <- vector(mode='numeric', length=nrow(timescale))
my05 <- vector(mode='numeric', length=nrow(timescale))
my95 <- vector(mode='numeric', length=nrow(timescale))

# loops through dataset and finds mean/5th & 9th quartile
for(i in 1:nrow(timescale)) { 
	meanVector[i] <- mean(sizeData$max_vol[sizeData$fad_age > timescale$age_top[i] & sizeData$lad_age < timescale$age_bottom[i]]) 
	my05[i] <- quantile(sizeData$max_vol[sizeData$fad_age > timescale$age_top[i] & sizeData$lad_age < timescale$age_bottom[i]],0.05)
	my95[i] <- quantile(sizeData$max_vol[sizeData$fad_age > timescale$age_top[i] & sizeData$lad_age < timescale$age_bottom[i]],0.95)	
	}
	
# draws lines
lines(x=log10(timescale$age_mid),y=meanVector, col="black", lwd=2.5)
lines(x=log10(timescale$age_mid),y=my05, col="black")
lines(x=log10(timescale$age_mid),y=my95, col="black")

#Adds labels
#layout; divide screen into sections