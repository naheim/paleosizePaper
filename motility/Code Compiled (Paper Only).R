# ALL CODE for Biodiversity "Motility" Group (Shannon & Noah)

# 1: Boxplots (both all 6 and simplified)

# Subsets data without Motility Level = 0
sizeDataMod <- sizeData[sizeData[,"motility"]!=0 & !is.na(sizeData$motility),]

sizeDataMod$motile <- 0 #non-motile
sizeDataMod$motile[(sizeDataMod[,"motility"]==1 | sizeDataMod[,"motility"]==2) & !is.na(sizeDataMod$motility)] <- 1 #motile
sizeDataMod$motile <- factor(sizeDataMod$motile, levels=c(1,0))

boxplot(log10(sizeDataMod$max_vol)~sizeDataMod$motile,data=sizeDataMod,main = "Body Size vs Motility Boxplot", xlab="Motility Level", ylab="", col=c("blue","red"), names=c("Motile","Nonmotile"),notch=TRUE)
title(ylab=expression(paste("Biovolume log"[10],"mm"^3)), line=2.2)


boxplot(log10(sizeDataMod$max_vol)~sizeDataMod$motility,data=sizeDataMod,main = "Body Size vs Motility Level Boxplot", xlab="Motility Level", ylab="", col=c("darkorange", "dodgerblue","yellow", "forestgreen", "red", "purple"),notch=TRUE)
title(ylab=expression(paste("Biovolume log"[10],"mm"^3)), line=2.2)

# The Notch - displays the a confidence interval around the median which is normally based on the median +/- 1.57 x IQR/sqrt of n.  According to Graphical Methods for Data Analysis (Chambers, 1983) although not a formal test the, if two boxes' notches do not

-----------------------------------------------------------------------------------

# 2: Color-Coded Body Size vs Geologic Time
 time.plot.mult(nrow=2, ncol=3,las=1,top.mar=2.5)
 par(las=1)
plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
 sizeData$log10max_vol <- log10(sizeData$max_vol)
 abline(v = c(65, 200, 251.2, 443.8), col="black",lty=6)
 
# FOR MOTILITY LEVEL 1
segments(motility1$fad_age,motility1$log10max_vol,motility1$lad_age,motility1$log10max_vol, col="darkorange")
 title(main="Motility Level 1")
 meanVector <- vector(mode='numeric', length=nrow(timescale))
 for(i in 1:nrow(timescale)) { 
  	meanVector[i] <- mean(motility1$log10max_vol[motility1$fad_age > timescale$age_top[i] & motility1$lad_age < timescale$age_bottom[i]]) }
 lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)
 
 
# FOR MOTILITY LEVEL 2
plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
segments(motility2$fad_age,motility2$log10max_vol,motility2$lad_age,motility2$log10max_vol, col="dodgerblue")
 meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
  	meanVector[i] <- mean(motility2$log10max_vol[motility2$fad_age > timescale$age_top[i] & motility2$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)
 title(main="Motility Level 2")

# FOR MOTILITY LEVEL 3
plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
segments(motility3$fad_age,motility3$log10max_vol,motility3$lad_age,motility3$log10max_vol, col="gold")
 title(main="Motility Level 3")
 abline(v = c(65, 200, 251.2, 443.8), col="black",lty=6)
 meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
 	meanVector[i] <- mean(motility3$log10max_vol[motility3$fad_age > timescale$age_top[i] & motility3$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)

# FOR MOTILITY LEVEL 4
plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
segments(motility4$fad_age,motility4$log10max_vol,motility4$lad_age,motility4$log10max_vol, col="forestgreen")
abline(v = c(65, 200, 251.2, 443.8), col="black",lty=6)
title(main="Motility Level 4")
 meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
 	meanVector[i] <- mean(motility4$log10max_vol[motility4$fad_age > timescale$age_top[i] & motility4$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)

# FOR MOTILITY LEVEL 5
plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
segments(motility5$fad_age,motility5$log10max_vol,motility5$lad_age,motility5$log10max_vol, col="red")
abline(v = c(65, 200, 251.2, 443.8), col="black",lty=6)
title(main="Motility Level 5")
meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
 	meanVector[i] <- mean(motility5$log10max_vol[motility5$fad_age > timescale$age_top[i] & motility5$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)

# FOR MOTILITY LEVEL 6
plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
segments(motility6$fad_age,motility6$log10max_vol,motility6$lad_age,motility6$log10max_vol, col="purple")
abline(v = c(65, 200, 251.2, 443.8), col="black",lty=6)
title(main="Motility Level 6")
meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
 	meanVector[i] <- mean(motility6$log10max_vol[motility6$fad_age > timescale$age_top[i] & motility6$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)

# FOR NONMOTILE & MOTILE ONLY
motile<-rbind(motility1, motility2)
nonmotile<-rbind(motility3, motility4, motility5, motility6)

time.plot.mult(nrow=1, ncol=2,las=1,top.mar=2.5)
par(las=1)

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
sizeData$log10max_vol <- log10(sizeData$max_vol)
abline(v = c(65, 200, 251.2, 443.8), col="black",lty=5)
segments(motility1$fad_age,motility1$log10max_vol,motility1$lad_age,motility1$log10max_vol, col="blue")
title(main="Motile")
segments(motility2$fad_age,motility2$log10max_vol,motility2$lad_age,motility2$log10max_vol, col="blue")

meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
  	meanVector[i] <- mean(motile$log10max_vol[motile$fad_age > timescale$age_top[i] & motile$lad_age < timescale$age_bottom[i]]) }
 lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
abline(v = c(65, 200, 251.2, 443.8), col="black",lty=5)
segments(motility3$fad_age,motility3$log10max_vol,motility3$lad_age,motility3$log10max_vol, col="red")
segments(motility4$fad_age,motility4$log10max_vol,motility4$lad_age,motility4$log10max_vol, col="red")
segments(motility5$fad_age,motility5$log10max_vol,motility5$lad_age,motility5$log10max_vol, col="red")
segments(motility6$fad_age,motility6$log10max_vol,motility6$lad_age,motility6$log10max_vol, col="red")
title(main="Nonmotile")
 
 # DRAWING MEAN BODY SIZE
 meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) { 
  	meanVector[i] <- mean(nonmotile$log10max_vol[nonmotile$fad_age > timescale$age_top[i] & nonmotile$lad_age < timescale$age_bottom[i]]) }
 lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)
 
-----------------------------------------------------------------------------------

#3: Akaike Weights
# within each motility level, evalutes which evolutionary model best supports the trend in mean body size

library(paleoTS)
# reads in sizeData and timescale datasets
sizeData <- read.delim(file="https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/bodySizes.txt")
timescale <- read.delim(file="https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt")

# variable for time intervals
n.bins <- nrow(timescale)

#creating empty vectors
my.mean <- vector(mode="numeric", length=n.bins)
my.var <- vector(mode="numeric", length=n.bins)
my.n <- vector(mode="numeric", length=n.bins)
my.time <- timescale$age_bottom 

names(my.var) <- timescale$interval_name
names(my.n) <- timescale$interval_name
names(my.time) <- timescale$interval_name

motility1 <- sizeData[sizeData[,"motility"]==1 & !is.na(sizeData$motility),]

# for each interval, calculate mean body size for genera with motility level 1
for(i in 1:n.bins) {
temp.data <- log10(motility1$max_vol[motility1$fad_age > timescale$age_top[i] & motility1$lad_age < timescale$age_bottom[i]])
my.mean[i] <- mean(temp.data)
my.var[i] <- var(temp.data)
my.n[i] <- length(temp.data)
}

# outputs statistics for 3 models
my.ts <- as.paleoTS(mm=my.mean[!is.na(my.mean)], vv=my.var[!is.na(my.mean)], nn=my.n[!is.na(my.mean)], tt=my.time[!is.na(my.mean)], oldest="last") # check my.ts for NA values and change mean --> var accordingly
fit3models(my.ts, method="Joint", pool=FALSE)

# REPEAT FOR EACH MOTILITY LEVEL

# for motile
motile <- sizeData[(sizeData[,"motility"]==1 | sizeData[,"motility"]==2) & !is.na(sizeData$motility),]

for(i in 1:n.bins) {
temp.data <- log10(motile$max_vol[motile$fad_age > timescale$age_top[i] & motile$lad_age < timescale$age_bottom[i]])
my.mean[i] <- mean(temp.data)
my.var[i] <- var(temp.data)
my.n[i] <- length(temp.data)
}

# for nonmotile
nonmotile <- sizeData[(sizeData[,"motility"]!=1 | sizeData[,"motility"]!=2) & !is.na(sizeData$motility),]

for(i in 1:n.bins) {
temp.data <- log10(nonmotile$max_vol[nonmotile$fad_age > timescale$age_top[i] & nonmotile$lad_age < timescale$age_bottom[i]])
my.mean[i] <- mean(temp.data)
my.var[i] <- var(temp.data)


 