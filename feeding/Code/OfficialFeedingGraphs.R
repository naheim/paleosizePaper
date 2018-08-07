library(paleoTS)
setwd("/Users/ashlijain/Documents/git/paleosizePaper/rawDataFiles")
source("https://github.com/naheim/paleosizePaper/raw/master/sharedCode/functions.r")

bodySize <- read.delim(file="bodySizes.txt")
timescale <- read.delim(file="timescale.txt")

bodySize <- subset(bodySize, !is.na(feeding) & feeding != 0)

suspensionFeeding <- subset(bodySize, feeding == 1)
depositFeeding <- subset(bodySize, feeding == 2)
miningFeeding <- subset(bodySize, feeding == 3)
grazingFeeding <- subset(bodySize, feeding == 4)
predatoryFeeding <- subset(bodySize, feeding == 5)
otherFeeding <- subset(bodySize, feeding == 6)

myCol <- c("#ff5640","#ffd900","#00ffd7","#ee92ed","#ff00ff","#0000ff")

#*************************************************BOXPLOT: Feeding Type vs. Geologic Time***************************************************
par(col="black")

boxplot(log10(max_vol)~feeding, bodySize, xlab="Feeding Type", ylab=expression(paste("Biovolume (log "[10]," mm"^3,")")), ylim=c(-3, 12), ylab="", main="Biovolume vs. Feeding Type", col=myCol, names=c("Suspension", "Dep.", "Mining", "Grazing", "Predatory", "Other"), notch=TRUE, mar = c(4, 3.5, 4, 3.5))
mtext(side=2, line=1.9, expression(paste("Biovolume (log "[10]," mm"^3,")")), col="black", font=4, cex=1.3)
#***************************graph all body sizes against geologic time (each feeding type is a different color)********************

#*************************************Trends of Mean Size for Feeding Type (using paleoTS analysis)********************************
n.bins <- nrow(timescale)

my.mean <- matrix(NA, nrow=n.bins, ncol=6)
my.var <- matrix(NA, nrow=n.bins, ncol=6)
my.n <- matrix(NA, nrow=n.bins, ncol=6)
my.time <- timescale$age_bottom

#my.mean
names(my.mean) <- timescale$interval_name
#my.mean
names(my.var) <- timescale$interval_name
names(my.n) <- timescale$interval_name
names(my.time) <- timescale$interval_name

for (i in 1:n.bins) {
  for (j in 1:6) {
    temp.data <- log10(bodySize$max_vol[bodySize$fad_age > timescale$age_top[i] & bodySize$lad_age < timescale$age_bottom[i] & bodySize$feeding == j])
    my.mean[i,j] <- mean(temp.data)
    my.var[i,j] <- var(temp.data)
    my.n[i,j] <- length(temp.data)
  }
}
par(col="black")

#time.plot(c(0, 8), expression(paste("Biovolume (log  "[10]," mm"^3,")")), mar = c(4, 3.5, 4, 3.5)+0.5, mgp = c(2.5, 0.75, 0))

time.plot(c(0,6), expression(paste("Biovolume (log  "[10]," mm"^3,")")), "Mean Size per Feeding Type", mar = c(4, 3.5, 4, 3.5))
#plot(timescale$age_bottom, my.mean[,3], type="n", pch=16, xlab="Geologic Time (Ma)", xlim=c(541, 0), ylab="Mean Size (log10mm^3)", ylim=c(1.2,6.5), main="Mean Size per Feeding Type")
#my.col=c("blue1", "chartreuse2", "orange3", "darkorchid1", "deeppink1", "lightskyblue")

#loop per column
for(i in 1:6) {
  my.ts <- as.paleoTS(mm=my.mean[!is.na(my.var[,i]), i], vv=my.var[!is.na(my.var[,i]), i], nn=my.n[!is.na(my.var[,i]), i], tt=my.time[!is.na(my.var[,i])], oldest="last")
  fit3models(my.ts, method="Joint", pool=FALSE)
  #par(col=my.mean$color[k]); par(col="deepskyblue3")
  lines(timescale$age_mid, my.mean[, i], col=myCol[i], lwd=3)
}

par(col="black")
abline(v = c(66, 201.3, 252.17, 358.9, 443.8), col="azure4",lty=5)
legend(240, 1.65, legend=c("Feeding Type 1: Suspension", "Feeding Type 2: Deposit", "Feeding Type 3: Mining", "Feeding Type 4: Grazing", "Feeding Type 5: Predatory", "Feeding Type 6: Other"), col = myCol, lty = 1, title="Feeding Color Legend", bg = "white", box.col=NA, title.adj = 0.26, cex=0.47)
#legend(179,2.035, legend=c("Feeding Type 1: Suspension", "Feeding Type 2: Deposit", "Feeding Type 3: Mining", "Feeding Type 4: Grazing", "Feeding Type 5: Predatory", "Feeding Type 6: Other"), col = my.col, lty = 1, title="Feeding Color Legend", title.adj = 0.26, cex=.723594624,box.lwd = 0,box.col = "white",bg = "white")
#**************************************Mean Size for Feeding Type WITH 95% Confidence Intervals***********************************
n.bins <- nrow(timescale)

my.mean <- vector(mode="numeric", length=n.bins)
my.var <- vector(mode="numeric", length=n.bins)
my.n <- vector(mode="numeric", length=n.bins)
my.time <- timescale$age_bottom

names(my.mean) <- timescale$interval_name
names(my.var) <- timescale$interval_name
names(my.n) <- timescale$interval_name
names(my.time) <- timescale$interval_name

for (i in 1:n.bins) {
  temp.data <- log10(bodySize$max_vol[bodySize$fad_age > timescale$age_top[i] & bodySize$lad_age < timescale$age_bottom[i] & bodySize$feeding == 5])
  
  my.mean[i] <- mean(temp.data)
  my.var[i] <- var(temp.data)
  my.n[i] <- length(temp.data)
}

time.plot(c(0,7.5), expression(paste("Biovolume (log  "[10]," mm"^3,")")), mar = c(4, 3.5, 4, 3.5)+0.5, mgp = c(2.5, 0.75, 0), cex.lab=1.25, cex.axis=1.25)
#cex.lab
#cex.axis
abline(v = c(66, 201.3, 252.17, 358.9, 443.8), col="azure4",lty=5)

my.ts <- as.paleoTS(mm=my.mean[!is.na(my.var)], vv=my.var[!is.na(my.var)], nn=my.n[!is.na(my.var)], tt=my.time[!is.na(my.var)], oldest="last")

fit3models(my.ts, method="Joint", pool=FALSE)

par(col="black")

#plot(timescale$age_bottom, my.mean, type="n", pch=16, xlab="Geologic Time (Ma)", xlim=c(541, 0), ylim=c(0,7.5), ylab="Other Feeders Mean Size", main="Other Feeders")

ci <- vector(mode="numeric", length=n.bins)
for (i in 1:n.bins) {
  ci[i] <- 1.96 * sqrt(my.var[i]) / sqrt(my.n[i])
}
#shading vector {"coral","palegoldenrod","mediumturquoise","mistyrose",,"cornflowerblue"}
polygon(c(timescale$age_mid[!is.na(my.var)], rev(timescale$age_mid[!is.na(my.var)])), c(my.mean[!is.na(my.var)] - ci[!is.na(my.var)], rev(my.mean[!is.na(my.var)] + ci[!is.na(my.var)])), col="lightpink")
lines(timescale$age_mid, my.mean, col="#ff00ff", lwd = 3.0)
mtext(side=3, line=0.20, "Mean Size for Predatory Feeders", col="black", font=4, cex=1.3)

#******************************************Proportional Diversity of Feeding Type***************************************************
#create 1 value vector of proportion of feeding time (corresponds to color) - 5 different ones needed
myProp <- matrix(NA, nrow=nrow(timescale), ncol=6)
bodySize$feeding <- factor(bodySize$feeding)
for(i in 1:nrow(timescale)) {
  temp <- subset(bodySize, fad_age > timescale$age_top[i] & lad_age < timescale$age_bottom[i])
  counts <- table(temp$feeding)
  myProp[i,] = counts/sum(counts)
}

plot(1:10, type="n", xlim=c(541,0), ylim=c(0,1))
time.plot(c(0,1), "Proportion of feeding", main="Feeding Proportions")

#assigning colors to proportions
propOrange <- myProp[,1]
propBlue <- myProp[,2]
propPink <- myProp[,3]
propGreen <- myProp[,4]
propCyan <- myProp[,5]
propPurple <- myProp[,6]

#creating all the polygons - each builds upon previous ones
myX <- c(timescale$age_mid, rev(timescale$age_mid))
myOrange <- c(rep(0, nrow(timescale)), rev(propOrange))
polygon(myX, myOrange, col=myCol[1])
myBlue <- c(propOrange, rev(propOrange + propBlue))
polygon(myX, myBlue, col=myCol[2])
myPink <- c((propOrange + propBlue), rev(propOrange + propBlue + propPink))
polygon(myX, myPink, col=myCol[3])
myGreen <- c((propOrange + propBlue + propPink), rev(propOrange + propBlue + propPink + propGreen))
polygon(myX, myGreen, col=myCol[4])
myCyan <- c((propOrange + propBlue + propPink + propGreen), rev(propOrange + propBlue + propPink + propGreen + propCyan))
polygon(myX, myCyan, col=myCol[5])
myPurple <- c((propOrange + propBlue + propPink + propGreen + propCyan), rev(propOrange + propBlue + propPink + propGreen + propCyan + propPurple))
polygon(myX, myPurple, col=myCol[6])

my.col=c("blue1", "chartreuse2", "orange3", "darkorchid1", "deeppink1", "lightskyblue")
my.col=myCol

legend(535, .29, legend=c("Feeding Type 1: Suspension", "Feeding Type 2: Deposit", "Feeding Type 3: Mining", "Feeding Type 4: Grazing", "Feeding Type 5: Predatory", "Feeding Type 6: Other"), col = my.col, lty = 1, title="Feeding Color Legend", bg = "white", box.col=NA, title.adj = 0.26, cex=0.64)


#********************Breakdown of Suspension Feeders into Motile & Nonmotile Biovolume - Stratigraphic****************************
nBins <- nrow(timescale) # a variable of convenience for when the number of stages is used

bodySize$log10max_vol <- log10(bodySize$max_vol)
suspension <- subset(bodySize, !is.na(feeding) & feeding == 1)
motile <- subset(suspension, !is.na(motility) & motility != 0)
nmotile <- subset(suspension, is.na(motility) | motility == 0)

time.plot.mult(nrow=1, ncol=1,las=1, top.mar=2.5)
par(las=1)

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")), font = 3)
abline(v = c(65, 200, 358, 251.2, 443.8), col="black",lty=5)

segments(suspension$fad_age,suspension$log10max_vol,suspension$lad_age,suspension$log10max_vol, col="#ff5640")
meanVector <- vector(mode='numeric', length=nrow(timescale))
#Motile
for(i in 1:nrow(timescale)) { 
  meanVector[i] <- mean(motile$log10max_vol[motile$fad_age > timescale$age_top[i] & motile$lad_age < timescale$age_bottom[i]])
}
lines(x=timescale$age_mid,y=meanVector, col="black",lwd=2.5)
mtext(side=3, line=0.25, "Breakdown of Suspension Feeders into Motile and Nonmotile", col="black", font=4, cex=1.3)
#Non Motile
for(i in 1:nrow(timescale)) { 
  meanVector[i] <- mean(nmotile$log10max_vol[nmotile$fad_age > timescale$age_top[i] & nmotile$lad_age < timescale$age_bottom[i]])
}
lines(x=timescale$age_mid,y=meanVector,col= "blue", lwd=2.5)

legend("topright", legend=c("Motile", "Nonmotile"), fill=c("black", "blue"), bg="white", title="Motility")

#***************************************Stratigraphic Ranges per Feeding Type*****************************************************
bodySize$log10max_vol <- log10(bodySize$max_vol)
feeding1 <- bodySize[bodySize[,"feeding"]==1 & !is.na(bodySize$feeding),]
feeding2 <- bodySize[bodySize[,"feeding"]==2 & !is.na(bodySize$feeding),]  
feeding3 <- bodySize[bodySize[,"feeding"]==3 & !is.na(bodySize$feeding),]  
feeding4 <- bodySize[bodySize[,"feeding"]==4 & !is.na(bodySize$feeding),]  
feeding5 <- bodySize[bodySize[,"feeding"]==5& !is.na(bodySize$feeding),]  
feeding6 <- bodySize[bodySize[,"feeding"]==6& !is.na(bodySize$feeding),]  
myCol <- c("#ff5640","#ffd900","#00ffd7","#ee92ed","#ff00ff","#0000ff")

#FIGURE: Body Size Over Time - Color Coded
time.plot.mult(nrow=2, ncol=3,las=1, top.mar=2.5)
par(las=1)

# For each feeding level, draw segment (stratigraphic ranges) & mean line
plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
abline(v = c(65, 200, 358, 251.2, 443.8), col="black",lty=5)
segments(feeding1$fad_age,feeding1$log10max_vol,feeding1$lad_age,feeding1$log10max_vol, col="#ff5640")
title(main="Suspension Feeder")
meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) {
  meanVector[i] <- mean(feeding1$log10max_vol[feeding1$fad_age > timescale$age_top[i] & feeding1$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
segments(feeding2$fad_age,feeding2$log10max_vol,feeding2$lad_age,feeding2$log10max_vol, col="#ffd900")
abline(v = c(65, 200, 358, 251.2, 443.8), col="black",lty=5)
meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) {
  meanVector[i] <- mean(feeding2$log10max_vol[feeding2$fad_age > timescale$age_top[i] & feeding2$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)
title(main="Surface Deposit")


plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
segments(feeding3$fad_age,feeding3$log10max_vol,feeding3$lad_age,feeding3$log10max_vol, col="#00ffd7")
title(main="Mining")
abline(v = c(65, 200, 358, 251.2, 443.8), col="black",lty=5)
meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) {
  meanVector[i] <- mean(feeding3$log10max_vol[feeding3$fad_age > timescale$age_top[i] & feeding3$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
segments(feeding4$fad_age,feeding4$log10max_vol,feeding4$lad_age,feeding4$log10max_vol, col="#ee92ed")
abline(v = c(65, 200, 358,  251.2, 443.8), col="black",lty=5)
title(main="Grazing")
meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) {
  meanVector[i] <- mean(feeding4$log10max_vol[feeding4$fad_age > timescale$age_top[i] & feeding4$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
segments(feeding5$fad_age,feeding5$log10max_vol,feeding5$lad_age,feeding5$log10max_vol, col="#ff00ff")
abline(v = c(65, 200, 358, 251.2, 443.8), col="black",lty=5)
title(main="Predatory")
meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) {
  meanVector[i] <- mean(feeding5$log10max_vol[feeding5$fad_age > timescale$age_top[i] & feeding5$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)

plot(1:10,type='n',xlim=c(541,0),xaxt='n',xlab='',ylim=c(-2,12),ylab=expression(paste("Biovolume (log"[10]," mm"^3*")")))
segments(feeding6$fad_age,feeding6$log10max_vol,feeding6$lad_age,feeding6$log10max_vol, col="#0000ff")
abline(v = c(65, 200, 358, 251.2, 443.8), col="black",lty=5)
title(main="Other")
meanVector <- vector(mode='numeric', length=nrow(timescale))
for(i in 1:nrow(timescale)) {
  meanVector[i] <- mean(feeding6$log10max_vol[feeding6$fad_age > timescale$age_top[i] & feeding6$lad_age < timescale$age_bottom[i]]) }
lines(x=timescale$age_mid,y=meanVector, col="black", lwd=2.5)



