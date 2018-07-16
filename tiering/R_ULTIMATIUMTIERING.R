#TOTAL GRAPH SINGLE DOC:

**************************************************************************************************************************************************************************************************************************************************************************************************************************************************************
#1: # of Gentra in Each Tiering Lvl Over Time:

#The Change in the Amount of Gentra Categorized by Tiering Level Over Million-Years

#WORKS: 

sizeData <- read.delim("bodySizes.txt") 
timescale <- read.delim("timescale.txt") 
sizeData = subset(sizeData, !is.na(tiering) & tiering != 0)

n.bins = nrow(timescale)

sizeData$tiering = factor(sizeData$tiering, levels = 1:6)
my.tiering = data.frame()

for(i in 1:nrow(timescale)){temp.data = sizeData$tiering[sizeData$fad_age > timescale$age_top[i] & sizeData$lad_age < timescale$age_bottom[i]]
	temp.count = data.frame(table(temp.data))$Freq
	my.tiering = rbind(my.tiering, temp.count)
	}

par(xaxs = "i", yaxs = "i")
my.col = c("red", "orange", "green", "cyan", "magenta", "blue")
source("https://github.com/naheim/paleosizePaper/raw/master/sharedCode/functions.r")
time.plot(c(0,range(my.tiering)), "Amount of Genera", mar = c(4, 3.5, 6, 3.5)+0.1, mgp = c(2.5, 0.75, 0))
#plot(1:10,1:10, type="n", xlim=c(541,-5), ylim=range(my.tiering), pch=21, xlab="Geologic Time (Ma)", ylab="Amount of Gentra")
for(i in 1:1:6){lines(timescale$age_bottom, my.tiering[,i], col=my.col[i])}
abline(v = c(65, 200, 251.2, 443.8), col="black")
mtext(side=3, line=0.5, "The Change in the Amount of Gentra Categorized by Tiering Level Over Million-Years", col="black", font=4, cex=1.3)
legend(540, 1197, legend=c("Tiering Level 1: Pelagic", "Tiering Level 2: Erect", "Tiering Level 3: Surficial", "Tiering Level 4: Semi-infaunal", "Tiering Level 5: Shallow infaunal", "Tiering Level 6: Deep infaunal"), col = my.col, lty = 1, title="Tiering Levels:", bg = "white", box.col=NA, title.adj = 0.31)

**************************************************************************************************************************************************************************************************************************************************************************************************************************************************************

#2: Proportion of Each Tiering Lvl Over Time:

#CHANGE DIRECTORIES!

#WORKS:

sizeData <- read.delim("bodySizes.txt") 
sizeData <- subset(sizeData, !is.na(tiering) & tiering > 0)
timescale <- read.delim("timescale.txt") 
sizeData$tiering = factor(sizeData$tiering)
myProp = matrix(NA, nrow = nrow(timescale), ncol = 6)
my.col = c("red", "orange", "green", "cyan", "magenta", "blue")

for(i in 1:nrow(timescale)){temp <- subset(sizeData, sizeData$fad_age > timescale$age_top[i] & sizeData$lad_age < timescale$age_bottom[i])
counts <- table(temp$tiering)
myProp[i,] <- counts/sum(counts)
}

par(xaxs = "i", yaxs = "i")
source("https://github.com/naheim/paleosizePaper/raw/master/sharedCode/functions.r")
time.plot(c(0,1), "Proportion of Genera", mar = c(4, 3.5, 6, 3.5)+0.1, mgp = c(2.5, 0.75, 0))
#plot(1:10, type = "n", xlim = c(541,0), ylim = c(0,1), xlab="Geologic Time (Ma)", ylab="Porportion of Genera")
myX <- c(timescale$age_mid, rev(timescale$age_mid))
myRed <- c(rep(0, nrow(timescale)), rev(myProp[,1]))
polygon(myX, myRed, col="red")
myOrange <- c(myProp[,1], rev(myProp[,1]+myProp[,2]))
polygon(myX, myOrange, col="orange")
myGreen <- c(myProp[,1]+myProp[,2], rev(myProp[,1]+myProp[,2]+myProp[,3]))
polygon(myX, myGreen, col="green")
myCyan <- c(myProp[,1]+myProp[,2]+myProp[,3], rev(myProp[,1]+myProp[,2]+myProp[,3]+myProp[,4]))
polygon(myX, myCyan, col="cyan")
myMagenta <- c(myProp[,1]+myProp[,2]+myProp[,3]+myProp[,4], rev(myProp[,1]+myProp[,2]+myProp[,3]+myProp[,4]+myProp[,5]))
polygon(myX, myMagenta, col="magenta")
myBlue <- c(myProp[,1]+myProp[,2]+myProp[,3]+myProp[,4]+myProp[,5], rev(myProp[,1]+myProp[,2]+myProp[,3]+myProp[,4]+myProp[,5]+myProp[,6]))
polygon(myX, myBlue, col="blue")
mtext(side=3, line=0.5, "The Change in the Proportions of Gentra Categorized by Tiering Level Over Million-Years", col="black", font=4, cex=1.3)
my.col = c("red", "orange", "green", "cyan", "magenta", "blue")
par(xpd=TRUE)
legend(550, 1.16, legend=c("Tiering Level 1: Pelagic", "Tiering Level 2: Erect", "Tiering Level 3: Surficial", "Tiering Level 4: Semi-infaunal", "Tiering Level 5: Shallow infaunal", "Tiering Level 6: Deep infaunal"), col = my.col, lty = 1, title="Tiering Color Legend", bg = "NA", box.col=NA, title.adj = 0.26, cex=0.55, text.font = 4)

**************************************************************************************************************************************************************************************************************************************************************************************************************************************************************

#3: Mean Biovolume of Each Tiering Lvl Over Time

#CHANGE DIRECTORY!

bodySize <- read.delim("bodySizes.txt") 
timescale <- read.delim("timescale.txt") 
bodySize <- subset(sizeData, !is.na(tiering) & tiering != 0)

library(paleoTS)

n.bins <- nrow(timescale)

my.mean <- matrix(NA, nrow=n.bins, ncol=6)
my.var <- matrix(NA, nrow=n.bins, ncol=6)
my.n <- matrix(NA, nrow=n.bins, ncol=6)
my.time <- timescale$age_bottom


names(my.mean) <- timescale$interval_name
names(my.var) <- timescale$interval_name
names(my.n) <- timescale$interval_name
names(my.time) <- timescale$interval_name

for (i in 1:n.bins) {for (j in 1:6) {
temp.data <- log10(bodySize$max_vol[bodySize$fad_age > timescale$age_top[i] & bodySize$lad_age < timescale$age_bottom[i] & bodySize$tiering == j])
my.mean[i,j] <- mean(temp.data)
my.var[i,j] <- var(temp.data)
my.n[i,j] <- length(temp.data)
}
}
par(col="black")
source("https://github.com/naheim/paleosizePaper/raw/master/sharedCode/functions.r")
time.plot(c(0, 8), expression(paste("Biovolume (log  "[10]," cm"^3,")")), mar = c(3.5, 3.5, 3.5, 3.5)+0.1, mgp = c(2, 0.75, 0))
#plot(timescale$age_bottom, my.mean[,3], type="n", pch=16, xlab="Geologic Time (Ma)", xlim=c(541, 0), ylab="Mean Size", ylim=c(1.2,6.5), main="Mean expression(paste("Biovolume (log"[10]," cm"^3)"))
my.col = c("red", "orange", "green", "cyan", "magenta", "blue")
for(i in 1:6) {
  my.ts <- as.paleoTS(mm=my.mean[!is.na(my.var[,i]), i], vv=my.var[!is.na(my.var[,i]), i], nn=my.n[!is.na(my.var[,i]), i], tt=my.time[!is.na(my.var[,i])], oldest="last")
  fit3models(my.ts, method="Joint", pool=FALSE)
  #par(col=my.mean$color[k]); par(col="deepskyblue3")
  lines(timescale$age_mid, my.mean[, i], col=my.col[i])
}
mtext(side=3, line=0.5, "The Change in Mean Biovolume of Gentra Categorized by Tiering Level Over Million-Years", col="black", font=4, cex=1.3)
abline(v = c(65, 200, 251.2, 443.8), col="black")
par(col="black")
legend("topleft", legend=c("Tiering Level 1: Pelagic", "Tiering Level 2: Erect", "Tiering Level 3: Surficial", "Tiering Level 4: Semi-infaunal", "Tiering Level 5: Shallow infaunal", "Tiering Level 6: Deep infaunal"), col = my.col, lty = 1, title="Tiering Level", bg = "white", title.adj = 0.31, cex=1)

**************************************************************************************************************************************************************************************************************************************************************************************************************************************************************

#4: Body Size Box Plot for Each Tier/Biovolume vs. Tiering Level

par(col="black")
my.col = c("red", "orange", "green", "cyan", "magenta", "blue")
my.opp = c("chartreuse3", "dodgerblue2", "red", "darkorange1", "forestgreen", "orange2")
par(mar = c(5.5, 5.5, 2, 2)+0.1)
par(mgp = c(3, 1.7, 0))
boxplot(log10(max_vol)~tiering, bodySize, xlab="Tiering Level", ylab=expression(paste("Biovolume (log  "[10]," cm"^3,")")), col=my.col, names=c("Tiering Level 1:\n Pelagic", "Tiering Level 2:\n Erect", "Tiering Level 3:\n Surficial", "Tiering Level 4:\n Semi-infaunal", "Tiering Level 5:\n Shallow infaunal", "Tiering Level 6:\n Deep infaunal"), border = my.opp)
mtext(side=3, line=0.5, "Biovolume vs. Tiering Level", col="black", font=4, cex=1.3)

**************************************************************************************************************************************************************************************************************************************************************************************************************************************************************

#5: Stephanie's Reuse of Noel's Chart for Tiering Levels with Wrong Colors

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
myCols<- c("lightpink", "orange", "lightgreen", "cyan", "magenta", "blue")
sizeData$color<-myCols[sizeData$tiering]
head(sizeData)
segments(sizeData$fad_age, sizeData$log10_volume, sizeData$lad_age, sizeData$log10_volume, col=sizeData$color)















