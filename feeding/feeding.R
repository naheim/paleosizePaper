library(paleoTS)
setwd("/Users/ashlijain/Documents/git/paleosizePaper/rawDataFiles")
source("https://github.com/naheim/paleosizePaper/raw/master/sharedCode/functions.r")

bodySize <- read.delim(file="bodySizes.txt")
timescale <- read.delim(file="timescale.txt")[-1,]

bodySize <- subset(bodySize, !is.na(feeding) & feeding != 0)

suspensionFeeding <- subset(bodySize, feeding == 1)
depositFeeding <- subset(bodySize, feeding == 2)
miningFeeding <- subset(bodySize, feeding == 3)
grazingFeeding <- subset(bodySize, feeding == 4)
predatoryFeeding <- subset(bodySize, feeding == 5)
otherFeeding <- subset(bodySize, feeding == 6)

#*************************************************Feeding Type vs. Geologic Time***************************************************
par(col="black")

#plot(1:10,xlab="Feeding Type", ylab="Biovolume (log10mm^3)", xlim=c(1,6), ylim=c(0,30), type="n")
boxplot(log10(max_vol)~feeding, bodySize, xlab="Feeding Type", ylab="Biovolume (log10mm^3)", main="Biovolume vs. Feeding Type")

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
time.plot(c(0,6), "Mean Size per Feeding Type")
plot(timescale$age_bottom, my.mean[,3], type="n", pch=16, xlab="Geologic Time (Ma)", xlim=c(541, 0), ylab="Mean Size", ylim=c(1.2,6.5), main="Mean Size per Feeding Type")
my.col=c("blue1", "chartreuse2", "orange3", "darkorchid1", "deeppink1", "lightskyblue")
#loop per column
for(i in 1:6) {
  my.ts <- as.paleoTS(mm=my.mean[!is.na(my.var[,i]), i], vv=my.var[!is.na(my.var[,i]), i], nn=my.n[!is.na(my.var[,i]), i], tt=my.time[!is.na(my.var[,i])], oldest="last")
  fit3models(my.ts, method="Joint", pool=FALSE)
  #par(col=my.mean$color[k]); par(col="deepskyblue3")
  lines(timescale$age_mid, my.mean[, i], col=my.col[i])
}

par(col="black")
legend("topleft", legend=c("Feeding Type 1: Suspension", "Feeding Type 2: Deposit", "Feeding Type 3: Mining", "Feeding Type 4: Grazing", "Feeding Type 5: Predatory", "Feeding Type 6: Other"), col = my.col, lty = 1, title="Feeding Color Legend", bg = NA, box.col=NA, title.adj = 0.26, cex=0.47)


#95% confidence intervals
#par(col="deepskyblue")
#for (i in 1:n.bins) {
#  ci <- 1.96 * sqrt(my.var[i]) / sqrt(my.n[i])
#  my.x <- rep(timescale$age_bottom[i], 2)
#  my.y <- c(my.mean[i] + ci, my.mean[i] - ci)
#  lines(my.x, my.y, lwd=0.75)
#}

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

propOrange <- myProp[,1]
propBlue <- myProp[,2]
propPink <- myProp[,3]
propGreen <- myProp[,4]
propCyan <- myProp[,5]
propPurple <- myProp[,6]


myX <- c(timescale$age_mid, rev(timescale$age_mid))
myOrange <- c(rep(0, nrow(timescale)), rev(propOrange))
polygon(myX, myOrange, col="orange")
myBlue <- c(propOrange, rev(propOrange + propBlue))
polygon(myX, myBlue, col="royalblue")
myPink <- c((propOrange + propBlue), rev(propOrange + propBlue + propPink))
polygon(myX, myPink, col="orchid1")
myGreen <- c((propOrange + propBlue + propPink), rev(propOrange + propBlue + propPink + propGreen))
polygon(myX, myGreen, col="green")
myCyan <- c((propOrange + propBlue + propPink + propGreen), rev(propOrange + propBlue + propPink + propGreen + propCyan))
polygon(myX, myCyan, col="cyan")
myPurple <- c((propOrange + propBlue + propPink + propGreen + propCyan), rev(propOrange + propBlue + propPink + propGreen + propCyan + propPurple))
polygon(myX, myPurple, col="mediumorchid4")

legend(535, .23, legend=c("Feeding Type 1: Suspension", "Feeding Type 2: Deposit", "Feeding Type 3: Mining", "Feeding Type 4: Grazing", "Feeding Type 5: Predatory", "Feeding Type 6: Other"), col = my.col, lty = 1, title="Feeding Color Legend", bg = "white", box.col=NA, title.adj = 0.26, cex=0.5)



