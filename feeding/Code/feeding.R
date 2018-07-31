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

boxplot(log10(max_vol)~feeding, bodySize, xlab="Feeding Type", ylim=c(-3, 12), ylab="", main="Biovolume vs. Feeding Type", col=myCol, names=c("Suspension", "Dep.", "Mining", "Grazing", "Predatory", "Other"), notch=TRUE)
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

time.plot(c(0,6), expression(paste("Biovolume (log  "[10]," mm"^3,")")), "Mean Size per Feeding Type")
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
legend(194, 1.3, legend=c("Feeding Type 1: Suspension", "Feeding Type 2: Deposit", "Feeding Type 3: Mining", "Feeding Type 4: Grazing", "Feeding Type 5: Predatory", "Feeding Type 6: Other"), col = myCol, lty = 1, title="Feeding Color Legend", bg = "white", box.col=NA, title.adj = 0.26, cex=0.47)

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
  temp.data <- log10(bodySize$max_vol[bodySize$fad_age > timescale$age_top[i] & bodySize$lad_age < timescale$age_bottom[i] & bodySize$feeding == 6])
  
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
#shading vector {,,,,,"cornflowerblue"}
polygon(c(timescale$age_mid[!is.na(my.var)], rev(timescale$age_mid[!is.na(my.var)])), c(my.mean[!is.na(my.var)] - ci[!is.na(my.var)], rev(my.mean[!is.na(my.var)] + ci[!is.na(my.var)])), col="cornflowerblue")
lines(timescale$age_mid, my.mean, col="#0000ff", lwd = 3.0)
mtext(side=3, line=0.25, "Mean size for 'Other' Feeders", col="black", font=4, cex=1.3)



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

#***************************************************Logistic Regression************************************************************
nBins <- nrow(timescale) # a variable of convenience for when the number of stages is used

# for this example plot, we're going to plot the size selectivity of the animals with feeding type = 1 to 6
FeedingType <- c("Suspension", "Surface Deposit", "Mining", "Grazing", "Predatory", "Other")
for(type in 1:6) {
  feed <- subset(sizeData, feeding==type)
  
  # define an empty data frame to hold all the proportions
  feedExtSel <- data.frame(matrix(NA, nrow=nBins, ncol=3, dimnames=list(timescale$interval_name, c('coef','ci.minus','ci.plus'))))
  
  # here is our loop to calculate extinction selectivity over time
  # this should be very familiar to you by now.
  for(i in 1:nBins) {
    temp <- subset(feed, fad_age > timescale$age_top[i] & lad_age < timescale$age_bottom[i]) # get all genera alive in interval
    
    
    if(nrow(temp) > 0) {
      # add a column to temp for extinction
      temp$extinct <- 0 # default give every genus a 0 (=survivor)
      temp$extinct[temp$lad_age >= timescale$age_top[i] & temp$lad_age < timescale$age_bottom[i]] <- 1 # assign a 1 to the victims
      
      # before we do the regression, we want to make sure there are at least 3 survivors & 3 victims
      if(sum(temp$extinct) >= 3 & nrow(temp) >= 6) {
        # run the logistic regression
        myGlm <- glm("extinct ~ log10(max_vol)", data=temp, family="binomial")
        summary(myGlm)
        
        # assign size coefficient to our output matrix
        feedExtSel$coef[i] <- myGlm$coefficients[2] # the second value is the size coefficient, the first is the intercept
        
        # get the conficence intervals
        myCi <- confint(myGlm)
        
        feedExtSel$ci.minus[i] <- myCi[2,1]
        feedExtSel$ci.plus[i] <- myCi[2,2]
      }
    }
  }
  
  # opens a new plot window with properly scaled and labeled axes
  title <- FeedingType[type]
  dev.new();
  time.plot(c(-2,2), "Log-odds of extinction", main=title)
  
  abline(h=0, lty=2) # adding a horizontal line at 0
  
  # the coefficients
  points(timescale$age_mid, feedExtSel$coef, pch=16, cex=1.25, col="tomato")
  
  # the error bars
  segments(timescale$age_mid,feedExtSel$ci.minus,timescale$age_mid,feedExtSel$ci.plus, col="#41dbb1")
}

#************************************************Extinction Rate vs. Feeding Type*************************************************
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
time.plot(c(-0.1,0.8), "Extinction Rate")
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

mtext(side=3, line=0.3, "Extinction Rate per Feeding Type", col="black", cex=0.9)
legend("topright", legend=(feedingType), fill=(feedColors), bg="white", title="Feeding Type", cex=0.75)
