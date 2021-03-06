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
