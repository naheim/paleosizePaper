#Porportion Plot for Tiering V1

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

#SPACE FOR COOL X-AXIS LABELS FOR GEOLOGIC AGES

plot(1:10, type = "n", xlim = c(541,0), ylim = c(0,1), xlab="Geologic Time (Ma)", ylab="Porportion of Genera")
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


