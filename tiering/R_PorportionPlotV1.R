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
mtext(side=3, line=0.5, "The Change in the Proportions of Gentra Categorized by Tiering Level Over Million-Years", col="black", font=2, cex=1.3)
my.col = c("red", "orange", "green", "cyan", "magenta", "blue")
par(xpd=TRUE)
legend(550, 1.16, legend=c("Tiering Level 1: Pelagic", "Tiering Level 2: Erect", "Tiering Level 3: Surficial", "Tiering Level 4: Semi-infaunal", "Tiering Level 5: Shallow infaunal", "Tiering Level 6: Deep infaunal"), col = my.col, lty = 1, title="Tiering Color Legend", bg = "NA", box.col=NA, title.adj = 0.26, cex=0.55, text.font = 4)

