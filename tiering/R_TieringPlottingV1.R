R Script for Tiering V1


plot(1:10,1:10, type="n", pch=21, xlab="Geologic Time (Ma)", ylab="Amount of Gentra")


EXTRA: for(i in 1:1:6){plot(timescale$age_bottom, my.tiering[i,], xlim = c(541,0), type="l", pch=21


The Change in the Amount of Gentra Categorized by Tiering Level Over Million-Years

WORKS: 

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
time.plot(c(0,range(my.tiering)), "Proportion of Genera", mar = c(4, 3.5, 6, 3.5)+0.1, mgp = c(2.5, 0.75, 0))
#plot(1:10,1:10, type="n", xlim=c(541,-5), ylim=range(my.tiering), pch=21, xlab="Geologic Time (Ma)", ylab="Amount of Gentra")
for(i in 1:1:6){lines(timescale$age_bottom, my.tiering[,i], col=my.col[i])}
abline(v = c(65, 200, 251.2, 443.8), col="black")
mtext(side=3, line=0.5, "The Change in the Amount of Gentra Categorized by Tiering Level Over Million-Years", col="black", font=4, cex=1.3)
legend(540, 1197, legend=c("Tiering Level 1: Pelagic", "Tiering Level 2: Erect", "Tiering Level 3: Surficial", "Tiering Level 4: Semi-infaunal", "Tiering Level 5: Shallow infaunal", "Tiering Level 6: Deep infaunal"), col = my.col, lty = 1, title="Tiering Levels:", bg = "white", box.col=NA, title.adj = 0.31)
