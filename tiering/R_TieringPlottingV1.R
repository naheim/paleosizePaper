R Script for Tiering V1


plot(1:10,1:10, type="n", pch=21, xlab="Geologic Time (Ma)", ylab="Amount of Gentra")


EXTRA: for(i in 1:1:6){plot(timescale$age_bottom, my.tiering[i,], xlim = c(541,0), type="l", pch=21

WORKS: 

sizeData = read.deim("sizeData.txt")
sizeData = subset(sizeData, !is.na(tiering) & tiering != 0)

n.bins = nrow(timescale)

sizeData$tiering = factor(sizeData$tiering, levels = 1:6)
my.tiering = data.frame()

for(i in 1:nrow(timescale)){temp.data = sizeData$tiering[sizeData$fad_age > timescale$age_top[i] & sizeData$lad_age < timescale$age_bottom[i]]
	temp.count = data.frame(table(temp.data))$Freq
	my.tiering = rbind(my.tiering, temp.count)
	}

plot(1:10,1:10, type="n", xlim=c(541,0), ylim=range(my.tiering), pch=21, xlab="Geologic Time (Ma)", ylab="Amount of Gentra")
for(i in 1:1:6){lines(timescale$age_bottom, my.tiering[,i])}

abline(v = c(66, 251.2), col=)