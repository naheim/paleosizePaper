#Mean Size vs. Tiering Category

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