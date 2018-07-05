# within each motility level, evalutes which evolutionary model best supports the trend in mean body size

library(paleoTS)
# reads in sizeData and timescale datasets
sizeData <- read.delim(file="https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/bodySizes.txt")
timescale <- read.delim(file="https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt")

# variable for time intervals
n.bins <- nrow(timescale)

#creating empty vectors
my.mean <- vector(mode="numeric", length=n.bins)
my.var <- vector(mode="numeric", length=n.bins)
my.n <- vector(mode="numeric", length=n.bins)
my.time <- timescale$age_bottom 

names(my.var) <- timescale$interval_name
names(my.n) <- timescale$interval_name
names(my.time) <- timescale$interval_name

motility1 <- sizeData[sizeData[,"motility"]==1 & !is.na(sizeData$motility),]

# for each interval, calculate mean body size for genera with motility level 1
for(i in 1:n.bins) {
temp.data <- log10(motility1$max_vol[motility1$fad_age > timescale$age_top[i] & motility1$lad_age < timescale$age_bottom[i]])
my.mean[i] <- mean(temp.data)
my.var[i] <- var(temp.data)
my.n[i] <- length(temp.data)
}

# outputs statistics for 3 models
my.ts <- as.paleoTS(mm=my.mean, vv=my.var, nn=my.n, tt=my.time, oldest="last")
fit3models(my.ts, method="Joint", pool=FALSE)

# REPEAT FOR EACH MOTILITY LEVEL, & NON-MOTILE (3-5) VS MOTILE (1-2)


