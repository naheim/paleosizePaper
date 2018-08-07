#Figure 1: Proportional Diversity
#create 1 value vector of proportion of phylum time (corresponds to color) - 5 different ones needed
myProp <- matrix(NA, nrow=nrow(timescale), ncol=5)
for(i in 1:nrow(timescale)) {
  temp <- subset(sizeData, fad_age > timescale$age_top[i] & lad_age < timescale$age_bottom[i])
  counts <- table(temp$phylum)
  myProp[i,] = counts/sum(counts)
}

time.plot(c(0,1), "Proportion of Phylum (# of Genera)", cex.lab = 1.2, mar = c(4.5,4.5,4.5,4.5)+0.1, mgp=c(3, 0.75, 0), cex.axis = 1.25, width = 9)

mtext(side=3, line=0.5, "Proportional Diversity of the 5 Phyla", col="black", font=4, cex=2)

#assigning colors to proportions
propArthropoda <- myProp[,1]
propBrachiopoda <- myProp[,2]
propChordata <- myProp[,3]
propEchinodermata <- myProp[,4]
propMollusca <- myProp[,5]

#creating all the polygons - each builds upon previous ones
myX <- c(timescale$age_mid, rev(timescale$age_mid))
myArthropoda <- c(rep(0, nrow(timescale)), rev(propArthropoda))
polygon(myX, myArthropoda, col="#ee92ed")
myBrachiopoda <- c(propArthropoda, rev(propArthropoda + propBrachiopoda))
polygon(myX, myBrachiopoda, col="#ff5640")
myChordata <- c((propArthropoda + propBrachiopoda), rev(propArthropoda + propBrachiopoda + propChordata))
polygon(myX, myChordata, col="#ffd900")
myEchinodermata <- c((propArthropoda + propBrachiopoda + propChordata), rev(propArthropoda + propBrachiopoda + propChordata + propEchinodermata))
polygon(myX, myEchinodermata, col="#00ffd7")
myMollusca <- c((propArthropoda + propBrachiopoda + propChordata + propEchinodermata), rev(propArthropoda + propBrachiopoda + propChordata + propEchinodermata + propMollusca))
polygon(myX, myMollusca, col="#0000ff")

legend(120,0.98,legend=c("Arthropoda","Brachiopoda","Chordata","Echinodermata","Mollusca"),col=c("#ee92ed","#ff5640","#ffd900", "#00ffd7", "#0000ff"),box.lty=0,lty=c(rep(1,5), 5), lwd = c(rep(3,5), 1), bg = "white" )
#***********************************************************************************************************

#Figure 2: Diversification Rates
time.plot.mult(ncol = 3, nrow = 2,time.height=0.8,top.mar=1.5, cex.lab = 1.2, cex.axis = 1.25)

#All
plot(panel.first = c( abline(v=443.8, lty = 5, col = "azure4"), abline(v=358.9, lty = 5, col = "azure4"), abline(v=252.17, lty = 5, col = "azure4"), abline(v=201.3, lty = 5, col = "azure4"), abline(v=66, lty = 5, col = "azure4")), 1:10, type="n", xlim=c(541,0), ylim=c(0,0.125), xlab="", ylab= "Diversification Rate (%)", main = "a. General Trend", xaxt = "n")
abline(lm(divRate$Arthropoda ~ timescale$age_mid), col = "#ee92ed", lw = 3)
abline(lm(divRate$Brachiopoda ~ timescale$age_mid), col = "#ff5640", lw = 3)
abline(lm(divRate$Chordata ~ timescale$age_mid), col = "#ffd900", lw = 3)
abline(lm(divRate$Echinodermata ~ timescale$age_mid), col = "#00ffd7", lw = 3)
abline(lm(divRate$Mollusca ~ timescale$age_mid), col = "#0000ff", lw = 3)

#Arthropods
plot(panel.first = c( abline(v=443.8, lty = 5, col = "azure4"), abline(v=358.9, lty = 5, col = "azure4"), abline(v=252.17, lty = 5, col = "azure4"), abline(v=201.3, lty = 5, col = "azure4"), abline(v=66, lty = 5, col = "azure4"), abline(h = 0, col = "azure4")), timescale$age_mid,divRate$Arthropoda,type="l",col='#ee92ed',xlim=c(541,0), ylim=c(-0.8,1),yaxp  = c(-0.8, 1,9), xlab="", ylab= " Diversification Rate (%)",main="b. Arthropoda", xaxt = "n", lwd = 3)

#Brachiopods
plot(panel.first = c( abline(v=443.8, lty = 5, col = "azure4"), abline(v=358.9, lty = 5, col = "azure4"), abline(v=252.17, lty = 5, col = "azure4"), abline(v=201.3, lty = 5, col = "azure4"), abline(v=66, lty = 5, col = "azure4"), abline(h = 0, col = "azure4")), timescale$age_mid,divRate$Brachiopoda,type="l",col='#ff5640',xlim=c(541,0), ylim=c(-0.8,1),yaxp  = c(-0.8, 1,9), xlab="", ylab= "Diversification Rate (%)", main ="c. Brachiopoda", xaxt = "n",lwd = 3)

#Chordates
plot(panel.first = c( abline(v=443.8, lty = 5, col = "azure4"), abline(v=358.9, lty = 5, col = "azure4"), abline(v=252.17, lty = 5, col = "azure4"), abline(v=201.3, lty = 5, col = "azure4"), abline(v=66, lty = 5, col = "azure4"), abline(h = 0, col = "azure4")), timescale$age_mid,divRate$Chordata,type="l",col='#ffd900',xlim=c(541,0), ylim=c(-0.8,1),yaxp  = c(-0.8, 1,9), xlab="", ylab= "Diversification Rate (%)",main="d. Chordata", xaxt = "n",lwd = 3)

#Echinoderms
plot(panel.first = c( abline(v=443.8, lty = 5, col = "azure4"), abline(v=358.9, lty = 5, col = "azure4"), abline(v=252.17, lty = 5, col = "azure4"), abline(v=201.3, lty = 5, col = "azure4"), abline(v=66, lty = 5, col = "azure4"), abline(h = 0, col = "azure4")), timescale$age_mid,divRate$Echinodermata,type="l",col='#00ffd7',xlim=c(541,0), ylim=c(-0.8,1), yaxp  = c(-0.8, 1,9),xlab="", ylab= "Diversification Rate(%)", main = "e. Echinodermata", xaxt = "n",lwd = 3)

#Mollusks
plot(panel.first = c( abline(v=443.8, lty = 5, col = "azure4"), abline(v=358.9, lty = 5, col = "azure4"), abline(v=252.17, lty = 5, col = "azure4"), abline(v=201.3, lty = 5, col = "azure4"), abline(v=66, lty = 5, col = "azure4"), abline(h = 0, col = "azure4")), timescale$age_mid,divRate$Mollusca,type="l",col='#0000ff',xlim=c(541,0), ylim=c(-0.8,1), yaxp  = c(-0.8, 1,9),xlab="", ylab= "Diversification Rate(%)",main= "f. Mollusca", xaxt = "n", lwd = 3)
#***********************************************************************************************************

#Figure 3: Box Plots
boxplot(log10(max_vol)~phylum, data =sizeData, xlab="Phylum", ylim=c(-2,12), ylab="Biovolume (log10mm^3)", col=(c("#ee92ed", "#ff5640", "#ffd900", "#00ffd7", "#0000ff")), names=c("Arthropoda", "Brachiopoda", "Chordata", "Echinodermata", "Mollusca"), notch = TRUE, cex.lab = 1.2, mar = c(4.5,4.5,4.5,10.5)+0.1, mgp=c(3, 0.75, 0), cex.axis = 1.25)
mtext(side=3, line=0.5, "Comparison of Biovolume among the 5 Phyla", col="black", font=4, cex=2)
#***********************************************************************************************************

#Figure 4: Mean Sizes
time.plot.mult(nrow=2, ncol = 3,time.height=0.8,top.mar=1.5,cex.lab = 1.2, cex.axis = 1.25)

#All
plot(1:10, type="n", xlim=c(541,0), ylim=c(-2,12), xlab="", ylab= "Biovolume (log10mm3)",main = "a. Full Size Range", xaxt = "n" )
SizeData$color <- NA
SizeData$color[SizeData$phylum=="Arthropoda"] <- "#ee92edB3"
SizeData$color[SizeData$phylum=="Brachiopoda"] <- "#ff5640B3"
SizeData$color[SizeData$phylum=="Chordata"] <- "#ffd900B3"
SizeData$color[SizeData$phylum=="Echinodermata"] <- "#00ffd7B3"
SizeData$color[SizeData$phylum== "Mollusca"] <-  "#0000ffB3"

randOrder <- sample(1:nrow(SizeData), nrow(SizeData), replace=FALSE)
segments(SizeData$fad_age[randOrder], SizeData$max_vol[randOrder], SizeData$lad_age[randOrder], SizeData$max_vol[randOrder], col = SizeData$color[randOrder])

#Arthropods
plot(timescale$age_mid,meanSize$Arthropoda,type="l",col='#ee92ed',xlim=c(541,0), ylim=c(0,9), xlab="", ylab= "Biovolume (log10mm3) ", xaxt = "n", main = "b. Arthropoda", lwd = 3)
ci <- 1.96 * sqrt(varSize$Arthropoda) / sqrt(nSize$Arthropoda)
polygon(c(timescale$age_mid,rev(timescale$age_mid)),c(meanSize$Arthropoda - ci,rev(meanSize$Arthropoda + ci)),col=rgb(238, 146, 237, max = 255, alpha = 50))
abline(v=443.8, lty = 5, col = "azure4") #Ordivician
abline(v=358.9, lty = 5, col = "azure4") #Devonian
abline(v=252.17, lty = 5, col = "azure4") #Permian
abline(v=201.3, lty = 5, col = "azure4") #Triassic
abline(v=66,lty = 5, col = "azure4") #Cretaceous

#Brachiopods
plot(timescale$age_mid,meanSize$Brachiopoda,type="l",col='#ff5640',xlim=c(541,0), ylim=c(0,9), xlab="", ylab= "Biovolume (log10mm3)", xaxt = "n", main="c. Brachiopoda", lwd = 3)
ci <- 1.96 * sqrt(varSize$Brachiopoda) / sqrt(nSize$Brachiopoda)
polygon(c(timescale$age_mid,rev(timescale$age_mid)),c(meanSize$Brachiopoda - ci,rev(meanSize$Brachiopoda + ci)),col=rgb(255, 86, 64, max = 255, alpha = 50))
abline(v=443.8, lty = 5, col = "azure4") #Ordivician
abline(v=358.9, lty = 5, col = "azure4") #Devonian
abline(v=252.17, lty = 5, col = "azure4") #Permian
abline(v=201.3, lty = 5, col = "azure4") #Triassic
abline(v=66,lty = 5, col = "azure4") #Cretaceous

#Chordates
plot(timescale$age_mid,meanSize$Chordata,type="l",col='#ffd900',xlim=c(541,0), ylim=c(0,9), xlab="", ylab= "Biovolume (log10mm3)", xaxt = "n", main = "d. Chordata", lwd = 3)
ci <- 1.96 * sqrt(varSize$Chordata) / sqrt(nSize$Chordata)
polygon(c(timescale$age_mid,rev(timescale$age_mid)),c(meanSize$Chordata - ci,rev(meanSize$Chordata + ci)),col=rgb(255, 217, 0, max = 255, alpha = 50))
abline(v=443.8, lty = 5, col = "azure4") #Ordivician
abline(v=358.9, lty = 5, col = "azure4") #Devonian
abline(v=252.17, lty = 5, col = "azure4") #Permian
abline(v=201.3, lty = 5, col = "azure4") #Triassic
abline(v=66,lty = 5, col = "azure4") #Cretaceous

#Echinoderms
plot(timescale$age_mid,meanSize$Echinodermata,type="l",col='#00ffd7',xlim=c(541,0), ylim=c(0,9), xlab="", ylab= "Biovolume (log10mm3)", xaxt = "n", main = "e. Echinodermata", lwd = 3)
library(paleoTS)
n.bins <- nrow(timescale)
my.mean <- vector(mode="numeric", length=n.bins) 
my.var <- vector(mode="numeric", length=n.bins)
my.n <- vector(mode="numeric", length=n.bins)
my.time <- timescale$age_bottom

my.mean
names(my.mean) <- timescale$interval_name
my.mean
names(my.var) <- timescale$interval_name
names(my.n) <- timescale$interval_name
names(my.time) <- timescale$interval_name

SizeData <- sizeData
SizeData$max_vol <- log10(sizeData$max_vol)
EchinodermataSizeData <- sizeData[which(sizeData["phylum"] == "Echinodermata"),]
for(i in 1:n.bins) { 
	temp.data <- log10(EchinodermataSizeData$max_vol[EchinodermataSizeData$fad_age > timescale$age_top[i] & EchinodermataSizeData$lad_age < timescale$age_bottom[i]])
	my.mean[i] <- mean(temp.data)  
	my.var[i] <- var(temp.data)  # variance
	my.n[i] <- length(temp.data)  # sample size
}

polygon(c(timescale$age_mid[!is.na(my.var)], rev(timescale$age_mid[!is.na(my.var)])), c(my.mean[!is.na(my.var)] - ci[!is.na(my.var)], rev(my.mean[!is.na(my.var)] + ci[!is.na(my.var)])), col=rgb(0, 255, 215, max = 255, alpha = 50)) 
abline(v=443.8, lty = 5, col = "azure4") #Ordivician
abline(v=358.9, lty = 5, col = "azure4") #Devonian
abline(v=252.17, lty = 5, col = "azure4") #Permian
abline(v=201.3, lty = 5, col = "azure4") #Triassic
abline(v=66,lty = 5, col = "azure4") #Cretaceous

#Mollusks
plot(timescale$age_mid,meanSize$Mollusca,type="l",col='#0000ff',xlim=c(541,0), ylim=c(0,9), xlab="", ylab= "Biovolume (log10mm3)",xaxt = "n", main = "f. Mollusca", lwd = 3)
ci <- 1.96 * sqrt(varSize$Mollusca) / sqrt(nSize$Mollusca)
polygon(c(timescale$age_mid,rev(timescale$age_mid)),c(meanSize$Mollusca - ci,rev(meanSize$Mollusca + ci)),col=rgb(0, 0, 255, max = 255, alpha = 50))
abline(v=443.8, lty = 5, col = "azure4") #Ordivician
abline(v=358.9, lty = 5, col = "azure4") #Devonian
abline(v=252.17, lty = 5, col = "azure4") #Permian
abline(v=201.3, lty = 5, col = "azure4") #Triassic
abline(v=66,lty = 5, col = "azure4") #Cretaceous
#***********************************************************************************************************

#Table 1: AIC Weights
library(paleoTS)
n.bins <- nrow(timescale)
my.mean <- vector(mode="numeric", length=n.bins) 
my.var <- vector(mode="numeric", length=n.bins)
my.n <- vector(mode="numeric", length=n.bins)
my.time <- timescale$age_bottom

my.mean
names(my.mean) <- timescale$interval_name
my.mean
names(my.var) <- timescale$interval_name
names(my.n) <- timescale$interval_name
names(my.time) <- timescale$interval_name

#ARTHROPODS
ArthropodaSizeData <- sizeData[which(sizeData["phylum"] == "Arthropoda"),]
for(i in 1:n.bins) { 
	temp.data <- log10(ArthropodaSizeData$max_vol[ArthropodaSizeData$fad_age > timescale$age_top[i] & ArthropodaSizeData$lad_age < timescale$age_bottom[i]])
	my.mean[i] <- mean(temp.data)  
	my.var[i] <- var(temp.data)  # variance
	my.n[i] <- length(temp.data)  # sample size
}

my.ts <- as.paleoTS(mm=my.mean[!is.na(my.var)], vv=my.var[!is.na(my.var)], nn=my.n[!is.na(my.var)], tt=my.time[!is.na(my.var)], oldest="last") 
fit3models(my.ts, method="Joint", pool=FALSE)
plot(timescale$age_bottom, my.mean, xlim=c(541,0), ylim = c(-2,12), type="o", pch=16, xlab="Geologic time (Ma)", ylab=expression(paste("Mean body size (log"[10],"mm)")))

polygon(c(timescale$age_mid[!is.na(my.var)], rev(timescale$age_mid[!is.na(my.var)])), c(my.mean[!is.na(my.var)] - ci[!is.na(my.var)], rev(my.mean[!is.na(my.var)] + ci[!is.na(my.var)])), col= rgb(0,255,215, max = 255, alpha = 50))



#BRACHIOPODS
BrachiopodaSizeData <- sizeData[which(sizeData["phylum"] == "Brachiopoda"),]
for(i in 1:n.bins) { 
	temp.data <- log10(BrachiopodaSizeData$max_vol[BrachiopodaSizeData$fad_age > timescale$age_top[i] & BrachiopodaSizeData$lad_age < timescale$age_bottom[i]])
	my.mean[i] <- mean(temp.data)  
	my.var[i] <- var(temp.data)  # variance
	my.n[i] <- length(temp.data)  # sample size
}

my.ts <- as.paleoTS(mm=my.mean[!is.na(my.var)], vv=my.var[!is.na(my.var)], nn=my.n[!is.na(my.var)], tt=my.time[!is.na(my.var)], oldest="last") 
fit3models(my.ts, method="Joint", pool=FALSE)


#CHORDATES
ChordataSizeData <- sizeData[which(sizeData["phylum"] == "Chordata"),]
for(i in 1:n.bins) { 
	temp.data <- log10(ChordataSizeData$max_vol[ChordataSizeData$fad_age > timescale$age_top[i] & ChordataSizeData$lad_age < timescale$age_bottom[i]])
	my.mean[i] <- mean(temp.data)  
	my.var[i] <- var(temp.data)  # variance
	my.n[i] <- length(temp.data)  # sample size
}

my.ts <- as.paleoTS(mm=my.mean[!is.na(my.var)], vv=my.var[!is.na(my.var)], nn=my.n[!is.na(my.var)], tt=my.time[!is.na(my.var)], oldest="last") 
fit3models(my.ts, method="Joint", pool=FALSE)


#ECHINODERMS
EchinodermataSizeData <- sizeData[which(sizeData["phylum"] == "Echinodermata"),]
for(i in 1:n.bins) { 
	temp.data <- log10(EchinodermataSizeData$max_vol[EchinodermataSizeData$fad_age > timescale$age_top[i] & EchinodermataSizeData$lad_age < timescale$age_bottom[i]])
	my.mean[i] <- mean(temp.data)  
	my.var[i] <- var(temp.data)  # variance
	my.n[i] <- length(temp.data)  # sample size
}

my.ts <- as.paleoTS(mm=my.mean[!is.na(my.var)], vv=my.var[!is.na(my.var)], nn=my.n[!is.na(my.var)], tt=my.time[!is.na(my.var)], oldest="last") 
fit3models(my.ts, method="Joint", pool=FALSE)


#MOLLUSKS
MolluscaSizeData <- sizeData[which(sizeData["phylum"] == "Mollusca"),]
for(i in 1:n.bins) { 
	temp.data <- log10(MolluscaSizeData$max_vol[MolluscaSizeData$fad_age > timescale$age_top[i] & MolluscaSizeData$lad_age < timescale$age_bottom[i]])
	my.mean[i] <- mean(temp.data)  
	my.var[i] <- var(temp.data)  # variance
	my.n[i] <- length(temp.data)  # sample size
}

my.ts <- as.paleoTS(mm=my.mean[!is.na(my.var)], vv=my.var[!is.na(my.var)], nn=my.n[!is.na(my.var)], tt=my.time[!is.na(my.var)], oldest="last") 
fit3models(my.ts, method="Joint", pool=FALSE)

#ALL
for(i in 1:n.bins) { 
	temp.data <- log10(sizeData$max_vol[sizeData$fad_age > timescale$age_top[i] & sizeData$lad_age < timescale$age_bottom[i]])
	my.mean[i] <- mean(temp.data)  
	my.var[i] <- var(temp.data)  # variance
	my.n[i] <- length(temp.data)  # sample size
}

my.ts <- as.paleoTS(mm=my.mean[!is.na(my.var)], vv=my.var[!is.na(my.var)], nn=my.n[!is.na(my.var)], tt=my.time[!is.na(my.var)], oldest="last") 
fit3models(my.ts, method="Joint", pool=FALSE)
