# Body Size vs Motility Plot

# Subsets data without Motility Level = 0
sizeDataMod <- sizeData[which(sizeData[,"motility"]!=0)]

# Creates boxplot
boxplot(log10(sizeDataMod$max_vol)~sizeDataMod$motility,data=sizeDataMod, main="Body Size vs Motility", xlab="Motility Level", ylab=expression(paste("Body Size log"[10],"mm"^3)))
