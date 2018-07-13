# Body Size vs Motility Plot

# Subsets data without Motility Level = 0
> sizeDataMod <- sizeData[sizeData[,"motility"]!=0 & !is.na(sizeData$motility),]

# Creates boxplot
> boxplot(log10(sizeDataMod$max_vol)~sizeDataMod$motility,data=sizeDataMod,main = "Body Size vs Motility Level", xlab="Motility Level", ylab="", col=c("darkorange", "dodgerblue","yellow", "forestgreen", "red", "purple"))
> title(ylab=expression(paste("Body Size log"[10],"mm"^3)), line=2.2)