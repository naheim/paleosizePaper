## Body Size vs Motility Plot ##

boxplot(log10(sizeData$max_vol)~sizeData$motility,data=sizeData, main="Body Size vs Motility", xlab="Motility Level", ylab=expression(paste("Body Size log"[10],"mm"^3)))
