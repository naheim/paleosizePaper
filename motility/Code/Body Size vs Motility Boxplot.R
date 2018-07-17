# Body Size vs Motility Plot

# Subsets data without Motility Level = 0
sizeDataMod <- sizeData[sizeData[,"motility"]!=0 & !is.na(sizeData$motility),]

sizeDataMod$motile <- 0 #non-motile
sizeDataMod$motile[(sizeDataMod[,"motility"]==1 | sizeDataMod[,"motility"]==2) & !is.na(sizeDataMod$motility)] <- 1 #motile
sizeDataMod$motile <- factor(sizeDataMod$motile, levels=c(1,0))

boxplot(log10(sizeDataMod$max_vol)~sizeDataMod$motile,data=sizeDataMod,main = "Body Size vs Motility Boxplot", xlab="Motility Level", ylab="", col=c("blue","red"), names=c("Motile","Nonmotile"))
title(ylab=expression(paste("Biovolume log"[10],"mm"^3)), line=2.2)


boxplot(log10(sizeDataMod$max_vol)~sizeDataMod$motility,data=sizeDataMod,main = "Body Size vs Motility Level Boxplot", xlab="Motility Level", ylab="", col=c("darkorange", "dodgerblue","yellow", "forestgreen", "red", "purple"))
title(ylab=expression(paste("Biovolume log"[10],"mm"^3)), line=2.2)

