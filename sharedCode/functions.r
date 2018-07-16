## Nice time series plots with geological time scale as x-axis
## plot.data must be a matrix or data frame with the first column as time (x-axis)
time.plot <- function(y.range, ylab="Time Series Data", main="", x.axis.pct=6, period.bars=FALSE, layout=FALSE, height=7, width=7, mar=c(2.75, 2.75, 1, 1) + 0.1, mgp=c(1.75, 1, 0), 
	y.log="", ts.cex=0.75, lab.length=1, show.plot=TRUE, file.name="Rplot.eps", inverse.colors=FALSE, ...) {
	# vertical scaling for time scale
	x.scale <- x.axis.pct/100
	
	# create a timescale axis
	periods <- rev(c(0.00, 23.03, 65.5, 145.5, 199.6, 251.0, 299.0, 359.2, 416.0, 443.7, 488.3, 542.0))
	names(periods) <- rev(c("Recent","Neogene","Paleogene","Cretaceous","Jurassic","Triassic","Permian","Carboniferous","Devonian","Silurian","Ordovician","Cambrian"))
	n.periods <- length(periods)
	period.mid <- vector(mode="numeric", length=length(periods)-1)
	for (i in 1:(length(periods)-1)) {
		period.mid[i] <- mean(periods[i:(i+1)])
	}
	cambrian <- max(periods)
	if(lab.length==1) {
		period.labs <- c("Cm", "O", "S", "D", "C", "P", "Tr", "J", "K", "Pg", "Ng")
	} else if (lab.length==2) {
		period.labs <- c("Cm", "O", "S", "D", "C", "P", "Tr", "J", "K", "Pg", "N")
	} else if (lab.length==3) {
		period.labs <- c("Cm", "O", "S", "D", "C", "P", "Tr", "J", "K", "P", "N")
	} else if (lab.length==4) {
		period.labs <- c("C", "O", "S", "D", "C", "P", "T", "J", "K", "P", "N")
	}
	period.poly <- c(1,3,5,7,9,11)
	
	if(y.log=="") {
		y.axis.labs <- pretty(y.range)
	} else if (y.log=="y") {
		min.y <- 10^ceiling(log10(min(y.range)))/10
		max.y <- 10^ceiling(log10(max(y.range)))
		if(max.y>1 & min.y<=1) {
			n.y <- abs(log10(min.y))+abs(log10(max.y))+1
		} else if (min.y>1) {
			n.y <- log10(max.y)
		} else if(max.y < 1) {
			n.y <- abs(log10(min.y))
		} else if(max.y==1) {
			n.y <- abs(log10(min.y))+1
		}
		y.axis.labs <- vector(mode="numeric"); y.axis.labs[1] <- min.y
		for(i in 2:n.y) {
			y.axis.labs[i] <- y.axis.labs[i-1]*10
		}
	} else {
		return(print("ERROR: Do not plot the time-axis on a log scale"))
	}
	if (y.log=="") {
		y.bottom <- min(y.axis.labs)-abs(max(y.axis.labs)*x.scale)
	} else {
		y.bottom <- min(y.axis.labs)*10*x.scale
	}
	y.top <- max(y.axis.labs)
	if(show.plot==TRUE & layout==FALSE & grepl("Windows", Sys.info()['sysname'])) {
		windows(height=height, width=width)
	} else if(show.plot==TRUE & layout==FALSE & grepl("Windows", Sys.info()['sysname'])) {
		quartz(height=height, width=width)
	} else if (show.plot==FALSE & layout==FALSE) {
		pdf(file=file.name, height=height, width=width)
	} else if (show.plot==FALSE & layout==TRUE) {
		return(print("ERROR: You can not use the layout function with the pdf function"))
	}
	
	if(inverse.colors==FALSE) {
		par(las=1, cex.axis=0.75, mar=mar, mgp=mgp, xaxs="i", yaxs="i", ...)
	} else {
		par(las=1, cex.axis=0.75, mar=mar, mgp=mgp, xaxs="i", yaxs="i", col.axis="white", col.lab="white", col.main="white", col.sub="white", col="white", bg="black", ...)
	}
	plot(periods, c(1:length(periods)), xlim=rev(range(periods)), ylim=c(y.bottom, y.top), main=main, xlab="Geologic Time (Ma)", ylab=ylab, type="n", axes=FALSE, log=y.log)
	if (period.bars==TRUE) {
		for(i in 1:length(period.poly)) {
			poly.x <- c(periods[period.poly[i]], periods[period.poly[i]+1], periods[period.poly[i]+1], periods[period.poly[i]])
			poly.y <- c(rep(min(y.axis.labs),2), rep(y.top,2))
			polygon(poly.x, poly.y, border=FALSE, col="gray90")
		}
	}
	if(inverse.colors==FALSE) {
		axis(side=1, at=c(seq(0,500,100), max(periods)), labels=FALSE, lwd.ticks=0, pos=y.bottom)
		axis(side=1, at=seq(0,500,100), lwd.ticks=1, lwd=0, pos=y.bottom, cex.lab=0.75)
		axis(side=2, at=y.axis.labs, lwd.ticks=1, pos=max(periods), cex=0.75)
	} else {
		axis(side=1, at=c(seq(0,500,100), max(periods)), labels=FALSE, lwd.ticks=0, pos=y.bottom, col="white")
		axis(side=1, at=seq(0,500,100), lwd.ticks=1, lwd=0, pos=y.bottom, cex.lab=0.75, col="white")
		axis(side=2, at=y.axis.labs, lwd.ticks=1, pos=max(periods), cex=0.75, col="white")
	}
	lines(x=c(cambrian, 0), y=rep(min(y.axis.labs), 2)) # horizontal line above time scale
	lines(x=rep(0, 2), y=c(min(y.axis.labs),y.top)) # right side of box
	lines(x=c(cambrian, 0), y=rep(y.top, 2)) # top side of box
	
	for(i in 1:n.periods) {
		lines(x=rep.int(periods[i],2),  y=c(min(y.axis.labs), y.bottom))
		if(i<n.periods & y.log=="") {
			text(x=period.mid[i], y=mean(c(min(y.axis.labs),y.bottom)), labels=period.labs[i], cex=ts.cex)
		} else if(i<n.periods &y.log=="y") {
			text(x=period.mid[i], y=10^mean(c(log10(min(y.axis.labs)),log10(y.bottom))), labels=period.labs[i], cex=ts.cex)
		}
	}
}
