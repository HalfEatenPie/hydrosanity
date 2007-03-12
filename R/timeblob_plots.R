## Hydrosanity: an interface for exploring hydrological time series in R
##
## Time-stamp: <2007-03-05 00:00:00 Felix>
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

# savePlot(filename, type=c("png", "pdf", "ps"))
# ?plotcorr
# ?colorRampPalette


# if plotQualCodes is TRUE, then quality codes (as factor) are taken from column "Qual"
# and colMap must be a named list specifying the colours to plot for each code (factor level) in "Qual"
timelineplot <- function(xylist, xlim=NULL, plotQualCodes=F, colMap=list(good="black",maybe="orange",poor="red"), verticalGrid=T, ...) {
	# check types
	if (!identical(class(xylist),"list")) { xylist <- list(xylist) }
	if (any(sapply(xylist, is.timeblob)==F)) { stop("xylist must be a list of timeblobs") }
	if (is.null(xlim)) {
		xlim <- c(start.timeblob(xylist), end.timeblob(xylist))
	}
	if (length(xlim) != 2) { stop("xlim must be like c(start, end)") }
	xlim <- c(as.POSIXct(xlim[1]), as.POSIXct(xlim[2]))
	n_ts <- length(xylist)
	# set margins
	opar <- par(mar=c(3,4,1,1))
	on.exit(par(opar))
	# blank plot
	plot.new()
	plot.window(xlim, ylim=c(n_ts+0.5, 0.5), ...)
	#plot(0, type="n", xlim=xlim, ylim=c(n_ts+0.5, 0.5), axes=F, ann=F, ...)
	axisTicks <- axis.POSIXct(1, xlim[1])
	if (verticalGrid) {
		abline(v=axisTicks, col="grey", lty=2)
	}
	# draw series labels
	tmp_par <- par(xpd=T)
	text(xlim[1], y=seq(1,length(xylist)), labels=names(xylist), adj=c(1,0.5))
	par(tmp_par)
	# calculate the data time lines
	for (k in seq(along=xylist)) {
		subBlob <- window.timeblob(xylist[[k]], xlim[1], xlim[2], inclusive=T)
		if (nrow(subBlob)==0) { next }
		
		thisNA <- is.na(subBlob[,2])
		
		# default, if plotQualCodes == FALSE
		thisCol <- rep(as.factor("black"), nrow(subBlob))
		
		if (plotQualCodes && !is.null(subBlob$Qual)) {
			thisCol <- applyColourMap(subBlob$Qual, colMap)
		}
		
		# vector of integer codes for each colour as well as NA
		colIntCodes <- as.integer(thisCol)
		colIntCodes[thisNA] <- -1 # unique code for NA
		
		# find indices (time steps) where colour (or is.na) changes
		# i.e. divide up into blocks of equal colour
		blockStart <- which(c(1,diff(colIntCodes))!=0)
		blockEnd <- c(blockStart[-1], nrow(subBlob)+1)
		
		# get subset of colours and convert to character strings
		thisCol.block <- levels(thisCol)[ thisCol[blockStart] ]
		# set colour to NA where data is NA
		thisCol.block[thisNA[blockStart]] <- NA
		
		# extrapolate last interval to same length as second-last
		thisTime <- c(subBlob$Time, end.timeblob(subBlob) + diff(as.numeric(subBlob$Time[nrow(subBlob)-c(1,0)])) )
		
		# plot this timeline
		rect(thisTime[blockStart], k-0.15, thisTime[blockEnd], k+0.15, col=thisCol.block, border=NA)
	}
}


# TODO: allow plotting multiple series on each plot (specify lty/col for each plot?) -- eg smoothed
# TODO: quality codes?
timeseriesplot <- function(xylist, xlim=NULL, interactiveMode=F, showTimeline=interactiveMode, scrollSteps=3, ...) {
	# check types
	if (!identical(class(xylist),"list")) { xylist <- list(xylist) }
	if (any(sapply(xylist, is.timeblob)==F)) { stop("xylist must be a list of timeblobs") }
	if (is.null(xlim)) {
		xlim <- c(start.timeblob(xylist), end.timeblob(xylist))
	}
	if (length(xlim) != 2) { stop("xlim must be like c(start, end)") }
	xlim <- c(as.POSIXct(xlim[1]), as.POSIXct(xlim[2]))
	# setup
	n_ts <- length(xylist)
	plotRanges <- lapply(xylist, range.timeblob, na.rm=T)
	timeRange <- as.numeric(xlim[2]) - as.numeric(xlim[1])
	halfWin <- round(timeRange/2)
	currFocus <- as.numeric(xlim[1]) + halfWin
	halfWin <- halfWin * 1.04 # pretty padding
	prevFocus <- currFocus
	old_par <- par(mar=c(3,4,0,1),oma=c(0,0,1,0),xpd=F)
	on.exit(par(old_par))
	old_opt <- options(locatorBell=F)
	on.exit(options(old_opt),add=T)
	doplot <- T
	while (doplot) {
		plotFocus <- currFocus
		if (interactiveMode && (currFocus != prevFocus)) {
			scrollUnit <- halfWin / scrollSteps
			displaceSteps <- (currFocus - prevFocus) / scrollUnit
			plotFocus <- prevFocus + (currFocus - prevFocus) / abs(round(displaceSteps))
			if (!is.finite(plotFocus)) { plotFocus <- currFocus }
			prevFocus <- plotFocus
		}
		if (showTimeline) {
			# place the timeline below the other plots, but draw it before them
			layout(matrix(c(seq(n_ts+1,2),1)),heights=c(rep(1,n_ts),lcm(2)))
			tmp_par <- par(mar=c(1,4,0,4),xpd=NA)
			# blank plot
			plot.new()
			plot.window(xlim, ylim=c(0,1), xaxs="i")
			rect(xlim[1], 0, xlim[2], 1, col=grey(0.9))
			rect(max(as.numeric(xlim[1]),plotFocus-halfWin), 0, min(as.numeric(xlim[2]),plotFocus+halfWin), 1, col="grey")
			text(xlim[1], 0.5, get.year(xlim[1]), pos=4)
			text(xlim[2], 0.5, get.year(xlim[2]), pos=2)
			if (interactiveMode) {
				text(as.numeric(xlim[1]) + timeRange/2, 0.5, "Click on the plot (or here) to scroll through time.")
				text(xlim[1], 0.5, "zoom\nout", pos=2, offset=1, cex=1, font=2, family="mono", col=grey(0.25))
				text(xlim[2], 0.5, "zoom\n in", pos=4, offset=1, cex=1, font=2, family="mono", col=grey(0.25))
			}
			par(tmp_par)
		} else {
			layout(matrix(seq(1,n_ts)))
		}
		for (k in 1:n_ts) {
			plotStart <- as.POSIXct.raw(plotFocus-halfWin)
			plotEnd <- as.POSIXct.raw(plotFocus+halfWin)
			plotData <- window.timeblob(xylist[[k]], plotStart, plotEnd, inclusive=T)
			iDates <- seq(1,nrow(plotData))
			iVals <- iDates
			#if (dataType[1] == "mean") {
				# draw as steps (note: plot type="s" fails to draw horiz line preceding NA points)
				iDates <- c(rep(iDates,each=2)[-1],NA)
				iVals <- rep(iVals,each=2)
				type <- "l"
			#}
			plot.default(plotData$Time[iDates], plotData[iVals,2], xlim=c(plotFocus-halfWin,plotFocus+halfWin), xaxs="i", xaxt="n", xlab=" ", ylim=plotRanges[[k]], ylab=names(xylist)[k], type="l", ...)
			axis.POSIXct(1, as.POSIXct.raw(plotFocus))
		}
		if (interactiveMode && (currFocus == prevFocus)) {
			# wait for user to click
			newFocus <- locator(n=1)
			# if the user cancelled then exit
			if (is.null(newFocus)) {
				return(c(as.POSIXct.raw(currFocus-halfWin), as.POSIXct.raw(currFocus+halfWin)))
			}
			# if the click occurred on the timeline area
			# timeline area starts 3 lines from bottom of plot area (i.e. mar=3)
			timelineAreaY <- par("usr")[3] - mar.to.xy(yMar=3)
			if (showTimeline && (newFocus$y < timelineAreaY)) {
				# convert main plot coordinates into timeline coordinates (scaled to [0,1])
				# adjust for different margins: right side is an extra 3 lines in (mar=4)
				relX <- (newFocus$x - (currFocus-halfWin)) / (halfWin*2 - mar.to.xy(xMar=3))
				if (relX < 0) {
					# user clicked on (-) to zoom out (expand time window)
					halfWin <- halfWin * 2
					if (halfWin > timeRange/2) {
						halfWin <- round(timeRange/2)
						currFocus <- as.numeric(xlim[1]) + halfWin
						prevFocus <- currFocus
					}
				} else if (relX > 1) {
					# user clicked on (+) to zoom in (contract time window)
					tmp_par <- par(xpd=NA)
					msgAreaHeightLines <- par("fig")[3] * par("din")[2] / par("csi")
					msgAreaHeight <- mar.to.xy(yMar=msgAreaHeightLines)
					leftEdgeX <- par("usr")[1] - mar.to.xy(xMar=4)
					rightEdgeX <- par("usr")[2] + mar.to.xy(xMar=1)
					rect(leftEdgeX, timelineAreaY - msgAreaHeight, rightEdgeX, timelineAreaY, col="yellow")
					text(currFocus, timelineAreaY - msgAreaHeight/2, "Click at the start of the window (to zoom in to)...", cex=1.5)
					newWindowStart <- locator(n=1)
					if (is.null(newWindowStart)) { next }
					newWindowStart <- round(newWindowStart$x)
					abline(v=newWindowStart, lty=2, lwd=2, col="red")
					rect(leftEdgeX, timelineAreaY - msgAreaHeight, rightEdgeX, timelineAreaY, col="yellow")
					text(currFocus, timelineAreaY - msgAreaHeight/2, "OK, now click at the end of the window.", cex=1.5)
					newWindowEnd <- locator(n=1)
					if (is.null(newWindowEnd)) { next }
					newWindowEnd <- round(newWindowEnd$x)
					abline(v=newWindowEnd, lty=2, lwd=2, col="red")
					if (newWindowEnd < newWindowStart) {
						tmp <- newWindowEnd
						newWindowEnd <- newWindowStart
						newWindowStart <- tmp
					}
					halfWin <- (newWindowEnd - newWindowStart) / 2
					currFocus <- newWindowStart + halfWin
					prevFocus <- currFocus
					par(tmp_par)
				} else {
					# user clicked timeline to scroll
					if (halfWin == round(timeRange/2)) { next }
					# convert into timeline coordinates
					currFocus <- as.numeric(xlim[1]) + round(relX * timeRange)
					prevFocus <- currFocus
				}
			} else {
				# click occurred on plot area
				if (halfWin == round(timeRange/2)) { next }				
				currFocus <- round(newFocus$x)
			}
			if (is.na(scrollSteps) || scrollSteps == 0) { prevFocus <- currFocus }
		}
		if (!interactiveMode) { doplot <- F }
	}
}

fdcplot <- function(xylist, xlim=NULL, timelim=NULL, log="y", zeroLevel=0.1, normalScale=T, plotQualCodes=F, colMap=list(good="black",maybe="orange",poor="red"), plotPoints=1000, lwd=2, ...) {
	# check types
	if (!identical(class(xylist),"list")) { xylist <- list(xylist) }
	if (any(sapply(xylist, is.timeblob)==F)) { stop("xylist must be a list of timeblobs") }
	if (is.null(timelim)) {
		timelim <- c(start.timeblob(xylist), end.timeblob(xylist))
	}
	if (length(timelim) != 2) { stop("timelim must be like c(start, end)") }
	timelim <- c(as.POSIXct(timelim[1]), as.POSIXct(timelim[2]))
	if (is.null(xlim)) {
		xlim <- c(0.001, 0.999)
	}
	if (length(xlim) != 2) { stop("xlim must be like c(p1, p2)") }
	# setup
	n_ts <- length(xylist)
	plotRanges <- lapply(xylist, range.timeblob, na.rm=T)
	globalRange <- range(unlist(plotRanges))
	globalRange[1] <- max(zeroLevel, globalRange[1])
	probFn <- qunif; invProbFn <- punif
	if (normalScale) { probFn <- qnorm; invProbFn <- pnorm }
	# blank plot
	plot.new()
	plot.window(xlim=probFn(xlim), ylim=globalRange, log=log, ...)
	title(xlab="Probability of exceedence (%)")
	#plot(0, type="n", log=log, xlim=probFn(xlim), ylim=globalRange, yaxt="n", xaxt="n", xlab="Probability of exceedence (%)", ...)
	axis(2, las=2, at = c(0.1, 1, 10, 100, 1000, 10^4, 10^5, 10^6), labels = c("0.1", "1", "10", "100", "1000", "10^4", "10^5", "10^6"))
	axis(1, at=probFn(c(0.001, 0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.99, 0.999)), 
		labels=c("0.1", "1", "10", "20", "30", "40", "50", "60", "70", "80", "90", "99", "99.9"))
	for (k in 1:n_ts) {
		thisCDF <- window.timeblob(xylist[[k]], timelim[1], timelim[2])
		if (nrow(thisCDF)==0) { next }
		thisCDF <- thisCDF[order(thisCDF[,2], decreasing=T, na.last=NA),]
		thisProb <- seq(0, by=1/nrow(thisCDF), length.out=nrow(thisCDF))
		if (normalScale) { thisProb[1] <- thisProb[2]/2 } # make sure it shows on the plot
		# select a subset of points to avoid massive plots (a problem with postscript files)
		rowSet <- rep(FALSE, nrow(thisCDF))
		# always include first 10 and last 10 points
		rowSet[c(1:10, nrow(thisCDF)-0:9)] <- TRUE
		# sample points uniformly along quantiles of prob distribution
		probSet <- seq(probFn(xlim[1]), probFn(xlim[2]), length.out=plotPoints)
		rowSet[round(invProbFn(probSet) * nrow(thisCDF))] <- TRUE
		thisCol <- "black"
		if (plotQualCodes) {
			thisCol <- applyColourMap(thisCDF$Qual, colMap)
			thisCol <- levels(thisCol)[thisCol]
		}
		# plot this series distribution
		points(probFn(thisProb[rowSet]), thisCDF[rowSet,2], col=thisCol, pch=".", cex=lwd)
	}
}

monthlyboxplot <- function(myBlob, aggrFun=mean, zeroLevel=1, log="y", ...) {
	monthBlob <- aggregate.timeblob(myBlob, by="months", aggrFun=aggrFun)
	monthBlob[,2] <- pmax(monthBlob[,2], zeroLevel/10) # 0 values would be lost by log transform
	#myMonths <- months(monthBlob$Time, abbreviate=TRUE)
	monthNums <- sapply(monthBlob$Time, function(x) { as.POSIXlt(as.POSIXct.raw(x))$mon })
	monthList <- split(monthBlob[,2], monthNums)
	names(monthList) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
	globalMax <- max(monthBlob[,2], na.rm=T)
	boxplot(monthList, ylim=c(zeroLevel, globalMax), range=0, log=log, ...)
}

# returns factor, levels are colours
applyColourMap <- function(qualityCodes, colMap) {
	if (is.null(qualityCodes[1])) { stop("qualityCodes can not be NULL") }
	if (!is.list(colMap)) { stop("colMap must be a list") }
	if (!is.factor(qualityCodes[1])) { 
		warning(paste("qualityCodes must be a factor, skipping"))
		return(rep(factor("black"), length(qualityCodes)))
	}
	if (length(intersect(names(colMap),levels(qualityCodes[1])))==0) {
		warning(paste("levels of qualityCodes do not match given colMap, skipping"))
		return(rep(factor("black"), length(qualityCodes)))
	}
	# need to invert colMap list
	colLevelMap <- as.list(names(colMap))
	names(colLevelMap) <- unlist(colMap)
	# do the conversion into colours
	thisCol <- qualityCodes
	levels(thisCol) <- colLevelMap
	# TODO: handle any other levels (or NA) in qualColumn?
	#defaultCol <- match(colLevelMap[[1]], levels(thisCol))
	return(thisCol)
}

mar.to.xy <- function(xMar=NA, yMar=NA) {
	if (is.na(xMar) == is.na(yMar)) { stop("Give either xMar or yMar.") }
	if (!is.na(xMar)) {
		xyX_proportion <- (xMar * par("csi")) / par("pin")[1]
		xyX <- xyX_proportion * (par("usr")[2] - par("usr")[1])
		return(xyX)
	}
	if (!is.na(yMar)) {
		xyY_proportion <- (yMar * par("csi")) / par("pin")[2]
		xyY <- xyY_proportion * (par("usr")[4] - par("usr")[3])
		return(xyY)
	}
}


# modified to fix failure of missing() function
axis.POSIXct <- function (side, x, at, format, labels = TRUE, ...) 
{
    missing_format <- missing(format)
    mat <- missing(at) || is.null(at)
    if (!mat) 
        x <- as.POSIXct(at)
    else x <- as.POSIXct(x)
    range <- par("usr")[if (side%%2) 
        1:2
    else 3:4]
    d <- range[2] - range[1]
    z <- c(range, x[is.finite(x)])
    if (d < 1.1 * 60) {
        sc <- 1
        if (missing(format)) 
            format <- "%S"
    }
    else if (d < 1.1 * 60 * 60) {
        sc <- 60
        if (missing(format)) 
            format <- "%M:%S"
    }
    else if (d < 1.1 * 60 * 60 * 24) {
        sc <- 60 * 24
        if (missing(format)) 
            format <- "%H:%M"
    }
    else if (d < 2 * 60 * 60 * 24) {
        sc <- 60 * 24
        if (missing(format)) 
            format <- "%a %H:%M"
    }
    else if (d < 7 * 60 * 60 * 24) {
        sc <- 60 * 60 * 24
        if (missing(format)) 
            format <- "%a"
    }
    else {
        sc <- 60 * 60 * 24
        if (missing(format)) 
            format <- "%b %d"
    }
    if (d < 60 * 60 * 24 * 50) {
        zz <- pretty(z/sc)
        z <- zz * sc
        class(z) <- c("POSIXt", "POSIXct")
        if (sc == 60 * 60 * 24) 
            z <- as.POSIXct(round(z, "days"))
    }
    else if (d < 1.1 * 60 * 60 * 24 * 365) {
        class(z) <- c("POSIXt", "POSIXct")
        zz <- as.POSIXlt(z)
        zz$mday <- 1
        zz$isdst <- zz$hour <- zz$min <- zz$sec <- 0
        zz$mon <- pretty(zz$mon)
        m <- length(zz$mon)
        m <- rep.int(zz$year[1], m)
        zz$year <- c(m, m + 1)
        z <- as.POSIXct(zz)
        if (missing_format) 
            format <- "%b"
    }
    else {
        class(z) <- c("POSIXt", "POSIXct")
        zz <- as.POSIXlt(z)
        zz$mday <- 1
        zz$isdst <- zz$mon <- zz$hour <- zz$min <- zz$sec <- 0
        zz$year <- pretty(zz$year)
        z <- as.POSIXct(zz)
        if (missing_format) 
            format <- "%Y"
    }
    if (!mat) 
        z <- x[is.finite(x)]
    z <- z[z >= range[1] & z <= range[2]]
    if (identical(labels, TRUE)) 
        labels <- format(z, format = format)
    else if (identical(labels, FALSE)) 
        labels <- rep("", length(z))
    axis(side, at = z, labels = labels, ...)
}

