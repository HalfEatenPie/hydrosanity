## Hydrosanity: an interface for exploring hydrological time series in R
##
## Time-stamp: <2007-03-14 00:00:00 Felix>
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL


grid.timeline.plot <- function(timeblob.list, xscale=NULL, colMap=list(good="black",maybe="orange",poor="red"), thickness=unit(1,"lines"), maxLabelChars=20, pad=unit(4,"mm"), grill=T, newpage=T) {
	# check types
	if (!identical(class(timeblob.list),"list")) { timeblob.list <- list(timeblob.list) }
	if (any(sapply(timeblob.list, is.timeblob)==F)) { stop("timeblob.list must be a list of timeblobs") }
	if (is.null(xscale)) {
		xscale <- c(start.timeblob(timeblob.list), end.timeblob(timeblob.list))
	}
	if (length(xscale) != 2) { stop("xscale must be like c(start, end)") }
	xscale <- c(as.POSIXct(xscale[1]), as.POSIXct(xscale[2]))
	# setup
	n <- length(timeblob.list)
	ylabs <- sapply(names(timeblob.list), toString, width=maxLabelChars)
	maxlab <- ylabs[ which.max(nchar(ylabs, "width")) ]
	if (newpage) { grid.newpage() }
	# layout for plot
	pushViewport(viewport(name="timeline.plot.layout",
		layout=grid.layout(2, 3,
			widths=unit.c(stringWidth(maxlab)+pad, unit(1,"null"), pad),
			heights=unit.c(unit(1,"null"), unit(3, "lines")))))
	# overall plot viewport, and layout for timeline bars
	pushViewport(viewport(name="timeline.bars.layout.vp", 
		layout.pos.col=2, layout.pos.row=1, xscale=xscale, gp=gp,
		layout=grid.layout(n*2+1, 1,
			heights=unit.c(rep(unit.c(unit(1,"null"), thickness), n), 
				unit(1,"null")))))
	# draw axis and grill
	tickX <- grid.xaxis.POSIXct(xscale, NA, name="timeline.xaxis")
	if (grill) {
		grid.segments(tickX, unit(0,"npc"), tickX, unit(1,"npc"),
			default.units="native", gp=gpar(col="grey", lwd=2, lty="dashed"))
	}
	for (k in 1:n) {
		# draw timeline bar number k
		pushViewport(viewport(name=paste("timeline.bar",k,".vp",sep=''),
			layout.pos.row=k*2, xscale=xscale))
		grid.timeline.bar(timeblob.list[[k]], xscale=xscale, colMap=colMap)
		# draw label number k
		grid.text(ylabs[k], x=0, just="right",
			name=paste("label",k,sep=''))
		upViewport()
	}
	# come back up
	upViewport(2)
}


#if colMap=NULL then ignore quality codes, just plot non-NA values as black
grid.timeline.bar <- function(timeblob, xscale=NULL, colMap=NULL, name="timeline.bar") {
	# check types
	if (!is.timeblob(timeblob)) { stop("timeblob must be a timeblob") }
	if (is.null(xscale)) {
		xscale <- c(start.timeblob(timeblob), end.timeblob(timeblob))
	}
	if (length(xscale) != 2) { stop("xscale must be like c(start, end)") }
	xscale <- c(as.POSIXct(xscale[1]), as.POSIXct(xscale[2]))
	# set up plot data
	subBlob <- window.timeblob(timeblob, xscale[1], xscale[2], inclusive=T)
	if (nrow(subBlob)==0) { return() }
	thisNA <- is.na(subBlob[,2])
	# default, if plotQualCodes == FALSE
	thisCol <- rep(as.factor("black"), nrow(subBlob))
	if (!is.null(colMap)) {
		thisCol <- applyColourMap(subBlob$Qual, colMap)
	}
	# vector of integer codes for each colour as well as NA
	colIntCodes <- as.integer(thisCol)
	colIntCodes[thisNA] <- -1 # unique code for NA
	# find indices (time steps) where colour (or is.na) changes
	# i.e. divide into blocks of continuous colour (for efficient plotting)
	blockStart <- which(c(1,diff(colIntCodes))!=0)
	nBlock <- length(blockStart)
	blockEnd <- c(blockStart[-1], nrow(subBlob))
	blockWidth <- (as.numeric(subBlob$Time[blockEnd]) - 
		as.numeric(subBlob$Time[blockStart]))
	# set last block to extend to plot boundary
	blockWidth[nBlock] <- (as.numeric(xscale[2]) - 
		as.numeric(subBlob$Time[blockStart[nBlock]]))
	# get subset of colours and convert to character strings
	blockCol <- levels(thisCol)[ thisCol[blockStart] ]
	# set colour to NA where data is NA
	blockCol[thisNA[blockStart]] <- NA
	# draw it
	grid.rect(x=subBlob$Time[blockStart], width=blockWidth, just="left",
		default.units="native", gp=gpar(fill=blockCol, col=NA),
		name=name)
}


grid.timeseries.plot <- function(timeblob.list, xscale=NULL, yscale=NULL, logScale=F, zerolevel=NULL, maxLabelChars=20, pad=unit(4,"mm"), gp=gpar(), newpage=T) {
	# check types
	if (!identical(class(timeblob.list),"list")) { timeblob.list <- list(timeblob.list) }
	if (any(sapply(timeblob.list, is.timeblob)==F)) { stop("timeblob.list must be a list of timeblobs") }
	if (is.null(xscale)) {
		xscale <- c(start.timeblob(timeblob.list), end.timeblob(timeblob.list))
	}
	if (length(xscale) != 2) { stop("xscale must be like c(start, end)") }
	xscale <- c(as.POSIXct(xscale[1]), as.POSIXct(xscale[2]))
	if (!is.null(yscale)) {
		if (length(yscale) != 2) { stop("yscale must be like c(lower, upper)") }
	}
	# setup
	n <- length(timeblob.list)
	ylabs <- sapply(names(timeblob.list), toString, width=maxLabelChars)
	if (newpage) { grid.newpage() }
	# layout for plot
	pushViewport(viewport(name="timeseries.plot.layout",
		layout=grid.layout(2, 3,
			widths=unit.c(unit(3, "lines"), unit(1,"null"), pad),
			heights=unit.c(unit(1,"null"), unit(3, "lines")))))
	# overall plot viewport, and layout for timeseries plots
	pushViewport(viewport(name="timeseries.layout.vp", 
		layout.pos.col=2, layout.pos.row=1, xscale=xscale,
		layout=grid.layout(n*2, 1,
			heights=unit.c(rep(unit.c(pad, unit(1,"null")), n)))))
	# draw axis
	grid.xaxis.POSIXct(xscale, NA, name="timeseries.xaxis")
	for (k in 1:n) {
		# draw timeseries number k
		myYScale <- yscale
		if (is.null(yscale)) {
			myYScale <- range.timeblob(timeblob.list[[k]], na.rm=T)
		}
		if (logScale) {
			if (is.null(zerolevel)) {
				myYScale[1] <- min(timeblob.list[[k]][
					(timeblob.list[[k]] > 0), 2], na.rm=T)
			} else {
				myYScale[1] <- zerolevel
			}
			myYScale <- log10(myYScale)
		}
		pushViewport(viewport(name=paste("timeseries",k,".vp",sep=''),
			layout.pos.row=k*2, xscale=xscale, yscale=myYScale, clip="on"))
		grid.timeseries.steps(timeblob.list[[k]], xscale=xscale, logScale=logScale,
			gp=gp, name=paste("timeseries",k,sep=''))
		pushViewport(viewport(clip="off"))
		grid.rect()
		grid.xaxis.POSIXct(xscale, NA, labels=F)
		if (logScale) {
			grid.yaxis.log()
			grid.yaxis.log(main=F, label=F)
		} else {
			grid.yaxis()
			grid.yaxis(main=F, label=F)
		}
		# draw label number k
		grid.text(ylabs[k], x=unit(-2, "lines"), just="left", rot=-90,
			name=paste("label",k,sep=''))
		upViewport(2)
	}
	# come back up
	upViewport(2)
}


grid.timeseries.steps <- function(timeblob, xscale=NULL, logScale=F, gp=gpar(), name="timeseries") {
	# check types
	if (!is.timeblob(timeblob)) { stop("timeblob must be a timeblob") }
	if (is.null(xscale)) {
		xscale <- c(start.timeblob(timeblob), end.timeblob(timeblob))
	}
	if (length(xscale) != 2) { stop("xscale must be like c(start, end)") }
	xscale <- c(as.POSIXct(xscale[1]), as.POSIXct(xscale[2]))
	# set up plot data
	subBlob <- window.timeblob(timeblob, xscale[1], xscale[2], inclusive=T)
	if (nrow(subBlob)==0) { return() }
	# draw as steps (note: plot type="s" fails to draw horiz line preceding NA points)
	iDates <- seq(1, nrow(subBlob))
	iVals <- iDates
	iDates <- c(rep(iDates,each=2)[-1], NA)
	iVals <- rep(iVals,each=2)
	transFn <- if (logScale) { log10 } else { I }
	grid.lines(x=subBlob$Time[iDates], y=transFn(subBlob[iVals,2]),
		default.units="native", gp=gp, name=name)
}


# returns factor with levels as colours (i.e. colour name/spec strings)
# any unspecified levels (or NAs in qualityCodes) gets first colour in colMap
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
	# handle any unspecified levels (or NAs in qualityCodes)
	thisCol[is.na(thisCol)] <- colMap[[1]]
	return(thisCol)
}

grid.yaxis.log <- function(at=log10(c(0.01, 0.1, 1, 10, 100, 1000, 10^4, 10^5, 10^6)), label=c("0.01", "0.1", "1", "10", "100", "1000", "10^4", "10^5", "10^6"), ...) {
	grid.yaxis(at=at, label=label, ...)
}

# modified version of axis.POSIXct (namespace:graphics) to work with grid
grid.xaxis.POSIXct <- function (range, x, at, format, labels = TRUE, ...) 
{
    mat <- missing(at) || is.null(at)
    if (!mat) 
        x <- as.POSIXct(at)
    else x <- as.POSIXct(x)
    d <- as.numeric(range[2]) - as.numeric(range[1])
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
    }
    if (d < 60 * 60 * 24 * 50) {
        zz <- pretty(z/sc)
        z <- zz * sc
        class(z) <- c("POSIXt", "POSIXct")
        if (sc == 60 * 60 * 24) 
            z <- as.POSIXct(round(z, "days"))
        if (missing(format)) 
            format <- "%b %d"
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
        if (missing(format)) 
            format <- "%b"
    }
    else {
        class(z) <- c("POSIXt", "POSIXct")
        zz <- as.POSIXlt(z)
        zz$mday <- 1
        zz$isdst <- zz$mon <- zz$hour <- zz$min <- zz$sec <- 0
        zz$year <- pretty(zz$year)
        z <- as.POSIXct(zz)
        if (missing(format)) 
            format <- "%Y"
    }
    if (!mat) 
        z <- x[is.finite(x)]
    z <- z[z >= range[1] & z <= range[2]]
    if (identical(labels, TRUE)) 
        labels <- format(z, format = format)
    else if (identical(labels, FALSE)) 
        labels <- rep("", length(z))
    grid.xaxis(at = z, label = labels, ...)
    return(z)
}

