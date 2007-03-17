## Hydrosanity: an interface for exploring hydrological time series in R
##
## Time-stamp: <2007-03-14 00:00:00 Felix>
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL


grid.timeline.plot <- function(blob.list, xscale=NULL, colMap=list(good="black",maybe="orange",poor="red"), thickness=unit(1.5,"lines"), maxLabelChars=20, pad=unit(4,"mm"), grill=T, newpage=T) {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	if (is.null(xscale)) {
		xscale <- c(start.timeblobs(blob.list), end.timeblobs(blob.list))
	}
	xscale <- as.POSIXct(xscale)
	if (any(is.na(xscale))) { stop("'xscale' must be a pair of valid times (POSIXt)") }
	# setup
	n <- length(blob.list)
	ylabs <- sapply(names(blob.list), toString, width=maxLabelChars)
	maxlab <- ylabs[ which.max(nchar(ylabs, "width")) ]
	if (newpage) { grid.newpage() }
	# layout for plot
	pushViewport(viewport(name="timeline.plot.layout",
		layout=grid.layout(2, 3,
			widths=unit.c(stringWidth(maxlab)+pad, unit(1,"null"), pad),
			heights=unit.c(unit(1,"null"), unit(3, "lines")))))
	# overall plot viewport, and layout for timeline bars
	pushViewport(viewport(name="timeline.bars.layout.vp", 
		layout.pos.col=2, layout.pos.row=1, xscale=xscale,
		layout=grid.layout(n*2+1, 1,
			heights=unit.c(rep(unit.c(unit(1,"null"), thickness), n), 
				unit(1,"null")))))
	# draw axis and grill
	tickX <- grid.xaxis.POSIXct(xscale, name="timeline.xaxis")
	if (grill) {
		grid.segments(tickX, unit(0,"npc"), tickX, unit(1,"npc")-pad,
			default.units="native", gp=gpar(col="grey"))
	}
	for (k in 1:n) {
		# draw timeline bar number k
		pushViewport(viewport(name=paste("timeline.bar",k,".vp",sep=''),
			layout.pos.row=k*2, xscale=xscale))
		grid.timeline.bar(blob.list[[k]], colMap=colMap)
		# draw label number k
		grid.text(ylabs[k], x=0, just="right",
			name=paste("label",k,sep=''))
		upViewport()
	}
	# come back up
	upViewport(2)
}


#if colMap=NULL then ignore quality codes, just plot non-NA values as black
grid.timeline.bar <- function(blob, colMap=NULL, name="timeline.bar") {
	# check types
	if (!is.timeblob(blob)) { stop("blob must be a timeblob") }
	xscale <- as.POSIXct.raw(convertX(unit(c(0,1), "npc"), "native"))
	# set up plot data
	subBlob <- window.timeblob(blob, xscale[1], xscale[2], inclusive=T)
	if (nrow(subBlob)==0) { return() }
	thisNA <- is.na(subBlob[,2])
	# default, if not plotting Qual codes
	thisCol <- rep(as.factor("black"), nrow(subBlob))
	if (!is.null(colMap) && !is.null(subBlob$Qual)) {
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
	# should extend last block with 1 timestep (from attr)
	# get subset of colours and convert to character strings
	blockCol <- levels(thisCol)[ thisCol[blockStart] ]
	# set colour to NA where data is NA
	blockCol[thisNA[blockStart]] <- NA
	# draw it
	grid.rect(x=subBlob$Time[blockStart], width=blockWidth, just="left",
		default.units="native", gp=gpar(fill=blockCol, col=NA),
		name=name)
}


grid.timeseries.plot <- function(blob.list, xscale=NULL, yscale=NULL, sameScales=T, logScale=F, zeroLevel=NULL, maxLabelChars=20, pad=unit(5,"mm"), superPos=1, newScale=T, newpage=!superPos, gp=gpar(col=colSet[superPos]), colSet=c("#0080ff", "#ff00ff", "darkgreen", "#ff0000", "orange", "#00ff00", "brown")) {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	if (is.null(xscale)) {
		xscale <- c(start.timeblobs(blob.list), end.timeblobs(blob.list))
	}
	xscale <- as.POSIXct(xscale)
	if (any(is.na(xscale))) { stop("'xscale' must be a pair of valid times (POSIXt)") }
	if (!is.null(yscale) && !is.numeric(yscale)) { stop("'yscale' must be numeric") }
	# setup
	n <- length(blob.list)
	blob.list <- lapply(blob.list, window.timeblob, xscale[1], xscale[2], inclusive=T)
	ylabs <- sapply(names(blob.list), toString, width=maxLabelChars)
	if (newpage) { grid.newpage() }
	# layout for plot
	if (superPos == 1) {
		pushViewport(viewport(name="timeseries.plot.layout",
			layout=grid.layout(2, 3,
			widths=unit.c(unit(4.5, "lines"), unit(1,"null"), pad),
			heights=unit.c(unit(1,"null"), unit(3, "lines")))))
		# overall plot viewport, and layout for timeseries plots
		pushViewport(viewport(name="timeseries.layout.vp", 
			layout.pos.col=2, layout.pos.row=1, xscale=xscale,
			layout=grid.layout(n*2, 1,
			heights=unit.c(rep(unit.c(pad, unit(1,"null")), n)))))
		# draw time axis with labels at bottom of plot
		grid.xaxis.POSIXct(xscale, name="timeseries.xaxis")
	}
	# calculate common yscale
	if (is.null(yscale) && sameScales) {
		yscale <- range(sapply(blob.list, range.timeblob, na.rm=T))
		if (doLog && (yscale[1] <= 0)) {
			# limit by minimum non-zero value (for log scale)
			yscale[1] <- min(sapply(blob.list,
				function(x){min(x[,2][x[,2]>0], na.rm=T)}))
		}
	}
	# plot each timeblob in the list
	for (k in 1:n) {
		# allow skipping for superposed series
		if (is.null(blob.list[[k]])) { next }
		# set up vertical scale for timeseries number k
		myYScale <- yscale
		if (is.null(yscale)) {
			myYScale <- range.timeblob(blob.list[[k]], na.rm=T)
			# need to ensure yscale > 0 for log scale
			if (logScale) {
				if (is.null(zeroLevel)) {
					if (myYScale[1] <= 0) {
						myYScale[1] <- nonzeromin(blob.list[[k]][,2], na.rm=T)
					}
				} else {
					myYScale[1] <- max(myYScale[1], zeroLevel)
				}
			}
		}
		if (logScale) {
			myYScale <- log10(myYScale)
		}
		if (superPos == 1) {
			# create viewport for timeseries number k
			pushViewport(viewport(name=paste("timeseries",k,".vp",sep=''),
				layout.pos.row=k*2, xscale=xscale, yscale=myYScale, clip="on"))
		} else {
			# navigate down to where timeseries number k was plotted
			downViewport(paste("timeseries",k,".vp",sep=''))
			if (newScale==T) {
				# push a new viewport to change scales
				pushViewport(viewport(name=paste("timeseries",k,"/",superPos,".vp",sep=''),
					layout.pos.row=k*2, xscale=xscale, yscale=myYScale, clip="on"))
			}
		}
		grid.timeseries.steps(blob.list[[k]], logScale=logScale,
			gp=gp, name=paste("timeseries",k,sep=''))
		# draw frame and axes
		if (superPos == 1) {
			pushViewport(viewport(xscale=xscale, yscale=myYScale, clip="off"))
			grid.rect()
			grid.xaxis.POSIXct(xscale, NA, labels=F)
			if (logScale) {
				grid.yaxis.log(myYScale)
				grid.yaxis.log(myYScale, main=F, label=F)
			} else {
				grid.yaxis()
				grid.yaxis(main=F, label=F)
			}
			# draw label number k
			grid.text(ylabs[k], x=unit(-4, "lines"), rot=90,
				name=paste("label",k,sep=''))
		}
		if ((superPos != 1) && newScale) {
			pushViewport(viewport(x=unit(-3,"lines"), just="left", clip="off"))
			grid.yaxis()
		}
		# come back up
		if (superPos == 1) { upViewport() } # axes
		if ((superPos != 1) && newScale) { upViewport(2) } # axes and scale
		upViewport() # from timeseriesk.vp
	}
	# come back up
	upViewport(2)
}


grid.timeseries.steps <- function(blob, logScale=F, gp=gpar(), name="timeseries") {
	# check types
	if (!is.timeblob(blob)) { stop("'blob' must be a timeblob") }
	# setup
	xscale <- as.POSIXct.raw(convertX(unit(c(0,1), "npc"), "native"))
	subBlob <- window.timeblob(blob, xscale[1], xscale[2], inclusive=T)
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


# plot superposed series
grid.timeseries.superpose <- function(blob.list, newScale=F, yscale=NULL, sameScales=T, logScale=F, superNum=2, gp=gpar(col=colSet[superNum]), colSet=c("#0080ff", "#ff00ff", "darkgreen", "#ff0000", "orange", "#00ff00", "brown")) {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	if (newScale && logScale) {
		stop("different superposed log scales are not supported")
	}
	# setup
	blob.list <- lapply(blob.list, window.timeblob, xscale[1], xscale[2], inclusive=T)
	# calculate common yscale
	if (is.null(yscale) && sameScales && newScale) {
		yscale <- range(sapply(blob.list, range.timeblob, na.rm=T))
		if (doLog && (yscale[1] <= 0)) {
			# limit by minimum non-zero value (for log scale)
			yscale[1] <- min(sapply(blob.list,
				function(x){min(x[,2][x[,2]>0], na.rm=T)}))
		}
	}
	# plot each timeblob in the list
	for (k in seq(along=blob.list)) {
		if (is.null(blob.list[[k]])) { next }
		# navigate down to where timeseries number k was plotted
		depth <- downViewport(paste("timeseries",k,".vp",sep=''))
		if (newScale==T) {
			# push a new viewport to change scales
			myYScale <- yscale
			if (is.null(yscale)) {
				myYScale <- range.timeblob(blob.list[[k]], na.rm=T)
			}
			xscale <- convertX(unit(c(0,1), "npc"), "native")
			pushViewport(viewport(xscale=xscale, yscale=myYScale, clip="on",
				name=paste("timeseries",k,"/",superNum,".vp",sep='')))
			depth <- depth + 1
		}
		# draw it
		grid.timeseries.steps(blob.list[[k]], logScale=logScale, gp=gp, 
			name=paste("timeseries",k,"/",superNum,sep=''))
		upViewport(depth)
	}
}


#qqmath(~ hsp$data[[1]][,2] + hsp$data[[2]][,2], ylim=c(0.1, 1000), scales=list(y=list(log=T)))


# returns factor with levels as colours (i.e. colour name/spec strings)
# any unspecified levels (or NAs in qualityCodes) gets first colour in colMap
applyColourMap <- function(qualityCodes, colMap) {
	# check types
	if (!is.factor(qualityCodes)) { 
		warning(paste("'qualityCodes' must be a factor, skipping"))
		return(rep(factor("black"), length(qualityCodes)))
	}
	if (!is.list(colMap)) { stop("'colMap' must be a list") }
	if (length(intersect(names(colMap),levels(qualityCodes)))==0) {
		warning(paste("levels of 'qualityCodes' do not match given 'colMap', skipping"))
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

grid.yaxis.log <- function(yscale, at=NULL, label=NULL, name="yaxis", ...) {
	if (is.null(at)) {
		# integer log powers
		at <- seq(ceiling(min(yscale)), max(yscale))
		if (diff(range(yscale)) < 3.9) {
			# make it a linear sequence log-transformed
			mags <- seq(floor(min(yscale)), max(yscale))
			logAt <- c()
			for (mag in mags) {
				logAt <- c(logAt, log10(seq(10^mag, 10^(mag+1), 
					length.out=10)))
			}
			logAt <- logAt[(min(yscale) <= logAt) & (logAt <= max(yscale))]
			if ((diff(range(yscale)) > 1.1) && !identical(label, F)) {
				# first draw ticks without labels
				# (can't just set these labels to ""; then main labels are not drawn)
				grid.yaxis(at=logAt, label=F,
					name=paste(name,".logticks",sep=''), ...)
			} else {
				at <- logAt
			}
		}
	}
	at <- unique(at)
	if (is.null(label)) {
		label <- sapply(10^at, format, digits=2, scientific=1)
		# large powers of 10 are better presented with an exponent
		bigIntPowers <- (at >= 5) & (at == round(at))
		label[bigIntPowers] <- paste("10^",at[bigIntPowers],sep='')
	}
	grid.yaxis(at=at, label=label, name=name, ...)
}

# modified version of axis.POSIXct (namespace:graphics) to work with grid
grid.xaxis.POSIXct <- function (range, x=NA, at, format, labels = TRUE, ...) 
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

