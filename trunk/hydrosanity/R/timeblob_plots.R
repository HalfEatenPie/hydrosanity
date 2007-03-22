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
		layout=grid.layout(3, 3,
			widths=unit.c(stringWidth(maxlab)+pad, unit(1,"null"), pad),
			heights=unit.c(pad, unit(1,"null"), unit(3, "lines")))))
	# overall plot viewport, and layout for timeline bars
	pushViewport(viewport(name="time.vp", 
		layout.pos.col=2, layout.pos.row=2, xscale=xscale,
		layout=grid.layout(n*2+1, 1,
			heights=unit.c(unit(1,"null"), 
				rep(unit.c(thickness, unit(1,"null")), n)))))
	# draw axis and grill
	grid.lines(y=unit(0,"npc"))
	tickX <- grid.xaxis.POSIXt(name="timeline.xaxis")$at
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


grid.timeseries.plot.superpose <- function(superpose.blob.list, ...) {
	for (layer in seq(along=superpose.blob.list)) {
		this.args <- list(...)
		blob.list <- superpose.blob.list[[layer]]
		this.args$blob.list <- blob.list
		this.args$superPos <- layer
		this.args$nSuperpose <- length(superpose.blob.list)
		do.call(grid.timeseries.plot, this.args)
	}
}


grid.timeseries.plot <- function(blob.list, xscale=NULL, yscale=NULL, sameScales=T, logScale=F, zeroLevel=NULL, maxLabelChars=20, pad=unit(5,"mm"), superPos=1, newScale=T, newpage=(superPos==1), nSuperpose=1, gp=gpar(col=colSet[superPos]), colSet=c("#0080ff", "#ff00ff", "darkgreen", "#ff0000", "orange", "#00ff00")) {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	if (is.null(xscale)) {
		if (superPos == 1) {
			xscale <- c(start.timeblobs(blob.list), end.timeblobs(blob.list))
		} else {
			depth <- downViewport("time.vp")
			xscale <- as.POSIXct.raw(convertX(unit(c(0,1), "npc"), "native"))
			upViewport(depth)
		}
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
	yLabSpace <- unit(1.5, "lines")
	yAxisWidth <- unit(3, "lines")
	yAxesSpace <- yAxisWidth
	if (newScale) { yAxesSpace <- yAxisWidth * nSuperpose }
	if (superPos == 1) {
		pushViewport(viewport(name="timeseries.plot.layout",
			layout=grid.layout(3, 3,
			widths=unit.c(yLabSpace+yAxesSpace, unit(1,"null"), pad),
			heights=unit.c(pad, unit(1,"null"), unit(3, "lines")))))
		# overall plot viewport, and layout for timeseries plots
		pushViewport(viewport(name="time.vp", 
			layout.pos.col=2, layout.pos.row=2, xscale=xscale,
			layout=grid.layout(n*2-1, 1,
			heights=unit.c(rep(unit.c(unit(1,"null"), pad), n), unit(1,"null")) )))
		# draw time axis with labels at bottom of plot
		grid.xaxis.POSIXt(name="timeseries.xaxis")
	}
	# calculate common yscale
	if (is.null(yscale) && sameScales) {
		yscale <- range(sapply(blob.list, range.timeblob, na.rm=T))
		if (logScale && (yscale[1] <= 0)) {
			# limit by minimum non-zero value (for log scale)
			yscale[1] <- min(sapply(blob.list,
				function(x){min(x[,2][x[,2]>0], na.rm=T)}))
		}
	}
	# plot each timeblob in the list
	for (k in 1:n) {
		# allow skipping for superposed series
		if (is.null(blob.list[[k]]) || (nrow(blob.list[[k]])==0)) { next }
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
		# extend yscale for clarity
		myYScale[2] <- myYScale[2] + 0.05*diff(myYScale)
		if (!logScale) { myYScale[1] <- myYScale[1] - 0.05*diff(myYScale) }
		# do plot
		if (superPos == 1) {
			# create viewport for timeseries number k
			pushViewport(viewport(
				name=paste("timeseries",k,".vp",sep=''),
				layout.pos.row=k*2-1, 
				xscale=xscale, yscale=myYScale, clip="on"))
		} else {
			# navigate down to where timeseries number k was plotted
			downViewport(paste("timeseries",k,".vp",sep=''))
			if (newScale==T) {
				# push a new viewport to change scales
				pushViewport(viewport(
					name=paste("timeseries",k,"/",superPos,".vp",sep=''),
					xscale=xscale, yscale=myYScale, clip="on"))
			}
		}
		grid.timeseries.steps(blob.list[[k]], logScale=logScale,
			gp=gp, name=paste("timeseries",k,sep=''))
		# draw frame and axes
		if (superPos == 1) {
			pushViewport(viewport(xscale=xscale, yscale=myYScale, clip="off"))
			grid.rect()
			grid.xaxis.POSIXt(label=F)
			if (logScale) {
				grid.yaxis.log()
				grid.yaxis.log(main=F, label=F)
			} else {
				grid.yaxis()
				grid.yaxis(main=F, label=F)
			}
			# draw label number k
			grid.text(ylabs[k], x=-1*yAxesSpace-unit(1,"lines"), rot=90,
				name=paste("label",k,sep=''))
		}
		if ((superPos != 1) && newScale) {
			pushViewport(viewport(x=-1*yAxisWidth*(superPos-1), 
				just="left", yscale=myYScale, clip="off", gp=gp))
			if (logScale) { grid.yaxis.log() }
			else { grid.yaxis() }
		}
		# come back up
		if (superPos == 1) { upViewport() } # axes
		if ((superPos != 1) && newScale) { upViewport(2) } # axes and scale
		upViewport() # from timeseriesk.vp
	}
	# come back up
	upViewport(2)
}


grid.timeseries.steps <- function(blob, logScale=F, name="timeseries", gp=NULL, vp=NULL) {
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
	myData <- subBlob[,2]
	if (logScale) {
		myData <- log10(myData)
		# set log(0)==-Inf to a finite value off bottom of plot
		yscale <- as.numeric(convertY(unit(c(0,1), "npc"), "native"))
		myData[na.omit(subBlob[,2]==0)] <- yscale[1] - diff(yscale)
	}
	grid.lines(x=subBlob$Time[iDates], y=myData[iVals],
		default.units="native", name=name, gp=gp, vp=vp)
}


# returns factor with levels as colours (i.e. colour name/spec strings)
# any unspecified levels (or NAs in qualityCodes) gets first colour in colMap
applyColourMap <- function(qualityCodes, colMap) {
	# check types
	if (!is.factor(qualityCodes)) { 
		warning(paste("'qualityCodes' must be a factor (skipping)"))
		return(rep(factor("black"), length(qualityCodes)))
	}
	if (!is.list(colMap)) { stop("'colMap' must be a list") }
	if (length(intersect(names(colMap),levels(qualityCodes)))==0) {
		warning(paste("levels of 'qualityCodes' do not match 'colMap' (skipping)"))
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

grid.yaxis.log <- function(at=NULL, label=NULL, name="yaxis", lim=as.numeric(convertY(unit(c(0,1), "npc"), "native")), ...) {
	if (is.null(at)) {
		# integer log powers
		at <- seq(ceiling(min(lim)), max(lim))
		if (diff(range(lim)) < 3.9) {
			# make it a linear sequence log-transformed
			mags <- seq(floor(min(lim)), max(lim))
			logAt <- c()
			for (mag in mags) {
				logAt <- c(logAt, log10(seq(10^mag, 10^(mag+1), 
					length.out=10)))
			}
			logAt <- logAt[(min(lim) <= logAt) & (logAt <= max(lim))]
			if ((diff(range(lim)) > 1.1) && !identical(label, F)) {
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
	return(list(at=at, label=label))
}

grid.xaxis.POSIXt <- function(at=NULL, label=NULL, name="timeaxis", lim=as.numeric(convertX(unit(c(0,1), "npc"), "native")), ...) {
	timelim <- as.POSIXct.raw(lim)
	if (is.null(at)) {
		myBy <- "1 hour"
		myStart <- trunc(min(timelim), units="hours")
		myFormat <- "%m-%d %H:%M"
		if (diff(lim) > 12 * 60*60) {
			myBy <- "6 hours"
			myStart <- trunc(myStart, units="days")
		}
		if (diff(lim) > 1.5 * 24*60*60) {
			myBy <- "1 day"
			myFormat <- "%Y-%m-%d"
		}
		if (diff(lim) > 7 * 24*60*60) {
			myBy <- "4 days"
		}
		if (diff(lim) > 30 * 24*60*60) {
			myBy <- "1 month"
			myStart <- trunc.month(myStart)
			myFormat <- "%Y-%m"
		}
		if (diff(lim) > 6 * 30*24*60*60) {
			myBy <- "3 months"
			myStart <- trunc.year(myStart)
			myFormat <- "%b"
		}
		if (diff(lim) > 24 * 30*24*60*60) {
			myBy <- "1 year"
			myFormat <- "%Y"
		}
		if (diff(lim) > 10 * 365.25*24*60*60) {
			myBy <- "5 years"
			myStart <- trunc.decade(myStart)
		}
		if (diff(lim) > 30 * 365.25*24*60*60) {
			myBy <- "10 years"
		}
		at <- seq.POSIXt(myStart, max(timelim), by=myBy)
		# only keep axis ticks within range
		inrange <- (min(timelim) <= at) & (at <= max(timelim))
		at <- at[inrange]
		if (is.null(label)) {
			label <- format(at, format=myFormat)
			atYears <- sapply(at, function(x) {
				# TODO: make this stand-alone
				x == trunc.year(as.POSIXct.raw(x))
			})
			label[atYears] <- format(at[atYears], format="%Y")
		}
	}
	grid.xaxis(at=at, label=label, name=name, ...)
	return(list(at=at, label=label))
}


