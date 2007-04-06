## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL


grid.timeline.plot <- function(blob.list, xscale=NULL, colMap=list(good="black",maybe="orange",poor="red"), thickness=unit(1.5,"lines"), maxLabelChars=20, pad=unit(4,"mm"), grill=T, main=NULL, sub=T, newpage=T) {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	if (is.null(xscale)) {
		xscale <- c(start.timeblobs(blob.list), end.timeblobs(blob.list))
	} else {
		xscale <- as.POSIXct(xscale)
		if (any(is.na(xscale))) { stop("'xscale' must be a pair of valid times (POSIXt)") }
		blob.list <- lapply(blob.list, window.timeblob, xscale[1], xscale[2], inclusive=T)
	}
	# setup
	n <- length(blob.list)
	ylabs <- sapply(names(blob.list), toString, width=maxLabelChars)
	maxlab <- ylabs[ which.max(nchar(ylabs, "width")) ]
	mainHeight <- unit(0, "npc")
	subHeight <- unit(0, "npc")
	if (!is.null(main) && !identical(main, F)) {
		if (is.character(main)) { main <- textGrob(main) }
		mainHeight <- grobHeight(mainHeight)
	}
	if (!is.null(sub) && !identical(sub, F)) {
		if (identical(sub, T)) {
			mySync <- sync.timeblobs(blob.list)
			dataPoints <- sum(is.na(unlist(mySync[-1]))==F)
			sub <- hydrosanity.caption(c(start.timeblobs(blob.list), end.timeblobs(blob.list)),
				by=attr(mySync, "timestep"), n=dataPoints, series=n)
		}
		if (is.character(sub)) { sub <- textGrob(sub) }
		subHeight <- grobHeight(sub)
	}
	if (newpage) { grid.newpage() }
	# layout for plot
	pushViewport(viewport(name="titles.layout",
		layout=grid.layout(3, 1,
			heights=unit.c(mainHeight, unit(1,"null"), subHeight))))
	pushViewport(viewport(name="timeline.plot.layout",
		layout.pos.row=2, 
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
			layout.pos.row=k*2, xscale=xscale, clip="on"))
		grid.timeline.bar(blob.list[[k]], colMap=colMap)
		# draw label number k
		pushViewport(viewport(clip="off"))
		grid.text(ylabs[k], x=0, just="right",
			name=paste("label",k,sep=''))
		upViewport()
		upViewport()
	}
	# come back up
	upViewport(2)
	# draw titles
	if (!is.null(main) && !identical(main, F)) {
		pushViewport(viewport(name="main.vp", layout.pos.row=1))
		grid.draw(main)
		upViewport()
	}
	if (!is.null(sub) && !identical(sub, F)) {
		pushViewport(viewport(name="sub.vp", layout.pos.row=3))
		grid.draw(sub)
		upViewport()
	}
	upViewport(1)
}

#if colMap=NULL then ignore quality codes, just plot non-NA values as black
grid.timeline.bar <- function(blob, colMap=NULL, name="timeline.bar", vp=NULL) {
	# check types
	if (!is.timeblob(blob)) { stop("blob must be a timeblob") }
	if (nrow(blob)==0) { return() }
	thisNA <- is.na(blob$Data)
	# default, if not plotting Qual codes
	thisCol <- rep(as.factor("black"), nrow(blob))
	if (!is.null(colMap) && !is.null(blob$Qual)) {
		thisCol <- applyColourMap(blob$Qual, colMap)
	}
	# vector of integer codes for each colour as well as NA
	colIntCodes <- as.integer(thisCol)
	colIntCodes[thisNA] <- -1 # unique code for NA
	# find indices (time steps) where colour (or is.na) changes
	# i.e. divide into blocks of continuous colour (for efficient plotting)
	blockStartIndex <- which(c(1,diff(colIntCodes))!=0)
	nBlock <- length(blockStartIndex)
	blockStart <- as.numeric(blob$Time[blockStartIndex])
	blockEnd <- c(blockStart[-1], end(blob))
	blockWidth <- blockEnd - blockStart
	# get subset of colours and convert to character strings
	blockCol <- levels(thisCol)[ thisCol[blockStartIndex] ]
	# set colour to NA where data is NA
	blockCol[thisNA[blockStartIndex]] <- NA
	# draw it
	grid.rect(x=blockStart, width=blockWidth, just="left",
		default.units="native", gp=gpar(fill=blockCol, col=NA),
		name=name, vp=vp)
}


grid.timeseries.plot.superpose <- function(superpose.blob.list, sameScalesGlobal=F, xscale=NULL, yscale=NULL, logScale=F, sub=T, ...) {
	# check types
	if (!identical(class(superpose.blob.list),"list")) {
		stop("'superpose.blob.list' must be a list of lists of timeblobs")
	}
	for (i in seq(along=superpose.blob.list)) {
		if (!identical(class(superpose.blob.list[[i]]),"list")) {
			stop("'superpose.blob.list' must be a list of lists of timeblobs")
		}
	}
	if (is.null(xscale)) {
		xscale <- c(min(lapply(superpose.blob.list, start.timeblobs)),
			max(lapply(superpose.blob.list, end.timeblobs)))
	} else {
		xscale <- as.POSIXct(xscale)
		if (any(is.na(xscale))) { stop("'xscale' must be a pair of valid times (POSIXt)") }
		for (i in seq(along=superpose.blob.list)) {
			superpose.blob.list[[i]] <- lapply(superpose.blob.list[[i]],
				window.timeblob, xscale[1], xscale[2], inclusive=T)
		}
	}
	# make common yscale
	if (sameScalesGlobal && is.null(yscale)) {
		allRanges <- sapply.timeblob.data(
			c(unlist(superpose.blob.list, recursive=F)), 
			range, finite=T)
		yscale <- range(allRanges[is.finite(allRanges)])
		if (logScale && (yscale[1] <= 0)) {
			# limit by minimum non-zero value (for log scale)
			allMins <- sapply.timeblob.data(
				c(unlist(superpose.blob.list, recursive=F)), 
				function(x){ min(x[x>0], na.rm=T) })
			yscale[1] <- min(allMins[is.finite(allMins)])
		}
	}
	# make caption
	if (identical(sub, T)) {
		mySync <- sync.timeblobs(c(unlist(superpose.blob.list, recursive=F)))
		dataPoints <- sum(is.na(unlist(mySync[-1]))==F)
		sub <- hydrosanity.caption(range(mySync$Time),
			by=attr(mySync, "timestep"), n=dataPoints, series=ncol(mySync)-1)
	}
	# call grid.timeseries.plot for each superposed layer
	for (layer in seq(along=superpose.blob.list)) {
		this.args <- list(...)
		this.args$blob.list <- superpose.blob.list[[layer]]
		this.args$superPos <- layer
		this.args$nSuperpose <- length(superpose.blob.list)
		this.args$xscale <- xscale
		this.args$yscale <- yscale
		this.args$logScale <- logScale
		if (!is.null(this.args$yscale)) { this.args$newScale <- F }
		this.args$sub <- sub
		do.call(grid.timeseries.plot, this.args)
	}
}


grid.timeseries.plot <- function(blob.list, xscale=NULL, yscale=NULL, sameScales=T, logScale=F, maxLabelChars=20, pad=unit(5,"mm"), superPos=1, newScale=T, main=NULL, sub=T, newpage=(superPos==1), nSuperpose=1, gp=gpar(col=colSet[superPos]), colSet=c("#0080ff", "#ff00ff", "darkgreen", "#ff0000", "orange", "#00ff00")) {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	if (is.null(xscale)) {
		if (superPos == 1) {
			xscale <- c(start.timeblobs(blob.list), end.timeblobs(blob.list))
		} else {
			depth <- downViewport("time.vp")
			xscale <- as.POSIXct.numeric(convertX(unit(c(0,1), "npc"), "native"))
			upViewport(depth)
		}
	} else {
		xscale <- as.POSIXct(xscale)
		if (any(is.na(xscale))) { stop("'xscale' must be a pair of valid times (POSIXt)") }
		blob.list <- lapply(blob.list, window.timeblob, xscale[1], xscale[2], inclusive=T)
	}
	if (!is.null(yscale) && !is.numeric(yscale)) { stop("'yscale' must be numeric") }
	# setup
	n <- length(blob.list)
	ylabs <- sapply(names(blob.list), toString, width=maxLabelChars)
	mainHeight <- unit(0, "npc")
	subHeight <- unit(0, "npc")
	if (!is.null(main) && !identical(main, F)) {
		if (is.character(main)) { main <- textGrob(main) }
		mainHeight <- grobHeight(mainHeight)
	}
	if (!is.null(sub) && !identical(sub, F)) {
		if (identical(sub, T)) {
			mySync <- sync.timeblobs(blob.list)
			dataPoints <- sum(is.na(unlist(mySync[-1]))==F)
			sub <- hydrosanity.caption(c(start.timeblobs(blob.list), end.timeblobs(blob.list)),
				by=attr(mySync, "timestep"), n=dataPoints, series=n)
		}
		if (is.character(sub)) { sub <- textGrob(sub) }
		subHeight <- grobHeight(sub)
	}
	if (newpage) { grid.newpage() }
	# layout for plot
	yLabSpace <- unit(1.5, "lines")
	yAxisWidth <- unit(3, "lines")
	yAxesSpace <- yAxisWidth
	if (newScale) { yAxesSpace <- yAxisWidth * nSuperpose }
	if (superPos == 1) {
		pushViewport(viewport(name="titles.layout",
		layout=grid.layout(3, 1,
			heights=unit.c(mainHeight, unit(1,"null"), subHeight))))
		pushViewport(viewport(name="timeseries.plot.layout",
			layout.pos.row=2, 
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
		allRanges <- sapply.timeblob.data(blob.list, range, finite=T)
		yscale <- range(allRanges[is.finite(allRanges)])
		if (logScale && (yscale[1] <= 0)) {
			# limit by minimum non-zero value (for log scale)
			allMins <- sapply.timeblob.data(blob.list, 
				function(x){ min(x[x>0], na.rm=T) })
			yscale[1] <- min(allMins[is.finite(allMins)])
		}
	}
	# plot each timeblob in the list
	for (k in 1:n) {
		# allow skipping for superposed series
		if (is.null(blob.list[[k]])) { next }
		# set up vertical scale for timeseries number k
		myYScale <- yscale
		if (is.null(myYScale) && nrow(blob.list[[k]])==0) {
			# no data for this series
			myYScale <- c(0, 1)
		}
		if (is.null(myYScale)) {
			myYScale <- range(blob.list[[k]]$Data, finite=T)
			# need to ensure yscale > 0 for log scale
			if (logScale) {
				if (myYScale[1] <= 0) {
					x <- blob.list[[k]]$Data
					myYScale[1] <- min(x[x>0], na.rm=T)
				}
			}
		}
		if (logScale) {
			myYScale <- log10(myYScale)
		}
		# extend yscale for clarity
		myYScale <- extendrange(myYScale)
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
	# draw titles
	if (superPos == 1) {
		if (!is.null(main) && !identical(main, F)) {
			pushViewport(viewport(name="main.vp", layout.pos.row=1))
			grid.draw(main)
			upViewport()
		}
		if (!is.null(sub) && !identical(sub, F)) {
			pushViewport(viewport(name="sub.vp", layout.pos.row=3))
			grid.draw(sub)
			upViewport()
		}
	}
	upViewport(1)
}


grid.timeseries.steps <- function(blob, logScale=F, name="timeseries", gp=NULL, vp=NULL) {
	# check types
	if (!is.timeblob(blob)) { stop("'blob' must be a timeblob") }
	if (nrow(blob)==0) { return() }
	# draw as steps (note: plot type="s" fails to draw horiz line preceding NA points)
	iDates <- seq(1, nrow(blob))
	iVals <- iDates
	iDates <- c(rep(iDates,each=2)[-1], NA)
	iVals <- rep(iVals,each=2)
	myData <- blob$Data
	if (logScale) {
		myData <- log10(myData)
		# set log(0)==-Inf to a finite value off bottom of plot
		yscale <- as.numeric(convertY(unit(c(0,1), "npc"), "native"))
		myData[na.omit(blob$Data==0)] <- yscale[1] - diff(yscale)
	}
	grid.lines(x=blob$Time[iDates], y=myData[iVals],
		default.units="native", name=name, gp=gp, vp=vp)
}


hydrosanity.caption <- function(timelim, by, n, series=NA, x=unit(1,"npc")-unit(1,"mm"), y=unit(1,"mm"), just=c("right","bottom"), gp=gpar(cex=0.6, col=grey(0.3))) {
	timelim <- as.POSIXct(timelim)
	if (any(is.na(timelim))) { stop("'timelim' must be a pair of valid times (POSIXt)") }
	if (!identical(by, "irregular") && !is.na(series)) {
		allN <- (length(seq(timelim[1], timelim[2], by=by))-1) * series
		pctN <- round(100*(n/allN), digits=0)
		if (pctN < 100) {
			n <- sprintf('%s=%s%%', n, pctN)
		}
	}
	vRSimple <- paste(R.version$major, R.version$minor, sep='.')
	textGrob(sprintf(
		"Data: %s to %s by %s (N=%s). Hydrosanity %s, R %s",
		timelim[1], timelim[2], by, n, VERSION, vRSimple),
		x=x, y=y, just=just, gp=gp, name="hydrosanity.caption")
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
	timelim <- as.POSIXct.numeric(lim)
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
				x == trunc.year(as.POSIXct.numeric(x))
			})
			label[atYears] <- format(at[atYears], format="%Y")
		}
	}
	grid.xaxis(at=at, label=label, name=name, ...)
	return(list(at=at, label=label))
}


lattice.y.prettylog <- function(lim, ...) {
	arglist <- list(...)
	have.log <- (!is.null(arglist$logsc)) && (!identical(arglist$logsc, F))
	tmp <- yscale.components.default(lim, ...)
	if (have.log) {
		stuff <- grid.yaxis.log(lim=lim, draw=F)
		tmp$left$ticks$at <- stuff$at
		tmp$left$labels$at <- stuff$at
		tmp$left$labels$labels <- stuff$label
	}
	return(tmp)
}

# prepanel.default.qqmath fails to take range(x, finite=T)
prepanel.qqmath.fix <- function(x, ...) {
	tmp <- prepanel.default.qqmath(x, ...)
	tmp$ylim <- range(x, finite=T) #extendrange(r=range(x, finite=T))
	tmp
}

# copied from lattice package because it is not exported
prepanel.default.qqmath <- function (x, f.value = NULL, distribution = qnorm, qtype = 7, 
    groups = NULL, subscripts, ...) 
{
    if (!is.numeric(x)) 
        x <- as.numeric(x)
    distribution <- if (is.function(distribution)) 
        distribution
    else if (is.character(distribution)) 
        get(distribution)
    else eval(distribution)
    nobs <- sum(!is.na(x))
    getxx <- function(x, f.value = NULL, nobs = sum(!is.na(x))) {
        if (is.null(f.value)) 
            distribution(ppoints(nobs))
        else if (is.numeric(f.value)) 
            distribution(f.value)
        else distribution(f.value(nobs))
    }
    getyy <- function(x, f.value = NULL, nobs = sum(!is.na(x))) {
        if (is.null(f.value)) 
            sort(x)
        else if (is.numeric(f.value)) 
            fast.quantile(x, f.value, names = FALSE, type = qtype, 
                na.rm = TRUE)
        else fast.quantile(x, f.value(nobs), names = FALSE, type = qtype, 
            na.rm = TRUE)
    }
    if (!nobs) 
        list(xlim = c(NA, NA), ylim = c(NA, NA), dx = NA, dy = NA)
    else if (!is.null(groups)) {
        sx <- split(x, groups[subscripts])
        xxlist <- lapply(sx, getxx, f.value = f.value)
        yylist <- lapply(sx, getyy, f.value = f.value)
        list(xlim = range(unlist(xxlist), na.rm = TRUE), ylim = range(unlist(yylist), 
            na.rm = TRUE), dx = unlist(lapply(xxlist, diff)), 
            dy = unlist(lapply(yylist, diff)))
    }
    else {
        xx <- getxx(x, f.value, nobs)
        yy <- getyy(x, f.value, nobs)
        list(xlim = range(xx), ylim = range(yy), dx = diff(xx), 
            dy = diff(yy))
    }
}

