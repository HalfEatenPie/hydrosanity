## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

updateRainPage <- function() {
	
	setupIconView(theWidget("rain_iconview"), selection="all")
	
	.hydrosanity$update$rain <<- F
	theWidget(APPWIN)$present()
}

.hs_on_rain_view_locations_button_clicked <- function(button) {
	theWidget(APPWIN)$setSensitive(F)
	on.exit(theWidget(APPWIN)$setSensitive(T))
	setStatusBar("")
	
	selNames <- iconViewGetSelectedNames(theWidget("rain_iconview"))
	if (length(selNames) == 0) {
		errorDialog("No items selected.")
		return()
	}
	nBlobs <- length(selNames)
	
	loc <- lapply(hsp$data[selNames], attr, "location.xy")
	ok <- (sapply(loc, length) == 2)
	okNames <- names(loc)[ok]
	
	if (length(okNames) < 4) {
		errorDialog("No items with valid locations. ",
			"Try 'edit metadata' in the 'Dataset' tab.")
		return()
	}
	
	addLogComment("Generate rainfall map")
	
	guiDo(isExpression=T, bquote({
		tmp.locs <- sapply(hsp$data[.(okNames)], attr, "location.xy")
		tmp.locs <- data.frame(t(tmp.locs))
		names(tmp.locs) <- c("X", "Y")
	}))
	
	# hydrosanity caption
	# TODO
	
	plot.cmd <- quote(
		xyplot(Y ~ X, tmp.locs, aspect="iso", panel=function(...) {
			panel.points(...)
			panel.text(..., labels=rownames(tmp.locs), pos=1)
		}, prepanel=function(x, y, ...) {
			list(xlim=extendrange(x, f=0.1),
				ylim=extendrange(y, f=0.1))
		})
	)
	
	setPlotDevice("rainfall")
	setCairoWindowButtons("rainfall", zoomin=T)
	
	result <- guiDo(plot.cmd, isExpression=T)
	# plot trellis object
	guiDo(print(result), doLog=F)
	
	.hydrosanity$call[["rainfall"]] <<- evalCallArgs(plot.cmd, pattern="^tmp")
	
	guiDo(rm(tmp.locs))
	
	setStatusBar("Generated rainfall map")
}

.hs_on_rain_view_surface_button_clicked <- function(button) {
	theWidget(APPWIN)$setSensitive(F)
	on.exit(theWidget(APPWIN)$setSensitive(T))
	setStatusBar("")
	
	selNames <- iconViewGetSelectedNames(theWidget("rain_iconview"))
	if (length(selNames) == 0) {
		errorDialog("No items selected.")
		return()
	}
	nBlobs <- length(selNames)
	
	doOverall <- theWidget("rain_surface_overall_radiobutton")$getActive()
	doSeasonal <- theWidget("rain_surface_seasonal_radiobutton")$getActive()
	doAnnual <- theWidget("rain_surface_annual_radiobutton")$getActive()
	startMonth <- theWidget("explore_yearstart_combobox")$getActive() + 1
	
	loc <- lapply(hsp$data[selNames], attr, "location.xy")
	ok <- (sapply(loc, length) == 2)
	okNames <- names(loc)[ok]
	
	if (length(okNames) < 4) {
		errorDialog("Need at least 4 items with valid locations ",
			"(have only ", length(okNames), ").")
		return()
	}
	
	addLogComment("Generate rainfall map")
	guiDo(require(akima))
	
	guiDo(isExpression=T, bquote({
		tmp.locs <- sapply(hsp$data[.(okNames)], attr, "location.xy")
		tmp.locs <- data.frame(t(tmp.locs))
		names(tmp.locs) <- c("X", "Y")
	}))
	
	xo <- seq(min(tmp.locs$X), max(tmp.locs$X), length=40)
	yo <- seq(min(tmp.locs$Y), max(tmp.locs$Y), length=40)
	
	guiDo(isExpression=T, bquote({
		tmp.data <- lapply(hsp$data[.(okNames)], aggregate, by="months", fun.qual="omit")
		tmp.data <- sync.timeblobs(tmp.data, timelim=hsp$timePeriod)
	}))
	
	values <- list()
	counts <- list()
	if (doOverall) {
		values$overall <- matrix(as.numeric(0), nrow=40, ncol=40)
		counts$overall <- matrix(as.integer(0), nrow=40, ncol=40)
	}
	if (doSeasonal) {
		for (i in 1:12) {
			values[[i]] <- matrix(as.numeric(0), nrow=40, ncol=40)
			counts[[i]] <- matrix(as.integer(0), nrow=40, ncol=40)
		}
	}
	if (doAnnual) {
		values$this <- matrix(as.numeric(0), nrow=40, ncol=40)
		counts$this <- matrix(as.integer(0), nrow=40, ncol=40)
	}
	
	for (i in seq_along(tmp.data$Time)) {
		myTime <- tmp.data$Time[i]
		# store values$this in values[[year]]
		
		# generate surface for this month
		ok <- !is.na(tmp.data[i,-1])
		if (!any(ok)) { next }
		monthPoints <- as.numeric(tmp.data[i, which(ok)+1])
		monthSurf <- interp(x=tmp.locs$X[ok], y=tmp.locs$Y[ok], 
			z=monthPoints, xo=xo, yo=yo, linear=T)$z
		gridOK <- !is.na(monthSurf)
		
		thisIndex <- NA
		if (doOverall) { thisIndex <- "overall" }
		if (doSeasonal) { thisIndex <- as.POSIXlt(myTime)$mon+1 }
		if (doAnnual) { thisIndex <- "this" }
		counts[[thisIndex]] <- counts[[thisIndex]] + gridOK
		monthSurf[!gridOK] <- 0
		values[[thisIndex]] <- values[[thisIndex]] + monthSurf
		#print(myTime)
		#print(summary(c(counts$overall)))
	}
	#counts$overall[counts$overall==0] <- NA
	if (doOverall) {
		values$overall <- values$overall / counts$overall
	}
	if (doSeasonal) {
		for (i in 1:12) {
			values[[i]] <- values[[i]] / counts[[i]]
		}
	}
	if (doAnnual) {
		
	}
	
	tmp.grid <<- expand.grid(x=xo, y=yo)
	tmp.grid$z <<- as.vector(values$overall)
	
	plot.cmd <- quote(
		levelplot(z ~ x * y, tmp.grid, aspect="iso")
	)
	
	setPlotDevice("rainfall")
	setCairoWindowButtons("rainfall", zoomin=T)
	
	result <- guiDo(plot.cmd, isExpression=T)
	# plot trellis object
	guiDo(print(result), doLog=F)
	
	.hydrosanity$call[["rainfall"]] <<- evalCallArgs(plot.cmd, pattern="^tmp")
	
	guiDo(rm(tmp.locs, tmp.data))
	
	setStatusBar("Generated rainfall map")
}

