## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

updateRainPage <- function() {
	
	role <- sapply(hsp$data, attr, "role")
	
	setupIconView(theWidget("rain_iconview"), 
		itemNames=names(hsp$data)[role=="RAIN"], selection="all")
	
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
	
	if (any(!ok)) {
		errorDialog(paste("Some selected items do not have a valid 'location.xy' attribute:",
			paste(selNames[!ok], collapse=", "),
			". Try 'edit metadata' in the 'Dataset' tab."))
		return()
	}
	
	addLogComment("Generate locations map")
	
	guiDo(isExpression=T, bquote({
		tmp.locs <- sapply(hsp$data[.(selNames)], attr, "location.xy")
		tmp.locs <- data.frame(x=tmp.locs[1,], y=tmp.locs[2,])
	}))
	
	plot.cmd <- quote(
		xyplot(y ~ x, tmp.locs, aspect="iso", labels=rownames(tmp.locs),
			panel=function(..., labels) {
				panel.points(...)
				panel.text(..., labels=labels, pos=1, cex=0.7)
			}, prepanel=function(x, y, ...) {
				list(xlim=extendrange(x, f=0.1),
					ylim=extendrange(y, f=0.1))
			})
	)
	
	setPlotDevice("locations")
	setCairoWindowButtons("locations", zoomin=T, centre=T)
	
	result <- guiDo(plot.cmd, isExpression=T)
	# plot trellis object
	guiDo(print(result), doLog=F)
	
	.hydrosanity$call[["locations"]] <<- evalCallArgs(plot.cmd, pattern="^tmp")
	
	guiDo(rm(tmp.locs))
	
	setStatusBar("Generated locations map")
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
	if (length(selNames) < 4) {
		errorDialog("Need at least 4 items for spatial interpolation.")
		return()
	}
	nBlobs <- length(selNames)
	
	doOverall <- theWidget("rain_surface_overall_radiobutton")$getActive()
	doQuarters <- theWidget("rain_surface_quarters_radiobutton")$getActive()
	doMonths <- theWidget("rain_surface_months_radiobutton")$getActive()
	doAnnual <- theWidget("rain_surface_annual_radiobutton")$getActive()
	doSplines <- theWidget("rain_spline_radiobutton")$getActive()
	doExtrap <- theWidget("rain_spline_extrapolation_checkbutton")$getActive()
	gridSideCells <- theWidget("rain_gridcells_spinbutton")$getValue()
	showPoints <- theWidget("rain_showpoints_checkbutton")$getActive()
	showCounts <- theWidget("rain_showcounts_checkbutton")$getActive()
	startMonth <- theWidget("explore_yearstart_combobox")$getActive() + 1
	
	myType <- if (doOverall) { "overall" } else
		if (doAnnual) { "annual" } else
		if (doQuarters) { "quarters" } else
		if (doMonths) { "months" }
	
	loc <- lapply(hsp$data[selNames], attr, "location.xy")
	ok <- (sapply(loc, length) == 2)
	
	if (any(!ok)) {
		errorDialog(paste("Some selected items do not have a valid 'location.xy' attribute:",
			paste(selNames[!ok], collapse=", "),
			". Try 'edit metadata' in the 'Dataset' tab."))
		return()
	}
	
	addLogComment("Generate rainfall map")
	
	guiDo(isExpression=T, bquote({
		tmp.names <- .(selNames)
	}))
	
	guiDo({
		tmp.locs <- sapply(hsp$data[tmp.names], attr, "location.xy")
		tmp.locs <- data.frame(x=tmp.locs[1,], y=tmp.locs[2,])
	})
	
	spatial.cmd <- call('spatialField')
	spatial.cmd[[2]] <- quote(hsp$data[tmp.names])
	spatial.cmd$timelim <- if (!is.null(hsp$timePeriod)) { quote(hsp$timePeriod) }
	spatial.cmd$type <- myType
	spatial.cmd$start.month <- if (startMonth != 1) { startMonth }
	spatial.cmd$linear <- !doSplines
	spatial.cmd$extrap <- if (doExtrap) { T }
	spatial.cmd$xo.length <- gridSideCells
	spatial.cmd$countSurface <- if (showCounts) { T }
	
	grid.cmd <- quote(tmp.grid <- foo)
	grid.cmd[[3]] <- spatial.cmd
	guiDo(grid.cmd, isExpression=T)
	
	plot.cmd <- switch(myType,
		overall=quote(
			levelplot(z ~ x * y, tmp.grid, aspect="iso")
		),
		annual=quote(
			levelplot(z ~ x * y | year, tmp.grid, aspect="iso",
				as.table=T)
		),
		quarters=quote(
			levelplot(z ~ x * y | quarter, tmp.grid, aspect="iso",
				as.table=T)
		),
		months=quote(
			levelplot(z ~ x * y | month, tmp.grid, aspect="iso",
				as.table=T)
		)
	)
	
	if (showPoints) {
		plot.cmd$locations <- quote(tmp.locs)
		plot.cmd$panel <- function(..., locations) {
			panel.levelplot(...)
			panel.points(locations)
		}
	}
	
	# hydrosanity caption
	# TODO
	
	setPlotDevice("rainfall")
	setCairoWindowButtons("rainfall", identify=T, zoomin=T, centre=T)
	
	result <- guiDo(plot.cmd, isExpression=T)
	# plot trellis object
	guiDo(print(result), doLog=F)
	
	.hydrosanity$call[["rainfall"]] <<- evalCallArgs(plot.cmd, pattern="^tmp")
	
	# construct call to panel.identify() for later use
	id.cmd <- call('panel.identify')
	id.cmd$x <- tmp.locs
	id.cmd$labels <- rownames(tmp.locs)
	.hydrosanity$id.call[["rainfall"]] <<- id.cmd
	
	guiDo(rm(tmp.names, tmp.locs, tmp.grid))
	
	setStatusBar("Generated rainfall map")
}

