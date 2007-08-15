## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

updateRainPage <- function() {
	StateEnv$update$rain <- F
	StateEnv$win$present()
}

.hs_on_rain_view_surface_button_clicked <- function(button) {
	StateEnv$win$setSensitive(F)
	on.exit(StateEnv$win$setSensitive(T))
	setStatusBar("")
	
	selNames <- iconViewGetSelectedNames(theWidget("selection_iconview"))
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
	showCounts <- theWidget("rain_showcounts_checkbutton")$getActive()
	
	myType <- if (doOverall) { "overall" } else
		if (doAnnual) { "annual" } else
		if (doQuarters) { "quarters" } else
		if (doMonths) { "months" }
	
	loc <- lapply(hsp$data[selNames], attr, "location.xy")
	ok <- (sapply(loc, length) == 2)
	
	if (any(!ok)) {
		errorDialog(paste("Some selected items do not have a valid 'location.xy' attribute:",
			paste(selNames[!ok], collapse=", "),
			". De-select them, or try 'edit metadata' in the 'Dataset' tab."))
		return()
	}
	
	addLogComment("Generate rainfall map")
	
	tmpObjs <- c('tmp.names')
	
	guiDo(call=bquote({
		tmp.names <- .(selNames)
	}))
	
	tmpObjs <- c(tmpObjs, 'tmp.locs')
	guiDo({
		tmp.locs <- sapply(hsp$data[tmp.names], attr, "location.xy")
		tmp.locs <- data.frame(x=tmp.locs[1,], y=tmp.locs[2,])
	})
	
	grid.call <- call('spaceTimeField')
	grid.call[[2]] <- quote(hsp$data[tmp.names])
	grid.call$timelim <- if (!is.null(hsp$timePeriod)) quote(hsp$timePeriod)
	grid.call$type <- myType
	grid.call$start.month <- hsp$startMonth
	grid.call$linear <- !doSplines
	grid.call$extrap <- if (doExtrap) T
	grid.call$countSurface <- if (showCounts) T
	
	tmpObjs <- c(tmpObjs, 'tmp.grid')
	grid.assign.call <- quote(tmp.grid <- foo)
	grid.assign.call[[3]] <- grid.call
	guiDo(call=grid.assign.call)
	
	plot.call <- switch(myType,
		overall=quote(
			levelplot(z ~ x * y, tmp.grid, aspect="iso")
		),
		annual=quote(
			levelplot(z ~ x * y | year, tmp.grid, aspect="iso",
				as.table=T, layout=c(0,12))
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
	
	plot.call$panel <- function(..., points.xy) {
		panel.levelplot(...)
		panel.worldmap()
		panel.rivers()
		panel.cities()
		if (!is.null(hsp$catchment))
			sp.polygons(hsp$catchment)
		panel.points(points.xy)
		quote(panel.text(points.xy, labels=row.names(points.xy)))
	}
	plot.call$points.xy <- quote(tmp.locs)
	
	plot.call$prepanel <- quote(prepanel.extend.10)
	if (!is.null(hsp$region)) {
		plot.call$xlim <- quote(hsp$region$xlim)
		plot.call$ylim <- quote(hsp$region$ylim)
	}
	
	# hydrosanity caption
	# TODO
	
	id.call <- call('panel.identify')
	id.call$x <- tmp.locs
	id.call$labels <- rownames(tmp.locs)
	
	addToLog(paste(deparse(plot.call), collapse="\n"))
	guiDo(playwith(plot.call=plot.call, name="rainfall map", 
		extra.buttons=hydrosanityButtons[c('setregion')], 
		identify.call=id.call, 
		eval.args="^hsp$", invert=T, restore.on.close=StateEnv$win), 
		doLog=F)
	
	if (length(tmpObjs) > 0) {
		guiDo(call=bquote(rm(list=.(tmpObjs))))
	}
	
	setStatusBar("Generated rainfall map")
}

.hs_on_rain_view_mosaic_button_clicked <- function(button) {
	StateEnv$win$setSensitive(F)
	on.exit(StateEnv$win$setSensitive(T))
	setStatusBar("")
	
	selNames <- iconViewGetSelectedNames(theWidget("selection_iconview"))
	if (length(selNames) == 0) {
		errorDialog("No items selected.")
		return()
	}
	if (length(selNames) < 4) {
		errorDialog("Need at least 4 items for spatial interpolation.")
		return()
	}
	nBlobs <- length(selNames)
	
	loc <- lapply(hsp$data[selNames], attr, "location.xy")
	ok <- (sapply(loc, length) == 2)
	
	if (any(!ok)) {
		errorDialog(paste("Some selected items do not have a valid 'location.xy' attribute:",
			paste(selNames[!ok], collapse=", "),
			". De-select them, or try 'edit metadata' in the 'Dataset' tab."))
		return()
	}
	
	if (is.null(hsp$catchment)) {
		errorDialog("This function requires a catchment polygon (import it from 'Scope' tab).")
		return()
	}
	
	addLogComment("Generate Voronoi mosaic")
	
	tmpObjs <- c('tmp.names')
	
	guiDo(call=bquote({
		tmp.names <- .(selNames)
	}))
	
	tmpObjs <- c(tmpObjs, 'tmp.locs')
	guiDo({
		tmp.locs <- sapply(hsp$data[tmp.names], attr, "location.xy")
		tmp.locs <- data.frame(x=tmp.locs[1,], y=tmp.locs[2,])
	})
	
	guiDo({
		tmp.poly <- hsp$catchment@polygons[[1]]@Polygons[[1]]
		subAreas <- arealSubPolygons(tmp.locs, boundary=coordinates(tmp.poly))
		subAreasSP <- SpatialPolygonsDataFrame(subAreas, 
			data=data.frame(1:nrow(tmp.locs), row.names=row.names(tmp.locs)))
	})
	
	plot.call <- quote(xyplot(y ~ x, tmp.locs, aspect="iso"))
	plot.call$panel <- function(x, y, labels, polys, polySites, ...) { }
	
	body(plot.call$panel)$worldmap <- quote(panel.worldmap())
	body(plot.call$panel)$cities <- quote(panel.cities())
	
	plot.call$poly <- quote(subAreasSP)
	body(plot.call$panel)$polygons <- quote(sp.polygons(polys))
	
	body(plot.call$panel)$points <- quote(quote(panel.points(x, y)))
	
	plot.call$labels <- quote(tmp.names)
	body(plot.call$panel)$labels <- quote(panel.text(x, y, labels, ...))
	
	guiDo(polySites <- match(getSpPPolygonsIDSlots(subAreasSP), tmp.names))
	plot.call$polySites <- quote(polySites)
	body(plot.call$panel)$centroids <- quote(centroids <- getSpPPolygonsLabptSlots(polys))
	body(plot.call$panel)$arrows <- quote(panel.segments(x0=x[polySites], y0=y[polySites],
		x1=centroids[,1], y1=centroids[,2], lty=2, col="red"))
	
	if (!is.null(hsp$region)) {
		plot.call$xlim <- quote(hsp$region$xlim)
		plot.call$ylim <- quote(hsp$region$ylim)
	} else {
		plot.call$prepanel <- quote(prepanel.extend.10)
	}
	
	addToLog(paste(deparse(plot.call), collapse="\n"))
	guiDo(playwith(plot.call=plot.call, name="Voronoi mosaic", 
		extra.buttons=hydrosanityButtons[c('setregion')],
		eval.args="^hsp$", invert=T, restore.on.close=StateEnv$win), 
		doLog=F)
	
	if (length(tmpObjs) > 0) {
		guiDo(call=bquote(rm(list=.(tmpObjs))))
	}
	
	setStatusBar("Generated Voronoi mosaic plot")
}

.hs_on_rain_make_areal_button_clicked <- function(button) {
	StateEnv$win$setSensitive(F)
	on.exit(StateEnv$win$setSensitive(T))
	setStatusBar("")
	
	selNames <- iconViewGetSelectedNames(theWidget("selection_iconview"))
	if (length(selNames) == 0) {
		errorDialog("No items selected.")
		return()
	}
	if (length(selNames) < 4) {
		errorDialog("Need at least 4 items for spatial interpolation.")
		return()
	}
	nBlobs <- length(selNames)
	
	doByVoronoi <- theWidget("rain_area_byvoronoi_radiobutton")$getActive()
	doBySurface <- theWidget("rain_areal_bysurface_radiobutton")$getActive()
	
	#if (doBySurface) {
		errorDialog("Sorry, this does not work yet.")
		return()
	#}
	
	sapply(subAreasSP@polygons, function(x) x@area)
	
	
	grid1 <- readFltRaster("G:/Projects/Tuross/surface_files/rainfall_surface/year2000/rainGrid_20001.flt")
	
	overlay(as(grid1, "SpatialPointsDataFrame"), catchment, fn=mean)

}

