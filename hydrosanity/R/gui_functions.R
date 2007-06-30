## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL
##
## Some of these functions are adapted from Rattle v2.1 by Graham Williams.

theWidget <- function(name) {
	return(StateEnv$GUI$getWidget(name))
}

## Log support

addLogSeparator <- function() {
	addToLog("\n",
		"## ", paste(rep("=", 60), collapse=""), "\n",
		"## Timestamp: ", format(Sys.time()))
}

addLogComment <- function(...) {
	addToLog("\n## ", ...)
}

addToLog <- function(..., sep="", end.with="\n") {
	addTextview(theWidget("log_textview"), ..., end.with, sep=sep)
}

## PLOTANDPLAYGTK CALLBACK FUNCTIONS

hydrosanityButtons <- alist(
	setregion=list("Set region", "gtk-yes", f=.hs_map_setregion_event),
	setperiod=list("Set period", "gtk-yes", f=.hs_time_setperiod_event),
	zoomin=list("Zoom in", "gtk-zoom-in", f=.hs_time_zoomin_event),
	zoomout=list("Zoom out", "gtk-zoom-out", f=.hs_time_zoomout_event),
	centre=list("Re-centre", "gtk-jump-to-ltr", f=.hs_time_centre_event),
	logscale=list("Log scale", "gtk-goto-top", f=.hs_time_logscale_event, isToggle=T)
)

.hs_map_setregion_event <- function(widget, user.data) {
	name <- user.data$name
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar(name)$setSensitive(F)
	on.exit(plotAndPlayGetToolbar(name)$setSensitive(T))
	# get current scale setting
	myX <- eval(plotAndPlayGTK:::StateEnv[[name]]$call$xlim)
	myY <- eval(plotAndPlayGTK:::StateEnv[[name]]$call$ylim)
	addLogComment("Set region for analysis")
	if (is.null(myX) || is.null(myY)) {
		guiDo(hsp$region <- NULL)
		infoDialog("Set region for analysis to NULL ",
			"(i.e. include all sites)")
	} else {
		myX <- round(myX, digits=3)
		myY <- round(myY, digits=3)
		guiDo(call=bquote(hsp$region <- list(xlim=.(myX), ylim=.(myY))))
		infoDialog("Set region for analysis: X=",
			paste(myX, collapse=" to "), " Y=", paste(myY, collapse=" to "))
	}
	regionModificationUpdate()
	plotAndPlayGTK:::StateEnv[[name]]$win$present()
}

.hs_time_setperiod_event <- function(widget, user.data) {
	name <- user.data$name
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar(name)$setSensitive(F)
	on.exit(plotAndPlayGetToolbar(name)$setSensitive(T))
	# get current scale setting
	myXScale <- eval(plotAndPlayGTK:::StateEnv[[name]]$call$xscale)
	addLogComment("Set time period for analysis")
	if (is.null(myXScale)) {
		guiDo(hsp$timePeriod <- NULL)
		infoDialog("Set time period for analysis to NULL ",
			"(i.e. include all data)")
	} else {
		timelim <- as.POSIXct(myXScale)
		myTimeStrings <- format(round(timelim, "days"))
		guiDo(call=bquote(hsp$timePeriod <- as.POSIXct(.(myTimeStrings))))
		infoDialog("Set time period for analysis: ",
			paste(myTimeStrings, collapse=" to "))
	}
	timeperiodModificationUpdate()
	plotAndPlayGTK:::StateEnv[[name]]$win$present()
}

.hs_time_centre_event <- function(widget, user.data) {
	name <- user.data$name
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar(name)$setSensitive(F)
	on.exit(plotAndPlayGetToolbar(name)$setSensitive(T))
	# switch to this device
	oldDev <- dev.cur()
	dev.set(plotAndPlayGTK:::StateEnv[[name]]$dev)
	on.exit(dev.set(oldDev), add=T)
	# set up prompt
	plotAndPlayMakePrompt(name)
	on.exit(plotAndPlayUnmakePrompt(name), add=T)
	# get new scales interactively
	depth <- try(downViewport("time.vp"), silent=T)
	if (inherits(depth, "try-error")) {
		errorDialog("Viewport 'time.vp' not found")
		return()
	}
	xscale <- as.numeric(convertX(unit(0:1, "npc"), "native"))
	# get new centre point
	plotAndPlaySetPrompt(name, "Click to re-centre the plot")
	clickLoc <- grid.locator()
	if (is.null(clickLoc)) {
		upViewport(depth)
		return()
	}
	xscale.new <- as.numeric(clickLoc$x) + diff(xscale) * c(-0.5, 0.5)
	xscale.new <- as.POSIXct(xscale.new)
	# update state
	tmp.state <- plotAndPlayGTK:::StateEnv[[name]]
	tmp.state$call$xscale <- xscale.new
	assign(name, tmp.state, envir=plotAndPlayGTK:::StateEnv)
	plotAndPlayUpdate(name)
}

.hs_time_zoomin_event <- function(widget, user.data) {
	name <- user.data$name
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar(name)$setSensitive(F)
	on.exit(plotAndPlayGetToolbar(name)$setSensitive(T))
	# switch to this device
	oldDev <- dev.cur()
	dev.set(plotAndPlayGTK:::StateEnv[[name]]$dev)
	on.exit(dev.set(oldDev), add=T)
	# set up prompt
	plotAndPlayMakePrompt(name)
	on.exit(plotAndPlayUnmakePrompt(name), add=T)
	# set up masking
	maskGrob <- rectGrob(gp=gpar(col="grey", 
		fill=rgb(0.5,0.5,0.5, alpha=0.5)), name="tmp.mask")
	# get new scales interactively
	depth <- try(downViewport("time.vp"), silent=T)
	if (inherits(depth, "try-error")) {
		errorDialog("Viewport 'time.vp' not found")
		return()
	}
	xscale <- convertX(unit(0:1, "npc"), "native")
	# get start time
	plotAndPlaySetPrompt(name, "Click at the start of the window (to zoom in to)")
	clickLoc <- grid.locator()
	if (is.null(clickLoc)) {
		upViewport(depth)
		return()
	}
	xscale.new <- as.POSIXct.numeric(clickLoc$x)
	grid.draw(editGrob(maskGrob, x=unit(0,"npc"), 
		width=(clickLoc$x - xscale[1]), just="left"))
	# get end time
	plotAndPlaySetPrompt(name, "OK, now click at the end of the window")
	clickLoc <- grid.locator()
	if (is.null(clickLoc)) {
		grid.remove("tmp.mask", grep=T, global=T, strict=T)
		upViewport(depth)
		return()
	}
	xscale.new[2] <- as.POSIXct.numeric(clickLoc$x)
	grid.draw(editGrob(maskGrob, x=unit(1,"npc"),
		width=(xscale[2] - clickLoc$x), just="right"))
	# update state
	tmp.state <- plotAndPlayGTK:::StateEnv[[name]]
	tmp.state$call$xscale <- xscale.new
	assign(name, tmp.state, envir=plotAndPlayGTK:::StateEnv)
	plotAndPlayUpdate(name)
}

.hs_time_zoomout_event <- function(widget, user.data) {
	name <- user.data$name
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar(name)$setSensitive(F)
	on.exit(plotAndPlayGetToolbar(name)$setSensitive(T))
	# switch to this device
	oldDev <- dev.cur()
	dev.set(plotAndPlayGTK:::StateEnv[[name]]$dev)
	on.exit(dev.set(oldDev), add=T)
	# get new scales interactively
	depth <- try(downViewport("time.vp"), silent=T)
	if (inherits(depth, "try-error")) {
		errorDialog("Viewport 'time.vp' not found")
		return()
	}
	xscale <- as.numeric(convertX(unit(0:1, "npc"), "native"))
	xscale <- xscale + diff(xscale) * c(-0.5, 0.5)
	xscale <- as.POSIXct(xscale)
	# update state
	tmp.state <- plotAndPlayGTK:::StateEnv[[name]]
	tmp.state$call$xscale <- xscale
	assign(name, tmp.state, envir=plotAndPlayGTK:::StateEnv)
	plotAndPlayUpdate(name)
}

.hs_time_logscale_event <- function(widget, user.data) {
	name <- user.data$name
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar(name)$setSensitive(F)
	on.exit(plotAndPlayGetToolbar(name)$setSensitive(T))
	# get new log scale setting
	logScale <- widget$getActive()
	# update state
	tmp.state <- plotAndPlayGTK:::StateEnv[[name]]
	tmp.state$call$logScale <- logScale
	assign(name, tmp.state, envir=plotAndPlayGTK:::StateEnv)
	plotAndPlayUpdate(name)
}


## TREE VIEW AND ICON VIEW HELPERS

insertTreeViewTextColumns <- function(treeView, colNames=colnames(treeView$getModel()), editors=NULL, combo=NULL) {
	for (i in seq(along=colNames)) {
		renderer <- gtkCellRendererText()
		#renderer$set(xalign = 1.0) # right-align
		if (!is.null(combo[[ colNames[i] ]])) {
			renderer <- gtkCellRendererCombo()
			renderer$set(model = rGtkDataFrame(data.frame(combo[[ colNames[i] ]])), text_column = 0, has_entry = F)
		}
		if (!is.null(editors[[ colNames[i] ]])) {
			renderer$set(editable = T)
			gSignalConnect(renderer, "edited", editors[[ colNames[i] ]])
		}
		treeView$insertColumnWithAttributes(
			-1, gsub('_', ' ', colNames[i]), renderer, text=i-1)
	}
	invisible(NULL)
}

# note these indices are in the R convention (first element is #1)
treeViewGetSelectedIndices <- function(treeView) {
	selPaths <- treeView$getSelection()$getSelectedRows()$retval
	if (length(selPaths)==0) { return(NULL) }
	indices <- sapply(selPaths, function(x) x$getIndices()) + 1
}

setupIconView <- function(iconView, itemNames=names(hsp$data), selection=c("first", "all", "none")) {
	selection <- match.arg(selection)
	
	rainPixbuf <- gdkPixbufNewFromFile(getpackagefile("icon_RAIN.png"))$retval
	flowPixbuf <- gdkPixbufNewFromFile(getpackagefile("icon_FLOW.png"))$retval
	otherPixbuf <- gdkPixbufNewFromFile(getpackagefile("icon_OTHER.png"))$retval
	# (or NULL)
	
	list_store <- gtkListStore("character", "GdkPixbuf")
	
	for (x in itemNames) {
		myRole <- attr(hsp$data[[x]], "role")
		if (is.null(myRole)) { myRole <- "OTHER" }
		iter <- list_store$append()$iter
		list_store$set(iter, 0, x)
		list_store$set(iter, 1, switch(myRole,
			"RAIN"=rainPixbuf,
			"FLOW"=flowPixbuf,
			otherPixbuf)
		)
		myName <- attr(hsp$data[[x]], "sitename")
		list_store$set(iter, 2, myName)
	}
	selPaths <- NULL
	if (!is.null(iconView$getModel())) {
		selPaths <- iconView$getSelectedItems()
	}
	iconView$setModel(list_store)
	iconView$setTextColumn(2)
	iconView$setPixbufColumn(1)
	iconView$setItemWidth(75)
	if (is.null(selPaths)) {
		if (selection == "first") {
			iconView$selectPath(gtkTreePathNewFromString("0"))
		}
		if (selection == "all") {
			iconView$selectAll()
		}
	} else {
		for (p in selPaths) { iconView$selectPath(p) }
	}
	invisible(NULL)
}

iconViewGetSelectedNames <- function(iconView) {
	# it's not enough to get the item indices since user can re-order them!
	selPaths <- iconView$getSelectedItems()
	if (length(selPaths)==0) { return(NULL) }
	# these are returned in reverse order, strangely
	selPaths <- rev(selPaths)
	# get names of items
	# assuming that the names are in column 0 of the model
	dataListModel <- iconView$getModel()
	selNames <- sapply(selPaths, function(x) {
		dataListModel$getValue(dataListModel$getIter(x)$iter, 0)$value
	})
	return(selNames)
}


## Misc

setStatusBar <- function(..., sep="")
{
  msg <- paste(sep=sep, ...)
  if (length(msg) == 0) msg <-""
  theWidget("statusbar")$push(1, msg)
  while (gtkEventsPending()) gtkMainIteration() # Refresh status and windows
  invisible(NULL)
}

setCursor <- function(cursor=NULL) {
	if (!is.null(cursor)) { cursor <- gdkCursorNew(cursor) }
	StateEnv$win$getWindow()$setCursor(cursor)
}


