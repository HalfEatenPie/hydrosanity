## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL


## PLOT WINDOW FUNCTIONS

setPlotDevice <- function(name) {
	if (is.null(.hydrosanity$dev[[name]]) ||
		(.hydrosanity$dev[[name]] %notin% dev.list())) {
		if (require("cairoDevice", quietly=T)) {
			newCairoWindow(name)
		} else {
			#do.call(getOption("device"), list())
			trellis.device()
		}
		.hydrosanity$dev[[name]] <<- dev.cur()
	}
	dev.set(.hydrosanity$dev[[name]])
	if (!is.null(.hydrosanity$win[[name]])) {
		.hydrosanity$win[[name]]$present()
	}
}

newCairoWindow <- function(name) {
	plotGUI <- gladeXMLNew(getpackagefile("hydrosanity.glade"), 
		root="plot_window")
	
	gladeXMLSignalAutoconnect(plotGUI)
	myWin <- plotGUI$getWidget("plot_window")
	gSignalConnect(myWin, "delete-event", .hs_on_plot_delete_event)
	
	.hydrosanity$win.gui[[name]] <<- plotGUI
	.hydrosanity$win[[name]] <<- plotGUI$getWidget("plot_window")
	newDev <- plotGUI$getWidget("drawingarea")
	asCairoDevice(newDev)
	trellis.device(new=F)
	myWin$setTitle(paste("Hydrosanity: ", name, " plot", sep=''))
}

setCairoWindowButtons <- function(name, identify=NA, centre=NA, zoomin=NA, zoomout=zoomin, log=NA, setperiod=NA) {
	# check that the window exists
	if (is.null(.hydrosanity$win[[name]])) { return() }
	myGUI <- .hydrosanity$win.gui[[name]]
	# first set the call to NULL so that toggle-button updates can be ignored
	.hydrosanity$call[[name]] <<- NULL
	# set which buttons are visible
	if (!is.na(identify)) { myGUI$getWidget("plot_identify_button")$show() }
	if (!is.na(centre)) { myGUI$getWidget("plot_centre_button")$show() }
	if (!is.na(zoomin)) { myGUI$getWidget("plot_zoomin_button")$show() }
	if (!is.na(zoomout)) { myGUI$getWidget("plot_zoomout_button")$show() }
	if (!is.na(setperiod)) { myGUI$getWidget("plot_setperiod_button")$show() }
	if (!is.na(log)) {
		myGUI$getWidget("plot_log_togglebutton")$show()
		myGUI$getWidget("plot_log_togglebutton")$setActive(log)
	}
	saveMenu <- gtkMenu()
	saveItemPDF <- gtkMenuItem("PDF")
	saveItemPNG <- gtkMenuItem("PNG (bitmap)")
	saveItemPS <- gtkMenuItem("PostScript")
	saveItemSVG <- gtkMenuItem("SVG")
	saveMenu$append(saveItemPDF)
	saveMenu$append(saveItemPNG)
	saveMenu$append(saveItemPS)
	saveMenu$append(saveItemSVG)
	myGUI$getWidget("plot_save_button")$setMenu(saveMenu)
	myWin <- .hydrosanity$win[[name]]
	gSignalConnect(saveItemPDF, "activate", .hs_on_plot_save_button_clicked, 
		data=list(win=myWin, ext="pdf"))
	gSignalConnect(saveItemPNG, "activate", .hs_on_plot_save_button_clicked, 
		data=list(win=myWin, ext="png"))
	gSignalConnect(saveItemPS, "activate", .hs_on_plot_save_button_clicked, 
		data=list(win=myWin, ext="ps"))
	gSignalConnect(saveItemSVG, "activate", .hs_on_plot_save_button_clicked, 
		data=list(win=myWin, ext="svg"))
}

.hs_on_plot_delete_event <- function(widget, event, user.data) {
	myWin <- widget
	myIndex <- NA
	for (i in seq(along=.hydrosanity$win)) {
		if (myWin == .hydrosanity$win[[i]]) { myIndex <- i; break }
	}
	if (!is.na(myIndex)) {
		myName <- names(.hydrosanity$win)[myIndex]
		.hydrosanity$dev[[myName]] <<- NULL
		.hydrosanity$call[[myName]] <<- NULL
		.hydrosanity$id.call[[myName]] <<- NULL
		.hydrosanity$win[[myName]] <<- NULL
		.hydrosanity$win.gui[[myName]] <<- NULL
	}
	myWin$destroy()
	theWidget(APPWIN)$present()
}

.hs_on_plot_setperiod_button_clicked <- function(button) {
	# look up information about this window in '.hydrosanity'
	myWin <- button$getParent()$getParent()$getParent()
	myIndex <- NA
	for (i in seq(along=.hydrosanity$win)) {
		if (myWin == .hydrosanity$win[[i]]) { myIndex <- i; break }
	}
	if (is.na(myIndex)) { return() }
	myName <- names(.hydrosanity$win)[myIndex]
	myCall <- .hydrosanity$call[[myName]]
	myXScale <- eval(myCall$xscale)
	if (is.null(myXScale)) {
		guiDo(hsp$timePeriod <- NULL)
		infoDialog("Set time period for analysis to NULL ",
			"(i.e. include all data)", restore=F)
		timeperiodModificationUpdate()
		return()
	}
	timelim <- as.POSIXct(myXScale)
	myTimeStrings <- format(round(timelim, "days"))
	addLogComment("Set time period for analysis")
	guiDo(isExpression=T, bquote(
		hsp$timePeriod <- as.POSIXct(.(myTimeStrings))
	))
	infoDialog("Set time period for analysis: ",
		paste(myTimeStrings, collapse=" to "), restore=F)
	timeperiodModificationUpdate()
}

.hs_on_plot_identify_button_clicked <- function(button) {
	# look up information about this window in '.hydrosanity'
	myWin <- button$getParent()$getParent()$getParent()
	myIndex <- NA
	for (i in seq(along=.hydrosanity$win)) {
		if (myWin == .hydrosanity$win[[i]]) { myIndex <- i; break }
	}
	if (is.na(myIndex)) { return() }
	myName <- names(.hydrosanity$win)[myIndex]
	# disable other plot buttons until this is over
	myToolbar <- .hydrosanity$win.gui[[myName]]$getWidget("toolbar")
	myToolbar$setSensitive(F)
	on.exit(myToolbar$setSensitive(T))
	# switch to this device
	oldDev <- dev.cur()
	on.exit(dev.set(oldDev), add=T)
	dev.set(.hydrosanity$dev[[myName]])
	# do identify
	idCall <- .hydrosanity$id.call[[myName]]
	if (is.null(idCall)) {
		errorDialog("Do not know how to identify data points here.")
		return()
	}
	#infoDialog("Click on the plot (with left mouse button) to label data points. You <b>must</b> click on the plot with the <b>right mouse button</b> to finish. If you do not, the program will freeze.", restore=F)
	# set up prompts
	pad <- unit(2, "mm")
	promptGrob <- textGrob("", 
		y=unit(1,"npc")-pad, just="top",
		gp=gpar(fontface="bold"), name="tmp.prompt")
	promptBoxGrob <- rectGrob(
		height=grobHeight(promptGrob)+pad*2,
		y=unit(1,"npc"), just="top", 
		gp=gpar(col="black", fill="yellow"),
		name="tmp.promptbox")
	grid.draw(promptBoxGrob)
	grid.draw(editGrob(promptGrob, label="Identifying data points... Click the right mouse button to finish."))
	trellis.focus("panel", 1, 1)
	idCall$cex <- 0.6
	eval(idCall)
	grid.remove("tmp.*", grep=T, global=T, redraw=F)
	trellis.unfocus()
}

.hs_on_plot_centre_button_clicked <- function(button) {
	# look up information about this window in '.hydrosanity'
	myWin <- button$getParent()$getParent()$getParent()
	myIndex <- NA
	for (i in seq(along=.hydrosanity$win)) {
		if (myWin == .hydrosanity$win[[i]]) { myIndex <- i; break }
	}
	if (is.na(myIndex)) { return() }
	myName <- names(.hydrosanity$win)[myIndex]
	myCallName <- deparse(.hydrosanity$call[[myName]][[1]])
	# disable other plot buttons until this is over
	myToolbar <- .hydrosanity$win.gui[[myName]]$getWidget("toolbar")
	myToolbar$setSensitive(F)
	on.exit(myToolbar$setSensitive(T))
	# switch to this device
	oldDev <- dev.cur()
	on.exit(dev.set(oldDev), add=T)
	dev.set(.hydrosanity$dev[[myName]])
	# set up prompts
	pad <- unit(2, "mm")
	promptGrob <- textGrob("", 
		y=unit(1,"npc")-pad, just="top",
		gp=gpar(fontface="bold"), name="tmp.prompt")
	promptBoxGrob <- rectGrob(
		height=grobHeight(promptGrob)+pad*2,
		y=unit(1,"npc"), just="top", 
		gp=gpar(col="black", fill="yellow"),
		name="tmp.promptbox")
	# get new scales interactively
	if (myCallName %in% c("grid.timeseries.plot",
		"grid.timeseries.plot.superpose", "grid.timeline.plot")) {
		# x (time) scale only
		depth <- downViewport("time.vp")
		xscale <- as.numeric(convertX(unit(c(0,1), "npc"), "native"))
		plotVpp <- current.vpPath()
		upViewport(depth)
		# display prompt to user (in top-level viewport)
		grid.draw(promptBoxGrob)
		grid.draw(editGrob(promptGrob, label="Click to move the time window"))
		# get new centre point
		downViewport(plotVpp)
		clickLoc <- grid.locator()
		upViewport(depth)
		if (is.null(clickLoc)) {
			grid.remove("tmp.*", grep=T, global=T)
			return()
		}
		xscale.new <- as.numeric(clickLoc$x) + diff(xscale) * c(-0.5, 0.5)
		xscale.new <- as.POSIXct(xscale.new)
		.hydrosanity$call[[myName]]$xscale <<- xscale.new
	} else {
		errorDialog("Do not know how to interact with ", myCallName)
		return()
	}
	# re-draw plot
	tmp <- eval(.hydrosanity$call[[myName]])
	if (identical(class(tmp), "trellis")) { print(tmp) }
}

.hs_on_plot_zoomin_button_clicked <- function(button) {
	# look up information about this window in '.hydrosanity'
	myWin <- button$getParent()$getParent()$getParent()
	myIndex <- NA
	for (i in seq(along=.hydrosanity$win)) {
		if (myWin == .hydrosanity$win[[i]]) { myIndex <- i; break }
	}
	if (is.na(myIndex)) { return() }
	myName <- names(.hydrosanity$win)[myIndex]
	myCallName <- deparse(.hydrosanity$call[[myName]][[1]])
	# disable other plot buttons until this is over
	myToolbar <- .hydrosanity$win.gui[[myName]]$getWidget("toolbar")
	myToolbar$setSensitive(F)
	on.exit(myToolbar$setSensitive(T))
	# switch to this device
	oldDev <- dev.cur()
	on.exit(dev.set(oldDev), add=T)
	dev.set(.hydrosanity$dev[[myName]])
	# set up prompts
	pad <- unit(2, "mm")
	promptGrob <- textGrob("", 
		y=unit(1,"npc")-pad, just="top",
		gp=gpar(fontface="bold"), name="tmp.prompt")
	promptBoxGrob <- rectGrob(
		height=grobHeight(promptGrob)+pad*2,
		y=unit(1,"npc"), just="top", 
		gp=gpar(col="black", fill="yellow"),
		name="tmp.promptbox")
	maskGrob <- rectGrob(
		gp=gpar(col="grey", fill=rgb(0.5,0.5,0.5, alpha=0.5)), 
		name="tmp.mask")
	# get new scales interactively
	if (myCallName %in% c("grid.timeseries.plot",
		"grid.timeseries.plot.superpose", "grid.timeline.plot")) {
		# x (time) scale only
		depth <- downViewport("time.vp")
		xscale <- convertX(unit(c(0,1), "npc"), "native")
		plotVpp <- current.vpPath()
		upViewport(depth)
		# display prompt to user (in top-level viewport)
		grid.draw(promptBoxGrob)
		grid.draw(editGrob(promptGrob, label="Click at the start of the window (to zoom in to) "))
		# get start time
		downViewport(plotVpp)
		clickLoc <- grid.locator()
		upViewport(depth)
		if (is.null(clickLoc)) {
			grid.remove("tmp.*", grep=T, global=T)
			return()
		}
		xscale.new <- as.POSIXct.numeric(clickLoc$x)
		grid.draw(editGrob(maskGrob, x=unit(0,"npc"), 
			width=(clickLoc$x - xscale[1]), just="left", vp=plotVpp))
		# display second prompt
		grid.draw(promptBoxGrob)
		grid.draw(editGrob(promptGrob, label="OK, now click at the end of the window"))
		# get end time
		downViewport(plotVpp)
		clickLoc <- grid.locator()
		upViewport(depth)
		if (is.null(clickLoc)) {
			grid.remove("tmp.*", grep=T, global=T)
			return()
		}
		xscale.new[2] <- as.POSIXct.numeric(clickLoc$x)
		grid.draw(editGrob(maskGrob, x=unit(1,"npc"),
			width=(xscale[2] - clickLoc$x), just="right", vp=plotVpp))
		.hydrosanity$call[[myName]]$xscale <<- xscale.new
	} else
	if (myCallName %in% c("xyplot", "bwplot", "stripplot", "qqmath", "qq", "levelplot")) {
		# x and y scales
		depth <- downViewport(trellis.vpname("panel", 1, 1))
		xscale <- convertX(unit(c(0,1), "npc"), "native")
		yscale <- convertY(unit(c(0,1), "npc"), "native")
		plotVpp <- current.vpPath()
		upViewport(depth)
		# display prompt to user (in top-level viewport)
		grid.draw(promptBoxGrob)
		grid.draw(editGrob(promptGrob, label="Click at the bottom-left corner of the region to zoom in to "))
		# get lower limits
		downViewport(plotVpp)
		clickLoc <- grid.locator()
		upViewport(depth)
		if (is.null(clickLoc)) {
			grid.remove("tmp.*", grep=T, global=T)
			return()
		}
		xscale.new <- clickLoc$x
		yscale.new <- clickLoc$y
		grid.draw(editGrob(maskGrob, 
			x=unit(0,"npc"), width=(xscale.new[1] - xscale[1]), 
			just="left", vp=plotVpp))
		grid.draw(editGrob(maskGrob, 
			y=unit(0,"npc"), height=(yscale.new[1] - yscale[1]),
			x=unit(1,"npc"), width=(xscale[2] - xscale.new[1]),
			just=c("right", "bottom"), vp=plotVpp))
		# display second prompt
		grid.draw(promptBoxGrob)
		grid.draw(editGrob(promptGrob, label="OK, now click at the top-right corner"))
		# get upper limits
		downViewport(plotVpp)
		clickLoc <- grid.locator()
		upViewport(depth)
		if (is.null(clickLoc)) {
			grid.remove("tmp.*", grep=T, global=T)
			return()
		}
		xscale.new[2] <- clickLoc$x
		yscale.new[2] <- clickLoc$y
		grid.draw(editGrob(maskGrob, 
			x=unit(1,"npc"), width=(xscale[2] - xscale.new[2]), 
			y=unit(1,"npc"), height=(yscale[2] - yscale.new[1]),
			just=c("right", "top"), vp=plotVpp))
		grid.draw(editGrob(maskGrob, 
			y=unit(1,"npc"), height=(yscale[2] - yscale.new[2]),
			x=xscale.new[2], width=(xscale.new[2] - xscale.new[1]),
			just=c("right", "top"), vp=plotVpp))
		# convert back from log scale if required
		xscale <- as.numeric(xscale.new)
		yscale <- as.numeric(yscale.new)
		myScalesArg <- .hydrosanity$call[[myName]]$scales
		if (!is.null(myScalesArg$log) && myScalesArg$log) {
			xscale <- 10 ^ xscale
			yscale <- 10 ^ yscale
		} else {
			if (!is.null(myScalesArg$x$log) && myScalesArg$x$log) {
				xscale <- 10 ^ xscale
			}
			if (!is.null(myScalesArg$y$log) && myScalesArg$y$log) {
				yscale <- 10 ^ yscale
			}
		}
		.hydrosanity$call[[myName]]$xlim <<- xscale
		.hydrosanity$call[[myName]]$ylim <<- yscale
	} else {
		errorDialog("Do not know how to interact with ", myCallName)
		return()
	}
	# re-draw plot
	tmp <- eval(.hydrosanity$call[[myName]])
	if (identical(class(tmp), "trellis")) { print(tmp) }
}

.hs_on_plot_zoomout_button_clicked <- function(button) {
	# look up information about this window in '.hydrosanity'
	myWin <- button$getParent()$getParent()$getParent()
	myIndex <- NA
	for (i in seq(along=.hydrosanity$win)) {
		if (myWin == .hydrosanity$win[[i]]) { myIndex <- i; break }
	}
	if (is.na(myIndex)) { return() }
	myName <- names(.hydrosanity$win)[myIndex]
	myCallName <- deparse(.hydrosanity$call[[myName]][[1]])
	# disable other plot buttons until this is over
	myToolbar <- .hydrosanity$win.gui[[myName]]$getWidget("toolbar")
	myToolbar$setSensitive(F)
	on.exit(myToolbar$setSensitive(T))
	# switch to this device
	oldDev <- dev.cur()
	on.exit(dev.set(oldDev), add=T)
	dev.set(.hydrosanity$dev[[myName]])
	# find existing scales and update call
	if (myCallName %in% c("grid.timeseries.plot",
		"grid.timeseries.plot.superpose", "grid.timeline.plot")) {
		# x (time) scale only
		depth <- downViewport("time.vp")
		xscale <- as.numeric(convertX(unit(c(0,1), "npc"), "native"))
		upViewport(depth)
		xscale <- xscale + diff(xscale) * c(-0.5, 0.5)
		xscale <- as.POSIXct(xscale)
		.hydrosanity$call[[myName]]$xscale <<- xscale
	} else
	if (myCallName %in% c("xyplot", "bwplot", "stripplot", "qqmath", "qq", "levelplot")) {
		# x and y scales
		depth <- downViewport(trellis.vpname("panel", 1, 1))
		xscale <- as.numeric(convertX(unit(c(0,1), "npc"), "native"))
		yscale <- as.numeric(convertY(unit(c(0,1), "npc"), "native"))
		upViewport(depth)
		xscale <- xscale + diff(xscale) * c(-0.5, 0.5)
		yscale <- yscale + diff(yscale) * c(-0.5, 0.5)
		# convert back from log scale if required
		myScalesArg <- .hydrosanity$call[[myName]]$scales
		if (!is.null(myScalesArg$log) && myScalesArg$log) {
			xscale <- 10 ^ xscale
			yscale <- 10 ^ yscale
		} else {
			if (!is.null(myScalesArg$x$log) && myScalesArg$x$log) {
				xscale <- 10 ^ xscale
			}
			if (!is.null(myScalesArg$y$log) && myScalesArg$y$log) {
				yscale <- 10 ^ yscale
			}
		}
		.hydrosanity$call[[myName]]$xlim <<- xscale
		.hydrosanity$call[[myName]]$ylim <<- yscale
	} else {
		errorDialog("Do not know how to interact with ", myCallName)
	}
	# re-draw plot
	tmp <- eval(.hydrosanity$call[[myName]])
	if (identical(class(tmp), "trellis")) { print(tmp) }
}


.hs_on_plot_log_togglebutton_clicked <- function(button) {
	# look up information about this window in '.hydrosanity'
	myWin <- button$getParent()$getParent()$getParent()
	myIndex <- NA
	for (i in seq(along=.hydrosanity$win)) {
		if (myWin == .hydrosanity$win[[i]]) { myIndex <- i; break }
	}
	if (is.na(myIndex)) { return() }
	myName <- names(.hydrosanity$win)[myIndex]
	myCallName <- deparse(.hydrosanity$call[[myName]][[1]])
	# disable other plot buttons until this is over
	myToolbar <- .hydrosanity$win.gui[[myName]]$getWidget("toolbar")
	myToolbar$setSensitive(F)
	on.exit(myToolbar$setSensitive(T))
	# get new log setting
	logScale <- button$getActive()
	# switch to this device
	oldDev <- dev.cur()
	on.exit(dev.set(oldDev), add=T)
	dev.set(.hydrosanity$dev[[myName]])
	# update the call and re-draw plot
	if (myCallName %in% c("xyplot", "qq")) {
		# lattice plots, log applies to both x and y scales
		.hydrosanity$call[[myName]]$scales$log <<- logScale
	} else
	if (myCallName %in% c("bwplot", "stripplot", "dotplot", "qqmath")) {
		# lattice plots, log applies to y scale only
		.hydrosanity$call[[myName]]$scales$y$log <<- logScale
	} else
	if (myCallName %in% c("grid.timeseries.plot", "grid.timeseries.plot.superpose")) {
		.hydrosanity$call[[myName]]$logScale <<- logScale
	}
	tmp <- eval(.hydrosanity$call[[myName]])
	if (identical(class(tmp), "trellis")) { print(tmp) }
}

.hs_on_plot_save_button_clicked <- function(button, data=NULL) {
	# look up information about this window in '.hydrosanity'
	myWin <- button$getParent()$getParent()$getParent()
	if (!is.null(data)) { myWin <- data$win }
	myIndex <- NA
	for (i in seq(along=.hydrosanity$win)) {
		if (myWin == .hydrosanity$win[[i]]) { myIndex <- i; break }
	}
	if (is.na(myIndex)) { return() }
	myName <- names(.hydrosanity$win)[myIndex]
	# disable other plot buttons until this is over
	myToolbar <- .hydrosanity$win.gui[[myName]]$getWidget("toolbar")
	myToolbar$setSensitive(F)
	on.exit(myToolbar$setSensitive(T))
	# get filename
	myExt <- if (!is.null(data)) { data$ext } else { 'pdf' }
	myDefault <- paste(myName, '.', myExt, sep='')
	filename <- choose.file.save(myDefault, caption="Save plot (pdf/png/ps/svg)", 
		filters=Filters[c("pdf","png","ps","All"),],
		index=match(myExt, c("pdf","png","ps","svg"))
	)
	myWin$present()
	if (is.na(filename)) { return() }
	if (get.extension(filename) == "") {
		filename <- sprintf("%s.pdf", filename)
	}
	# switch to this device
	oldDev <- dev.cur()
	on.exit(dev.set(oldDev), add=T)
	dev.set(.hydrosanity$dev[[myName]])
	# save plot to file
	mySize <- .hydrosanity$win.gui[[myName]]$getWidget("drawingarea")$getAllocation()
	myWidth <- mySize$width
	myHeight <- mySize$height
	#myScale <- min(7/myWidth, 7/myHeight)
	ext <- get.extension(filename)
#	if (ext %in% c("png")) {
		myScale <- 1/72
#	}
	if (ext %in% "pdf") {
		dev.copy(pdf, file=filename, width=myWidth*myScale, height=myHeight*myScale)
		dev.off()
	}
	else if (ext %in% c("ps", "eps")) {
		dev.copy(postscript, file=filename, width=myWidth*myScale, height=myHeight*myScale)
		dev.off()
	}
	else if (ext %in% "png") {
		#devFn <- if (require(Cairo, quietly=T)) { CairoPNG } else { png }
		#dev.copy(devFn, file=filename, width=myWidth, height=myHeight)
		dev.copy(Cairo_png, file=filename, width=myWidth*myScale, height=myHeight*myScale)
		dev.off()
	}
	else if (ext %in% "svg") {
		dev.copy(Cairo_svg, file=filename, width=myWidth*myScale, height=myHeight*myScale)
		dev.off()
	}
	else {
		errorDialog("Unrecognised filename extension", restore=F)
		return()
	}
	myWin$present()
	setStatusBar("Saved plot to ", filename)
}

.hs_on_plot_greyscale_togglebutton_clicked <- function(button) {
	# look up information about this window in '.hydrosanity'
	myWin <- button$getParent()$getParent()$getParent()
	myIndex <- NA
	for (i in seq(along=.hydrosanity$win)) {
		if (myWin == .hydrosanity$win[[i]]) { myIndex <- i; break }
	}
	if (is.na(myIndex)) { return() }
	myName <- names(.hydrosanity$win)[myIndex]
	# disable other plot buttons until this is over
	myToolbar <- .hydrosanity$win.gui[[myName]]$getWidget("toolbar")
	myToolbar$setSensitive(F)
	on.exit(myToolbar$setSensitive(T))
	# get new greyscale setting
	greyscale <- button$getActive()
	# switch to this device
	oldDev <- dev.cur()
	on.exit(dev.set(oldDev), add=T)
	dev.set(.hydrosanity$dev[[myName]])
	# make change and re-draw plot
	if (greyscale) {
		trellis.device(new=F, color=F)
	} else {
		trellis.device(new=F)
	}
	tmp <- eval(.hydrosanity$call[[myName]])
	if (identical(class(tmp), "trellis")) { print(tmp) }
}


## TREE VIEW AND ICON VIEW HELPERS

insertTreeViewTextColumns <- function(treeView, colNames=colnames(treeView$getModel()), editors=NULL, combo=NULL) {
	for (i in seq(along=colNames)) {
		renderer <- gtkCellRendererText()
		renderer$set(xalign = 1.0)
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
	
	for (i in itemNames) {
		myRole <- attr(hsp$data[[i]], "role")
		if (is.null(myRole)) { myRole <- "OTHER" }
		iter <- list_store$append()$iter
		list_store$set(iter, 0, i)
		list_store$set(iter, 1, switch(myRole,
			"RAIN"=rainPixbuf,
			"FLOW"=flowPixbuf,
			otherPixbuf)
		)
	}
	selPaths <- NULL
	if (!is.null(iconView$getModel())) {
		selPaths <- iconView$getSelectedItems()
	}
	iconView$setModel(list_store)
	iconView$setTextColumn(0)
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


## ERROR CATCHING STUFF

guiDo <- function(expr, isExpression=F, isParseString=F, doLog=T, doFailureDialog=T, doFailureLog=doLog, doStop=T, envir=if (doLog) {.GlobalEnv} else {parent.frame()}) {
	setCursor("watch")
	result <- NULL
	if (!isExpression && !isParseString) {
		expr <- substitute(expr)
	}
	if (isParseString) {
		result <- tryCatch(eval(parse(text=expr), envir=envir), 
			error=function(e)e)
	} else {
		result <- tryCatch(eval(expr, envir=envir), 
			error=function(e)e)
	}
	setCursor()
	if (doLog) {
		exprOK <- expr
		if (isParseString) {
			exprOK <- try(parse(text=expr)[[1]])
		}
		if (isParseString && inherits(exprOK, "try-error")) {
			# syntax error
			addToLog(expr)
		} else {
			exprPretty <- paste(capture.output(
				# if the code is in a simple block, omit braces
				if (identical(exprOK[[1]], as.symbol("{"))) {
					for (i in 2:length(exprOK)) {
						print(exprOK[[i]])
					}
				} else {
					print(exprOK)
				}
			), collapse="\n")
			addToLog(exprPretty)
		}
	}
	if (inherits(result, "error") && doFailureDialog) {
		setStatusBar("")
		commandText <- if (isParseString) { expr } else { deparse(expr) }
		msgText <- conditionMessage(result)
		callText <- deparse(conditionCall(result), width.cutoff=500)[1]
		if (length(msgText)==0) { msgText <- "" }
		if (length(callText)==0) { callText <- "" }
		vRSimple <- paste(R.version$major, R.version$minor, sep='.')
		errorDialog(paste(sep='',
	"A command has failed. The error was: \n\n<span foreground=\"#aa0000\">", msgText,
	"</span>\n\n", "The error occurred in: \n\n<tt>", callText, "</tt>\n\n", 
	"The original command was: \n\n<tt>", commandText, "</tt>\n\n",
	"If this is not your fault, you might want to select this text and copy it into a bug report.",
	" [HS=", VERSION, ";R=", vRSimple, "]"))
	}
	if (inherits(result, "error") && doFailureLog) {
		addToLog("# FAILED")
	}
	# propagate the error
	if (inherits(result, "error") && doStop) {
		stop(result)
	}
	return(result)
}


## COMMON DIALOGS

errorDialog <- function(...) {
	guiMessageDialog(type="error", ...)
}

infoDialog <- function(...) {
	guiMessageDialog(type="info", ...)
}

questionDialog <- function(...) {
	guiMessageDialog(type="question", ...)
}

guiMessageDialog <- function(type="info", ..., isMarkup=F, restore=T) {
	myString <- paste(sep='', ...)
	if (!isMarkup) {
		myString <- gsub('%', '%%', myString)
		myString <- gsub('&&', '&amp;&amp;', myString)
		myString <- gsub('& ', '&amp; ', myString)
		myString <- gsub('<<', '&lt;&lt;', myString)
		myString <- gsub('<-', '&lt;-', myString)
		myString <- gsub('< ', '&lt; ', myString)
	}
	myButtons <- switch(type,
		error="close",
		info="ok",
		question="yes-no"
	)
	dialog <- gtkMessageDialogNewWithMarkup(theWidget(APPWIN), 
		"destroy-with-parent", type, myButtons, myString)
	result <- dialog$run() # make it modal
	dialog$destroy()
	if (restore) { theWidget(APPWIN)$present() }
	return(if (result == GtkResponseType["yes"]) { "yes" } else { NULL })
}

guiTextInput <- function(text="", title="Text Input", prompt="") {
	# construct dialog
	editBox <- gtkDialog(title=title, NULL, "destroy-with-parent",
		"OK", GtkResponseType["ok"], "Cancel", GtkResponseType["cancel"],
		show = F)
	editBox$setDefaultResponse(GtkResponseType["ok"])
	editBox$setDefaultSize(400,300)
	if (nchar(prompt) > 0) {
		editBox[["vbox"]]$packStart(gtkLabel(prompt), expand=F, pad=2)
	}
	editTV <- gtkTextView()
	setTextviewMonospace(editTV)
	setTextview(editTV, text)
	editBox[["vbox"]]$add(editTV)
	result <- editBox$run() # make it modal
	newTxt <- getTextviewText(editTV)
	editBox$destroy()
	if (result != GtkResponseType["ok"]) { return(invisible(NULL)) }
	newTxt
}

## EDIT DATA FRAMES AS TEXT

editAsText <- function(x, title=NULL, edit.row.names=any(row.names(x) != 1:nrow(x))) {
	if (!is.data.frame(x)) { stop("'x' must be a data frame") }
	if (is.null(title)) {
		title <- paste("Editing", deparse(substitute(x)))
	}
	# make table text block from data frame 'x'
	foo <- capture.output(
		write.table(x, sep="\t", quote=F, row.names=edit.row.names,
		col.names=if (edit.row.names) {NA} else {T})
	)
	tableTxt <- paste(paste(foo, collapse="\n"), "\n", sep='')
	if (edit.row.names) { tableTxt <- paste("row.names", tableTxt, sep='') }
	# show text box and repeat if there was an error
	readOK <- F
	while (!readOK) {
		newTableTxt <- guiTextInput(text=tableTxt, title=title, 
			prompt=paste("Copy and paste to/from a spreadsheet,",
				"or edit the text here (in tab-separated format).\n",
				"Do not move the columns around,",
				"they must stay in this order."))
		if (is.null(newTableTxt)) { return(x) }
		# convert table text block back to data frame
		zz <- textConnection(newTableTxt)
		newData <- tryCatch(
			read.delim(file=zz, header=T, colClasses=sapply(x, class),
			row.names=if (edit.row.names) {1} else {NULL}),
			error=function(e)e)
		close(zz)
		# check whether there was an error in reading the table
		if (inherits(newData, "error")) {
			errorDialog("Error reading table: ", conditionMessage(newData))
			tableTxt <- newTableTxt
		} else {
			readOK <- T
		}
	}
	# warn if the number of rows has changed
	if (nrow(newData) != nrow(x)) {
		warning("Number of rows changed from ",
			nrow(x), " to ", nrow(newData))
	} else if (!edit.row.names) {
		# keep original row names 
		row.names(newData) <- attr(x, "row.names")
	}
	# ensure factor levels are the same
	for (i in which(sapply(x, class) == "factor")) {
		newData[[i]] <- factor(newData[[i]], levels=levels(x[[i]]))
	}
	newData
}

## SAVE FILE DIALOG

# returns character string, or NA if cancelled
choose.file.save <- function(default="", caption="Save File", filters=Filters[c("All"),], index=0) {
	dialog <- gtkFileChooserDialog(caption, NULL, "save",
		"gtk-cancel", GtkResponseType["cancel"],
		"gtk-save", GtkResponseType["accept"])
	dialog$setCurrentName(default)
	
	if (length(filters)==2) {
		filters <- matrix(filters, nrow=1, ncol=2)
	}
	
	for (i in seq(1, nrow(filters))) {
		ff <- gtkFileFilterNew()
		ff$setName(filters[i,1])
		for (x in strsplit(filters[i,2], ';')[[1]]) {
			ff$addPattern(x)
		}
		dialog$addFilter(ff)
		if (i == index) { dialog$setFilter(ff) }
	}
	
	#dialog$setDoOverwriteConfirmation(T) crap, appears behind filechooser
	if (dialog$run() == GtkResponseType["accept"]) {
		filename <- dialog$getFilename()
		if (file.exists(filename)) {
			if (is.null(questionDialog("Replace existing file?"))) {
				filename <- NA
			}
		}
		dialog$destroy()
		return(filename)
	} else {
		dialog$destroy()
		return(NA)
	}
}

