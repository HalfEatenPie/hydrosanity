## Hydrosanity: an interface for exploring hydrological time series in R
##
## Time-stamp: <2007-03-05 00:00:00 Felix>
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL


## PLOT WINDOW FUNCTIONS

setPlotDevice <- function(name) {
	if (is.null(.hydrosanity$dev[[name]]) ||
		(.hydrosanity$dev[[name]] %notin% dev.list())) {
		if (require("cairoDevice", quietly=TRUE)) {
			newCairoWindow(name)
		} else {
			do.call(getOption("device"), list())
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
	myWin$setTitle(paste("Hydrosanity: ", name, " plot", sep=''))
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
		.hydrosanity$win[[myName]] <<- NULL
		.hydrosanity$win.gui[[myName]] <<- NULL
	}
	myWin$destroy()
	theWidget("hs_window")$present()
}

.hs_on_plot_identify_button_clicked <- function(button) {
	infoDialog("not implemented")
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
	if (is.null(.hydrosanity$call[[myName]])) {
		errorDialog("No call for this window.")
		return()
	}
	depth <- try(downViewport("time.vp"), silent=T)
	if (inherits(depth, "try-error")) {
		errorDialog("No suitable time scales were found.")
		return()
	}
	xscale <- as.numeric(convertX(unit(c(0,1), "npc"), "native"))
	upViewport(depth)
	# display prompt to user (in top-level plot)
	pad <- unit(2, "mm")
	promptGrob <- textGrob("Click to move the time window", 
		y=unit(1,"npc")-pad, just="top",
		gp=gpar(fontface="bold"), name="tmp.prompt")
	promptBoxGrob <- rectGrob(width=grobWidth(promptGrob)+pad*2, 
		height=grobHeight(promptGrob)+pad*2,
		y=unit(1,"npc"), just="top", 
		gp=gpar(col="black", fill="yellow"),
		name="tmp.promptbox")
	grid.draw(promptBoxGrob)
	grid.draw(promptGrob)
	# get new centre point
	downViewport("time.vp")
	clickLoc <- grid.locator()
	upViewport(depth)
	if (is.null(clickLoc)) {
		return()
	}
	xscale.new <- as.numeric(clickLoc$x) + diff(xscale) * c(-0.5, 0.5)
	xscale.new <- as.POSIXct(xscale.new)
	# update the call (using do.call to force eval of args) and re-draw plot
	.hydrosanity$call[[myName]] <<- do.call(update, list(
		list(call=.hydrosanity$call[[myName]]), 
		xscale=xscale.new,
		evaluate=F)
	)
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
	if (is.null(.hydrosanity$call[[myName]])) {
		errorDialog("No call for this window.")
		return()
	}
	depth <- try(downViewport("time.vp"), silent=T)
	if (inherits(depth, "try-error")) {
		errorDialog("No suitable time scales were found.")
		return()
	}
	xscale <- convertX(unit(c(0,1), "npc"), "native")
	timeVpPath <- current.vpPath()
	upViewport(depth)
	# display prompt to user (in top-level plot)
	pad <- unit(2, "mm")
	promptGrob <- textGrob("Click at the start of the window (to zoom in to) ", 
		y=unit(1,"npc")-pad, just="top",
		gp=gpar(fontface="bold"), name="tmp.prompt")
	promptBoxGrob <- rectGrob(width=grobWidth(promptGrob)+pad*2, 
		height=grobHeight(promptGrob)+pad*2,
		y=unit(1,"npc"), just="top", 
		gp=gpar(col="black", fill="yellow"),
		name="tmp.promptbox")
	grid.draw(promptBoxGrob)
	grid.draw(promptGrob)
	# get start time
	downViewport("time.vp")
	clickLoc <- grid.locator()
	upViewport(depth)
	if (is.null(clickLoc)) {
		grid.remove("tmp.*", grep=T, global=T)
		return()
	}
	xscale.new <- as.POSIXct.numeric(clickLoc$x)
	maskGrob <- rectGrob(x=0, width=(clickLoc$x - xscale[1]), just="left", 
		gp=gpar(col=NULL, fill="grey", alpha=0.5), 
		vp=timeVpPath, name="tmp.mask")
	grid.draw(maskGrob)
	grid.lines(x=clickLoc$x, vp=timeVpPath, name="tmp.maskline")
	# display second prompt
	promptGrob <- editGrob(promptGrob, label="OK, now click at the end of the window")
	promptBoxGrob <- editGrob(promptBoxGrob, gp=gpar(alpha=1))
	grid.draw(promptBoxGrob)
	grid.draw(promptGrob)
	# get end time
	downViewport("time.vp")
	clickLoc <- grid.locator()
	upViewport(depth)
	if (is.null(clickLoc)) {
		grid.remove("tmp.*", grep=T, global=T)
		return()
	}
	xscale.new[2] <- as.POSIXct.numeric(clickLoc$x)
	grid.draw(editGrob(maskGrob, x=unit(1,"npc"),
		width=(xscale[2] - clickLoc$x), just="right"))
	grid.lines(x=clickLoc$x, vp=timeVpPath, name="tmp.maskline2")
	# update the call (using do.call to force eval of args) and re-draw plot
	.hydrosanity$call[[myName]] <<- do.call(update, list(
		list(call=.hydrosanity$call[[myName]]), 
		xscale=force(xscale.new),
		evaluate=F)
	)
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
	if (is.null(.hydrosanity$call[[myName]])) {
		errorDialog("No call for this window.")
		return()
	}
	depth <- try(downViewport("time.vp"), silent=T)
	if (inherits(depth, "try-error")) {
		errorDialog("No suitable time scales were found.")
		return()
	}
	xscale <- as.numeric(convertX(unit(c(0,1), "npc"), "native"))
	upViewport(depth)
	# make new xscale twice length of existing time period
	xscale.new <- xscale + diff(xscale) * c(-0.5, 0.5)
	xscale.new <- as.POSIXct(xscale.new)
	# update the call (using do.call to force eval of args) and re-draw plot
	.hydrosanity$call[[myName]] <<- do.call(update, list(
		list(call=.hydrosanity$call[[myName]]), 
		xscale=xscale.new,
		evaluate=F)
	)
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
	if (myName %notin% names(.hydrosanity$call)) {
		return()
	}
	myCallName <- deparse(.hydrosanity$call[[myName]][[1]])
	if (myCallName == "grid.timeline.plot") { return() }
	# get new value of log argument
	logScale <- button$getActive()
	# update the call (using do.call to force eval of args) and re-draw plot
	if (myCallName %in% c("xyplot", "bwplot", "stripplot", "qqmath", "qq")) {
		# lattice plots
		myScalesArg <- eval(.hydrosanity$call[[myName]]$scales)
		myScalesArg$y$log <- logScale
		.hydrosanity$call[[myName]] <<- do.call(update, list(
			list(call=.hydrosanity$call[[myName]]), 
			scales=myScalesArg,
			evaluate=F)
		)
	}
	if (myCallName %in% c("grid.timeseries.plot", "grid.timeseries.plot.superpose")) {
		.hydrosanity$call[[myName]] <<- do.call(update, list(
			list(call=.hydrosanity$call[[myName]]), 
			logScale=logScale,
			evaluate=F)
		)
	}
	tmp <- eval(.hydrosanity$call[[myName]])
	if (identical(class(tmp), "trellis")) { print(tmp) }
}

.hs_on_plot_save_button_clicked <- function(button) {
	# look up information about this window in '.hydrosanity'
	myWin <- button$getParent()$getParent()$getParent()
	myIndex <- NA
	for (i in seq(along=.hydrosanity$win)) {
		if (myWin == .hydrosanity$win[[i]]) { myIndex <- i; break }
	}
	if (is.na(myIndex)) { return() }
	myName <- names(.hydrosanity$win)[myIndex]
	
	myDefault <- paste(myName, '.pdf', sep='')
	filename <- choose.file.save(myDefault, caption="Save plot (pdf/ps/png/jpg)", 
		filters=Filters[c("pdf","ps","png","jpeg"),])
	myWin$present()
	if (is.na(filename)) { return() }
	
	if (get.extension(filename) == "") {
		filename <- sprintf("%s.pdf", filename)
	}
	
	mySize <- .hydrosanity$win.gui[[myName]]$getWidget("drawingarea")$getAllocation()
	myWidth <- mySize$width
	myHeight <- mySize$height
	myScale <- min(7/myWidth, 7/myHeight)
	
	ext <- get.extension(filename)
	if (ext %in% "pdf") {
		pdf(filename, width=myWidth*myScale, height=myHeight*myScale)
	}
	else if (ext %in% c("ps", "eps")) {
		ps(filename, width=myWidth*myScale, height=myHeight*myScale)
	}
	else if (ext %in% "png") {
		#oldDev <- dev.cur()
		#dev.set(.hydrosanity$dev[[myName]])
		#dev.copy(png, filename, width=myWidth*2, height=myHeight*2, pointsize=12*2, res=144)
		png(filename, width=myWidth, height=myHeight)
		#dev.set(oldDev)
	}
	else if (ext %in% c("jpeg", "jpg")) {
		jpeg(filename, width=myWidth, height=myHeight, quality=95)
	}
	else {
		errorDialog("Unrecognised filename extension")
		return()
	}
	# draw plot from scratch on this device
	tmp <- eval(.hydrosanity$call[[myName]])
	if (identical(class(tmp), "trellis")) { print(tmp) }
	dev.off()
	
	myWin$present()
	setStatusBar("Saved plot to ", filename)
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
}

# note these indices are in the R convention (first element is #1)
treeViewGetSelectedIndices <- function(treeView) {
	selPaths <- treeView$getSelection()$getSelectedRows()$retval
	if (length(selPaths)==0) { return(NULL) }
	indices <- sapply(selPaths, function(x) x$getIndices()) + 1
}

# it's not enough to get the item indices since user can re-order them!
iconViewGetSelectedNames <- function(iconView) {
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

guiTryEval <- function(command, doLog=T, doFailureDialog=T, doFailureLog=doLog, doParse=T) {
	if (length(command)==0) { command <- "" }
	setCursor("watch")
	result <- NULL
	if (doParse) {
		result <- tryCatch(eval(parse(text=command)), error=function(e)e)
	} else {
		result <- tryCatch(eval(command), error=function(e)e)
	}
	setCursor()
	if (doLog) {
		commandExpr <- command
		if (doParse) {
			commandExpr <- try(parse(text=command))
		}
		if (inherits(commandExpr, "try-error")) {
			# syntax error
			addToLog(command)
		} else {
			commandPretty <- paste(capture.output(
				lapply(commandExpr, print)
			), collapse="\n")
			addToLog(commandPretty)
		}
	}
	if (inherits(result, "error") && doFailureDialog) {
		setStatusBar("")
		commandText <- command
		if (!doParse) { commandText <- deparse(command) }
		msgText <- conditionMessage(result)
		callText <- deparse(conditionCall(result), width.cutoff=500)[1]
		if (length(msgText)==0) { msgText <- "" }
		if (length(callText)==0) { callText <- "" }
		errorDialog(paste(sep='',
	"A command has failed. The error was: \n\n<span foreground=\"#aa0000\">", msgText,
	"</span>\n\n", "The error occurred in: \n\n<tt>", callText, "</tt>\n\n", 
	"The original command was: \n\n<tt>", commandText, "</tt>\n\n",
	"If this is not your fault, you might want to select this text and copy it into a bug report."))
	}
	if (inherits(result, "error") && doFailureLog) { addToLog("# FAILED") }
	return(result)
}

guiTryEvalSink <- function(...) {
	cmdOutput <- capture.output(result <- guiTryEval(...))
	if (inherits(result, "error")) {
		return("FAILED")
	}
	return(cmdOutput)
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

guiMessageDialog <- function(type="info", ...) {
	myString <- paste(sep='', ...)
	myString <- gsub('%', '%%', myString)
	myString <- gsub('&&', '&amp;&amp;', myString)
	myString <- gsub('& ', '&amp; ', myString)
	myString <- gsub('<<', '&lt;&lt;', myString)
	myString <- gsub('<-', '&lt;-', myString)
	myString <- gsub('< ', '&lt; ', myString)
	myButtons <- switch(type,
		error="close",
		info="ok",
		question="yes-no"
	)
	dialog <- gtkMessageDialogNewWithMarkup(theWidget("hs_window"), 
		"destroy-with-parent", type, myButtons, myString)
	result <- dialog$run() # make it modal
	dialog$destroy()
	theWidget("hs_window")$present()
	return(if (result == GtkResponseType["yes"]) { "yes" } else { NULL })
}

## SAVE FILE DIALOG

# returns character string, or NA if cancelled
choose.file.save <- function(default="", caption="Save File", filters=Filters[c("All"),]) {
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

