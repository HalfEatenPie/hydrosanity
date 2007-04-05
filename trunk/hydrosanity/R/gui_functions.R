## Hydrosanity: an interface for exploring hydrological time series in R
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

setCairoWindowButtons <- function(name, identify=NA, centre=NA, zoomin=NA, zoomout=zoomin, log=NA, setperiod=NA) {
	# check that the window exists
	if (is.null(.hydrosanity$win[[name]])) { return() }
	# first set the call to NULL so that toggle-button updates can be ignored
	.hydrosanity$call[[name]] <<- NULL
	# set which buttons are visible
	.hydrosanity$win.gui[[name]]$getWidget("plot_identify_button")$setSensitive(!is.na(identify))
	.hydrosanity$win.gui[[name]]$getWidget("plot_centre_button")$setSensitive(!is.na(centre))
	.hydrosanity$win.gui[[name]]$getWidget("plot_zoomin_button")$setSensitive(!is.na(zoomin))
	.hydrosanity$win.gui[[name]]$getWidget("plot_zoomout_button")$setSensitive(!is.na(zoomout))
	.hydrosanity$win.gui[[name]]$getWidget("plot_log_togglebutton")$setSensitive(!is.na(log))
	.hydrosanity$win.gui[[name]]$getWidget("plot_setperiod_button")$setSensitive(!is.na(setperiod))
	if (!is.na(log)) {
		.hydrosanity$win.gui[[name]]$getWidget("plot_log_togglebutton")$setActive(log)
	}
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
	if (is.null(myCall$xscale)) {
		.hs_on_timeperiod_reset_button_clicked()
		return()
	}
	timelim <- as.POSIXct(myCall$xscale)
	periodString <- paste(format(round(timelim, "days")), collapse=" to ")
	theWidget("timeperiod_chosenperiod_entry")$setText(periodString)
	.hs_on_timeperiod_updateperiod_button_clicked()
	infoDialog("Set time period for analysis: ", periodString)
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

setupIconView <- function(iconView) {
	rainPixbuf <- gdkPixbufNewFromFile(getpackagefile("icon_RAIN.png"))$retval
	flowPixbuf <- gdkPixbufNewFromFile(getpackagefile("icon_FLOW.png"))$retval
	otherPixbuf <- gdkPixbufNewFromFile(getpackagefile("icon_OTHER.png"))$retval
	# (or NULL)
	
	list_store <- gtkListStore("character", "GdkPixbuf")
	
	for (i in seq(along=hsp$data)) {
		iter <- list_store$append()$iter
		list_store$set(iter, 0, names(hsp$data)[i])
		list_store$set(iter, 1, switch(attr(hsp$data[[i]], "role"),
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
		iconView$selectPath(gtkTreePathNewFromString("0"))
	} else {
		for (p in selPaths) { iconView$selectPath(p) }
	}
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

guiTryEval <- function(...) {
	guiDo(..., isParseString=T, doStop=F)
}

guiDo <- function(expr, isExpression=F, isParseString=F, doLog=T, doFailureDialog=T, doFailureLog=doLog, doStop=T) {
	setCursor("watch")
	result <- NULL
	if (!isExpression && !isParseString) {
		expr <- substitute(expr)
	}
	if (isParseString) {
		result <- tryCatch(eval(parse(text=expr)), ###### envir=.GlobalEnv
			error=function(e)e)
	} else {
		result <- tryCatch(eval(expr, envir=.GlobalEnv), 
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

