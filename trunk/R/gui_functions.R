## HydroSanity: an interface for exploring hydrological time series in R
##
## Time-stamp: <2007-03-05 00:00:00 Felix>
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

## SAVE FILE DIALOG

# returns character string, or NA if cancelled
choose.file.save <- function(default="", caption="Save File") {
	dialog <- gtkFileChooserDialog(caption, NULL, "save",
		"gtk-cancel", GtkResponseType["cancel"],
		"gtk-save", GtkResponseType["accept"])
	dialog$setCurrentName(default)
	
	ff <- gtkFileFilterNew()
	ff$setName("All Files")
	ff$addPattern("*")
	dialog$addFilter(ff)
	
	if (dialog$run() == GtkResponseType["accept"]) {
		filename <- dialog$getFilename()
		dialog$destroy()
		#TODO: warn about replace?
		return(filename)
	} else {
		dialog$destroy()
		return(NA)
	}
}


## PLOT WINDOW FUNCTIONS

setPlotDevice <- function(name) {
	#imageFile <- paste(tempfile(), ".png", sep="")
	#myWidth <- theWidget("timeperiod_image")$getAllocation()$width
	#myHeight <- theWidget("timeperiod_image")$getAllocation()$height
	#png(imageFile, width=myWidth, height=myHeight)

	if (is.null(.hydrosanity$dev[[name]]) ||
		(.hydrosanity$dev[[name]] %notin% dev.list())) {
		if (require("cairoDevice", quietly=TRUE)) {
			newCairoWindow()
		} else {
			do.call(getOption("device"), list())
		}
		.hydrosanity$dev[[name]] <<- dev.cur()
	}
	dev.set(.hydrosanity$dev[[name]])
}

newCairoWindow <- function() {
	plotGUI <- gladeXMLNew(getpackagefile("hydrosanity.glade"), 
		root="plot_window")
	
	gladeXMLSignalAutoconnect(plotGUI)
	myWin <- plotGUI$getWidget("plot_window")
	gSignalConnect(myWin, "delete-event", on_plot_delete_event)
	
	.hydrosanity$win[[length(.hydrosanity$win)+1]] <<- plotGUI$getWidget("plot_window")
	newDev <- plotGUI$getWidget("drawingarea")
	asCairoDevice(newDev)
	myWin$setTitle(paste("HydroSanity: Plot", dev.cur()))
}

on_plot_delete_event <- function(widget, event, user.data) {
	myWin <- widget
	myTitle <- myWin$getTitle()
	devnum <- as.integer(sub("HydroSanity: Plot ", "", myTitle))
	dev.off(devnum)
	for (i in seq(along=.hydrosanity$win)) {
		if (myWin == .hydrosanity$win[[i]]) {
			.hydrosanity$win[[i]] <<- NULL
		}
	}
	myWin$destroy()
	theWidget("hs_window")$present()
}


## TREE VIEW AND ICON VIEW HELPERS

insertTreeViewTextColumns <- function(treeView, colNames=colnames(treeView$getModel()), editors=NULL, combo=NULL) {
	for (i in seq(along=colNames)) {
		renderer <- gtkCellRendererText()
		renderer$set(xalign = 0.0)
		if (!is.null(combo[[ colNames[i] ]])) {
			renderer <- gtkCellRendererCombo()
			renderer$set(model = rGtkDataFrame(data.frame(combo[[ colNames[i] ]])), text_column = 0, has_entry = F)
		}
		#renderer <- gtkCellRendererCombo()
		#renderer$set(model = rGtkDataFrame(empty_data_frame("character")), text_column = 0, has_entry = F)
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
	indices <- sapply(selPaths, function(x) x$getIndices()) + 1
}

# it's not enough to get the item indices since user can re-order them!
iconViewGetSelectedNames <- function(iconView) {
	selPaths <- iconView$getSelectedItems()
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

guiTryEval <- function(commandText, doFailureDialog=T, doFailureLog=T) {
	setCursor("watch")
	result <- try(eval(parse(text=commandText)))
	setCursor()
	if (inherits(result, "try-error")) {
		if (doFailureDialog) {
			setStatusBar("")
			popTryError(commandText)
		}
		if (doFailureLog) { addToLog("# FAILED") }
	}
	return(result)
}

guiTryEvalSink <- function(...) {
	cmdOutput <- capture.output(result <- guiTryEval(...))
	if (inherits(result, "try-error")) {
		return("FAILED")
	}
	return(cmdOutput)
}

popTryError <- function(commandText) {
	errorDialog(paste(sep="",
		"A command has failed: \n\n", commandText, "\n\n",
		"The action you requested has not been completed. ",
		"Refer to the R Console for details."))
}



