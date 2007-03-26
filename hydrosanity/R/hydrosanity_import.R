## Hydrosanity: an interface for exploring hydrological time series in R
##
## Time-stamp: <2007-03-05 00:00:00 Felix>
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

updateImportPage <- function() {
	# generate summary table
	
	dfName <- dfStart <- dfEnd <- dfLength <- dfFreq <- character(length(hsp$data))
	dfData <- dfRole <- dfQual <- dfExtra <- character(length(hsp$data))
	
	for (i in seq(along=hsp$data)) {
		myLength <- end.timeblob(hsp$data[[i]]) - start.timeblob(hsp$data[[i]])
		myAvgFreq <- myLength / nrow(hsp$data[[i]])
		
		dfName[i] <- names(hsp$data)[i]
		dfStart[i] <- format(start.timeblob(hsp$data[[i]]))
		dfEnd[i] <- format(end.timeblob(hsp$data[[i]]))
		dfLength[i] <- as.byString(myLength, digits=2)
		#dfFreq[i] <- as.byString(myAvgFreq, digits=2)
		dfFreq[i] <- attr(hsp$data[[i]], "timestep")
		
		dfData[i] <- attr(hsp$data[[i]], "dataname")
		dfQual[i] <- class(hsp$data[[i]]$Qual)[1]
		if (is.factor(hsp$data[[i]]$Qual) || is.numeric(hsp$data[[i]]$Qual)) {
			levelsFn <- if (is.factor(hsp$data[[i]]$Qual))
			{ levels } else { unique }
			dfQual[i] <- paste(' (', toString( paste(
				sort(levelsFn(hsp$data[[i]]$Qual)),
			collapse="/"), width=30), ')', sep='')
		}
		dfExtra[i] <- ""
		if (ncol(hsp$data[[i]]) >= 4) {
		for (xcol in seq(4, ncol(hsp$data[[i]]))) {
			dfExtra[i] <- paste(dfExtra[i],
				if(xcol > 4)', ',
				names(hsp$data[[i]])[xcol],
				if (is.factor(hsp$data[[i]][[xcol]])) {
					paste(' (', toString( paste(
						sort(levels(hsp$data[[i]][[xcol]])),
					collapse="/"), width=30), ')', sep='')
				},
				sep=''
			)
		}
		}
		
		dfRole[i] <- attr(hsp$data[[i]], "role")
		if (is.null(dfRole[i])) { dfRole[i] <- "" }
	}
	
	dfModel <- rGtkDataFrame(data.frame(
		Name=dfName,
		Start=dfStart,
		End=dfEnd,
		Length=dfLength,
		Timestep=dfFreq,
		Data=dfData,
		Qual=dfQual,
		Extra_data=dfExtra,
		Role=dfRole,
		stringsAsFactors=F)
		)
	myTreeView <- theWidget("import_summary_treeview")
	myTreeView$setModel(dfModel)
	myTreeView$columnsAutosize()
	
	# this call to updateImportPage means dataset was modified
	.hydrosanity$update$timeperiod <<- T
	.hydrosanity$update$explore <<- T
	
	.hydrosanity$update$import <<- F
	theWidget("hs_window")$present()
}

## ACTIONS

.hs_on_import_robj_button_clicked <- function(button) {
	theWidget("hs_window")$setSensitive(F)
	on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	data.cmd <- theWidget("import_robj_entry")$getText()
	dataName <- make.names(data.cmd)
	addLogComment("Import data from R object ", data.cmd)
	# first set a local variable x and do checks
	x <- guiTryEval(data.cmd, doLog=F)
	if (inherits(x, "error")) { return() }
	# check the user-defined object
	if (is.list(x) && !is.data.frame(x)) {
		# it should be a list of timeblobs
		xNames <- names(x)
		if (is.null(xNames)) { xNames <- paste(dataName, seq(along=x), sep=".") }
		# only warn once about invalid data
		alreadyWarned <- F
		for (i in seq(along=x)) {
			subDataName <- xNames[i]
			if (is.timeblob(x[[i]])) {
				addToLog(sprintf('hsp$data[["%s"]] <- %s[[%i]]', subDataName, data.cmd, i))
				hsp$data[[subDataName]] <<- x[[i]]
				setStatusBar(sprintf('Imported object %s[[%i]] to hsp$data[["%s"]]', data.cmd, i, subDataName))
			} else {
				setStatusBar("")
				if (!alreadyWarned) {
					errorDialog(subDataName, " (component of list ", data.cmd, ") is not a data frame with first column \"Time\" of type POSIXct, and third column \"Qual\". Further warnings will not be shown.")
					alreadyWarned <- T
				}
			}
		}
	}
	else {
		# it should be a single timeblob
		if (is.timeblob(x)) {
			addToLog(sprintf('hsp$data[["%s"]] <- %s', dataName, data.cmd))
			hsp$data[[dataName]] <<- x
			setStatusBar(sprintf('Imported object %s to hsp$data[["%s"]]', data.cmd, dataName))
		} else {
			errorDialog(data.cmd, "is not a data frame with first column \"Time\" of type POSIXct, and third column \"Qual\", or a list of these.")
		}
	}
	.hydrosanity$modified <<- T
	updateImportPage()
}

.hs_on_import_displayfile_button_clicked <- function(button) {
	theWidget("hs_window")$setSensitive(F)
	on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	filenames <- choose.files(multi=T)
	theWidget("hs_window")$present()
	if (length(filenames)==0) { return() }
	file.show(filenames)
}

.hs_on_import_viewtable_button_clicked <- function(button) {
	theWidget("hs_window")$setSensitive(F)
	on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	blobIndices <- treeViewGetSelectedIndices(theWidget("import_summary_treeview"))
	if (length(blobIndices)==0) {
		errorDialog("No items selected.")
		return()
	}
	thisIndex <- blobIndices[1]
	blobName <- names(hsp$data)[thisIndex]
	if (length(blobIndices) > 1) {
		infoDialog("Only the first selected item (",
			names(hsp$data)[thisIndex], ") will be shown.")
	}
	tmp <- hsp$data[[thisIndex]][,-1]
	row.names(tmp) <- make.unique(format(hsp$data[[thisIndex]]$Time))
	tmp2 <- edit(tmp, title=blobName)
	attributes(tmp2) <- attributes(tmp)
	if (!identical(tmp, tmp2)) {
		hsp$data[[thisIndex]][,-1] <<- tmp2
		setStatusBar(sprintf('Edited data object "%s"', blobName))
		.hydrosanity$modified <<- T
		updateImportPage()
	}
}

.hs_on_import_file_button_clicked <- function(button) {
	theWidget("hs_window")$setSensitive(F)
	on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	filenames <- choose.files()
	theWidget("hs_window")$present()
	if (length(filenames)==0) { return() }
	
	import.cmd <- rep("", length(filenames))
	dataName <- rep("", length(filenames))
	
	## Fix filename for MS - otherwise eval/parse strip the \\.
	for (i in seq(along=filenames)) {
		filenames[i] <- gsub("\\\\", "/", filenames[i])
		dataName[i] <- get.stem(filenames[i])
	}
	
	myOptionString <- theWidget("import_options_entry")$getText()
	if (nchar(myOptionString) > 0) {
		myOptionString <- paste(",", myOptionString)
	}
	
	if (theWidget("import_known_format_radio")$getActive()) {
		kfIndex <- theWidget("import_known_format_combobox")$getActive()+1
		importSpec <- .KNOWN_FORMATS[[kfIndex]]
		myImportFnName <- importSpec[1]
		# user may have changed options in GUI, so use myOptionString
		for (i in seq(along=filenames)) {
			import.cmd[i] <- sprintf('hsp$data[["%s"]] <<- %s("%s"%s)', 
				dataName[i], myImportFnName, filenames[i], myOptionString)
		}
	}
	else if (theWidget("import_file_with_time_radio")$getActive()) {
		myTimeCol <- theWidget("import_time_column_spinbutton")$getValue()
		myTimeFormat <- theWidget("import_time_format_comboboxentry")$getActiveText()
		for (i in seq(along=filenames)) {
			import.cmd[i] <- sprintf('hsp$data[["%s"]] <<- read.timeblob("%s", timeCol=%i, timeFormat="%s"%s)',
				dataName[i], filenames[i], myTimeCol, myTimeFormat, myOptionString)
		}
	}
	else if (theWidget("import_file_seq_radio")$getActive()) {
		myStartTime <- theWidget("import_time_start_entry")$getText()
		myTimeSeqBy <- theWidget("import_time_step_comboboxentry")$getActiveText()
		for (i in seq(along=filenames)) {
			import.cmd[i] <- sprintf('hsp$data[["%s"]] <<- read.timeblob("%s", startTime="%i", timeSeqBy="%s"%s)',
				dataName[i], filenames[i], myStartTime, myTimeSeqBy, myOptionString)
		}
	}
	
	addLogComment("Import data from file")
	for (i in seq(along=filenames)) {
		result <- guiTryEval(import.cmd[i])
		if (inherits(result, "error")) { return() }
		setStatusBar(sprintf('Imported file "%s" to hsp$data[["%s"]]', basename(filenames[i]), dataName[i]))
		# mark as rain/flow/etc
		setDataRole(dataName[i], doLogComment=F)
		# update table etc: inefficient, but gives user more feedback
		updateImportPage()
	}
	
	# basic check included in transcipt, not used in GUI
	addLogComment("Display a simple summary (structure) of the data.")
	addToLog('str(hsp$data)')
	
	theWidget("import_options_expander")$setExpanded(FALSE)
	theWidget("import_makechanges_expander")$setExpanded(TRUE)
	
	.hydrosanity$modified <<- T
	#updateImportPage()
}

.hs_on_import_summary_treeview_name_edited <- function(cell, path.string, new.text, user.data) {
	blobIndex <- as.numeric(path.string)+1
	blobName <- names(hsp$data)[blobIndex]
	if (new.text == blobName) { return() }
	mv.cmd <- sprintf('names(hsp$data)[%i] <<- "%s"', blobIndex, new.text)
	addLogComment(paste("Rename data object", blobName))
	result <- guiTryEval(mv.cmd)
	if (inherits(result, "error")) { return() }
	setStatusBar(sprintf('Renamed data object "%s" to "%s"', blobName, new.text))
	.hydrosanity$modified <<- T
	updateImportPage()
}

.hs_on_import_summary_treeview_role_edited <- function(cell, path.string, new.text, user.data) {
	blobIndex <- as.numeric(path.string)+1
	blobName <- names(hsp$data)[blobIndex]
	if (attr(hsp$data[[blobIndex]], "role") == new.text) { return() }
	setDataRole(blobName, new.text)
	.hydrosanity$modified <<- T
	updateImportPage()
}

.hs_on_import_remove_blob_button_clicked <- function(button) {
	theWidget("hs_window")$setSensitive(F)
	on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	blobIndices <- treeViewGetSelectedIndices(theWidget("import_summary_treeview"))
	if (length(blobIndices)==0) {
		errorDialog("No items selected.")
		return()
	}
	
	blobNames <- names(hsp$data)[blobIndices]
	if (is.null(questionDialog("Remove item(s) ", paste(blobNames,collapse=', '), "?"))) {
		return()
	}
	rm.cmd <- sprintf('hsp$data[c(%s)] <<- NULL', paste(blobIndices,collapse=','))
	addLogComment("Remove data object(s)")
	result <- guiTryEval(rm.cmd)
	if (inherits(result, "error")) { return() }
	setStatusBar(sprintf('Removed data object(s) %s', paste(blobNames,collapse=', ')))
	.hydrosanity$modified <<- T
	updateImportPage()
}

.hs_on_import_makefactor_button_clicked <- function(button) {
	theWidget("hs_window")$setSensitive(F)
	on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	blobIndices <- treeViewGetSelectedIndices(theWidget("import_summary_treeview"))
	if (length(blobIndices)==0) {
		errorDialog("No items selected.")
		return()
	}
	
	addLogComment("Convert quality codes")
	factorCmdRaw <- theWidget("import_makefactor_comboboxentry")$getActiveText()
	factor_fn.cmd <- sprintf("tmp.factor <<- function(x){ %s }", factorCmdRaw)
	result <- guiTryEval(factor_fn.cmd)
	if (inherits(result, "error")) { return() }
	
	for (blobIndex in blobIndices) {
		blobName <- names(hsp$data)[blobIndex]
		data.cmd <- sprintf("hsp$data[[%i]]$Qual", blobIndex)
		factor.cmd <- sprintf("%s <<- factor(tmp.factor(%s), exclude=NULL)", data.cmd, data.cmd)
		result <- guiTryEval(factor.cmd)
		if (inherits(result, "error")) { return() }
		setStatusBar(sprintf('Converted quality codes of object "%s"', blobName))
	}
	addToLog('rm(tmp.factor)')
	rm(tmp.factor, envir=.GlobalEnv)
	.hydrosanity$modified <<- T
	updateImportPage()
}


## NON-ACTIONS, just interface bits and pieces

setDataRole <- function(blobName, role=NULL, doLogComment=T) {
	if (is.null(role)) {
		if (one.step.acf(hsp$data[[blobName]]) > 0.5) {
			role <- "FLOW"
		} else {
			role <- "RAIN"
		}
	}
	
	mv.cmd <- sprintf('attr(hsp$data[["%s"]], "role") <<- "%s"', blobName, role)
	if (doLogComment) { addLogComment("Set data role") }
	result <- guiTryEval(mv.cmd)
	if (inherits(result, "error")) { return() }
	setStatusBar(sprintf('Set data role for object "%s" to "%s"', blobName, role))
}

.hs_on_import_file_radio_options_toggled <- function(button) {
	
	newPageIdx <- 0
	if (theWidget("import_known_format_radio")$getActive()) {
		newPageIdx <- 0
		.hs_on_import_known_format_combobox_changed(
			theWidget("import_known_format_combobox"))
	} else {
		theWidget("import_options_expander")$setExpanded(TRUE)
		# TODO: need to check switching from known format or not
		theWidget("import_options_entry")$setText(
			'sep=",", skip=1, dataname="Data", dataCol=2, qualCol=3')
	}
	
	if (theWidget("import_file_with_time_radio")$getActive()) {
		newPageIdx <- 1
	}
	
	if (theWidget("import_file_seq_radio")$getActive()) {
		newPageIdx <- 2
	}
	
	if (theWidget("import_robj_radio")$getActive()) {
		newPageIdx <- 3
		theWidget("import_options_expander")$setExpanded(FALSE)
	}
	
	theWidget("import_file_radio_options_notebook")$setCurrentPage(newPageIdx)
}

.hs_on_import_known_format_combobox_changed <- function(widget) {
	kfIndex <- widget$getActive()+1
	theWidget("import_options_entry")$setText(.KNOWN_FORMATS[[kfIndex]][2])
}


