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
		dfLength[i] <- difftimeString(myLength, digits=2)
		#dfFreq[i] <- difftimeString(myAvgFreq, digits=2)
		dfFreq[i] <- attr(hsp$data[[i]], "timestep")
		
		dfData[i] <- names(hsp$data[[i]])[2]
		dfQual[i] <- class(hsp$data[[i]]$Qual[1])[1]
		if (dfQual[i] == "factor") {
			dfQual[i] <- paste(' (',
				paste(levels(hsp$data[[i]]$Qual[1]),collapse="/"),
			')', sep='')
		}
		dfExtra[i] <- ""
		for (xcol in seq(4, ncol(hsp$data[[i]]))) {
			dfExtra[i] <- paste(dfExtra[i],
				if(xcol > 4)', ',
				names(hsp$data[[i]])[xcol],
				if (is.factor(hsp$data[[i]][1,xcol])) {
					paste(' (',
					paste(levels(hsp$data[[i]][1,xcol]),collapse="/"),
					')', sep='')
				},
				sep=''
			)
		}
		
		dfRole[i] <- attr(hsp$data[[i]], "role")
		if (is.null(dfRole[i])) { dfRole[i] <- "" }
	}
	
	dfModel <- rGtkDataFrame(data.frame(
		Name=dfName,
		Start=dfStart,
		End=dfEnd,
		Length=dfLength,
		Avg_Freq=dfFreq,
		Data=dfData,
		Qual=dfQual,
		Extra_columns=dfExtra,
		Role=dfRole,
		stringsAsFactors=F)
		)
	myTreeView <- theWidget("import_summary_treeview")
	myTreeView$setModel(dfModel)
	
	theWidget("hs_window")$present()
}

## ACTIONS

on_import_robj_button_clicked <- function(button) {
	theWidget("hs_window")$setSensitive(F)
	on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	data.cmd <- theWidget("import_robj_entry")$getText()
	dataName <- make.names(data.cmd)
	addLogSeparator()
	addLogComment("Import data from R object ", data.cmd)
	# first set a local variable x and do checks
	x <- guiTryEval(data.cmd)
	if (inherits(x, "try-error")) { return() }
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
	updateImportPage()
}

on_import_displayfile_button_clicked <- function(button) {
	theWidget("hs_window")$setSensitive(F)
	on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	filenames <- choose.files(multi=T)
	theWidget("hs_window")$present()
	if (length(filenames)==0) { return() }
	file.show(filenames)
}

on_import_file_button_clicked <- function(button) {
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
			import.cmd[i] <- sprintf('hsp$data[["%s"]] <<- read.timeblob("%s", startTime=%i, timeSeqBy="%s"%s)',
				dataName[i], filenames[i], myStartTime, myTimeSeqBy, myOptionString)
		}
	}
	
	addLogSeparator()
	addLogComment("Import data from file")
	for (i in seq(along=filenames)) {
		addToLog(import.cmd[i])
		result <- guiTryEval(import.cmd[i])
		if (inherits(result, "try-error")) { return() }
		setStatusBar(sprintf('Imported file "%s" to hsp$data[["%s"]]', basename(filenames[i]), dataName[i]))
		# mark as rain/flow/etc
		setDataRole(dataName[i], doLogComment=F)
		# update table etc: inefficient, but gives user more feedback
		updateImportPage()
	}
	
	str.cmd <- 'str(hsp$data)'
	addLogItem("Display a simple summary (structure) of the data.", str.cmd)
	
	theWidget("import_options_expander")$setExpanded(FALSE)
	theWidget("import_makechanges_expander")$setExpanded(TRUE)
	
	#updateImportPage()
}

setDataRole <- function(blobName, role=NULL, doLogComment=T) {
	if (is.null(role)) {
		role <- "RAIN"
	}
	
	mv.cmd <- sprintf('attr(hsp$data[["%s"]], "role") <<- "%s"', blobName, role)
	if (doLogComment) { addLogComment("Set data role") }
	addToLog(mv.cmd)
	result <- guiTryEval(mv.cmd)
	if (inherits(result, "try-error")) { return() }
	setStatusBar(sprintf('Set data role for object "%s" to "%s"', blobName, role))
}

on_import_summary_treeview_name_edited <- function(cell, path.string, new.text, user.data) {
	blobIndex <- as.numeric(path.string)+1
	blobName <- names(hsp$data)[blobIndex]
	if (new.text == blobName) { return() }
	mv.cmd <- sprintf('names(hsp$data)[%i] <<- "%s"', blobIndex, new.text)
	addLogItem(paste("Rename data object", blobName), mv.cmd)
	result <- guiTryEval(mv.cmd)
	if (inherits(result, "try-error")) { return() }
	setStatusBar(sprintf('Renamed data object "%s" to "%s"', blobName, new.text))
	
	updateImportPage()
}

on_import_summary_treeview_role_edited <- function(cell, path.string, new.text, user.data) {
	blobIndex <- as.numeric(path.string)+1
	blobName <- names(hsp$data)[blobIndex]
	if (attr(hsp$data[[blobIndex]], "role") == new.text) { return() }
	
	setDataRole(blobName, new.text)
	
	updateImportPage()
}

on_import_remove_blob_button_clicked <- function(button) {
	theWidget("hs_window")$setSensitive(F)
	on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	blobIndices <- treeViewGetSelectedIndices(theWidget("import_summary_treeview"))
	if (length(blobIndices)==0) { return() }
	
	blobNames <- names(hsp$data)[blobIndices]
	if (is.null(questionDialog("Remove item(s)", paste(blobNames,collapse=', '), "?"))) {
		return()
	}
	rm.cmd <- sprintf('hsp$data[c(%s)] <<- NULL', paste(blobIndices,collapse=','))
	addLogItem("Remove data object(s)", rm.cmd)
	result <- guiTryEval(rm.cmd)
	if (inherits(result, "try-error")) { return() }
	setStatusBar(sprintf('Removed data object(s) %s', paste(blobNames,collapse=', ')))
	
	updateImportPage()
}

on_import_makefactor_button_clicked <- function(button) {
	theWidget("hs_window")$setSensitive(F)
	on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	blobIndices <- treeViewGetSelectedIndices(theWidget("import_summary_treeview"))
	if (length(blobIndices)==0) { return() }
	
	addLogComment("Convert quality codes")
	factorCmdRaw <- theWidget("import_makefactor_comboboxentry")$getActiveText()
	factor_fn.cmd <- sprintf("tmp.factor <<- function(x){ %s }", factorCmdRaw)
	addToLog(factor_fn.cmd)
	result <- guiTryEval(factor_fn.cmd)
	if (inherits(result, "try-error")) { return() }
	
	for (blobIndex in blobIndices) {
		blobName <- names(hsp$data)[blobIndex]
		data.cmd <- sprintf("hsp$data[[%i]]$Qual", blobIndex)
		factor.cmd <- sprintf("%s <<- factor(tmp.factor(%s), exclude=NULL)", data.cmd, data.cmd)
		addToLog(factor.cmd)
		result <- guiTryEval(factor.cmd)
		if (inherits(result, "try-error")) { return() }
		setStatusBar(sprintf('Converted quality codes of object "%s" to factor.', blobName))
	}
	addToLog('rm(tmp.factor)')
	tmp.factor <<- NULL
	
	updateImportPage()
}


## NON-ACTIONS, just interface bits and pieces

on_import_file_radio_options_toggled <- function(button) {
	
	newPageIdx <- 0
	if (theWidget("import_known_format_radio")$getActive()) {
		newPageIdx <- 0
		on_import_known_format_combobox_changed(
			theWidget("import_known_format_combobox"))
	} else {
		theWidget("import_options_expander")$setExpanded(TRUE)
		# TODO: need to check switching from known format or not
		theWidget("import_options_entry")$setText(hsp$defaultImportOptions)
	}
	
	if (theWidget("import_file_with_time_radio")$getActive()) {
		newPageIdx <- 1
	}
	
	if (theWidget("import_file_seq_radio")$getActive()) {
		newPageIdx <- 2
	}
	
	theWidget("import_file_radio_options_notebook")$setCurrentPage(newPageIdx)
}

on_import_known_format_combobox_changed <- function(widget) {
	kfIndex <- widget$getActive()+1
	theWidget("import_options_entry")$setText(.KNOWN_FORMATS[[kfIndex]][2])
}


