## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

updateImportPage <- function() {
	# generate summary table
	
	dfName <- dfData <- dfStart <- dfEnd <- dfLength <- dfFreq <- 
		dfLoc <- dfQual <- dfExtra <- dfRole <- character(length(hsp$data))
	
	for (i in seq(along=hsp$data)) {
		myLength <- end(hsp$data[[i]]) - start(hsp$data[[i]])
		myAvgFreq <- myLength / nrow(hsp$data[[i]])
		
		dfName[i] <- names(hsp$data)[i]
		dfData[i] <- attr(hsp$data[[i]], "dataname")
		dfStart[i] <- format(start(hsp$data[[i]]))
		dfEnd[i] <- format(end(hsp$data[[i]]))
		dfLength[i] <- as.byString(myLength, digits=2, explicit=T)
		#dfFreq[i] <- as.byString(myAvgFreq, digits=2)
		dfFreq[i] <- attr(hsp$data[[i]], "timestep")
		
		dfLoc[i] <- 'NA'
		myLoc <- attr(hsp$data[[i]], "location.xy")
		if (!is.null(myLoc)) {
			dfLoc[i] <- paste('(', myLoc[1], ', ', myLoc[2], ')', sep='')
		}
		
		dfQual[i] <- class(hsp$data[[i]]$Qual)[1]
		if (is.factor(hsp$data[[i]]$Qual) || is.numeric(hsp$data[[i]]$Qual)) {
			levelsFn <- if (is.factor(hsp$data[[i]]$Qual))
			{ levels } else { unique }
			dfQual[i] <- paste(' (', toString( paste(
				levelsFn(hsp$data[[i]]$Qual),
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
						levels(hsp$data[[i]][[xcol]]),
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
		Data=dfData,
		Start=dfStart,
		End=dfEnd,
		Length=dfLength,
		Timestep=dfFreq,
		Location_X.Y=dfLoc,
		Qual=dfQual,
		Extra_data=dfExtra,
		Role=dfRole,
		stringsAsFactors=F)
		)
	myTreeView <- theWidget("import_summary_treeview")
	myTreeView$setModel(dfModel)
	myTreeView$columnsAutosize()
	
	.hydrosanity$update$import <<- F
	APPWIN$present()
}

## ACTIONS

.hs_on_import_displayfile_button_clicked <- function(button) {
	APPWIN$setSensitive(F)
	on.exit(APPWIN$setSensitive(T))
	setStatusBar("")
	
	filenames <- choose.files(multi=T)
	APPWIN$present()
	if (length(filenames)==0) { return() }
	file.show(filenames)
}

.hs_on_import_viewtable_button_clicked <- function(button) {
	APPWIN$setSensitive(F)
	on.exit(APPWIN$setSensitive(T))
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
			blobName, ") will be shown.")
	}
	tmp <- hsp$data[[thisIndex]][,-1]
	row.names(tmp) <- make.unique(format(hsp$data[[thisIndex]]$Time))
	tmp2 <- edit(tmp, title=blobName)
	attributes(tmp2) <- attributes(tmp)
	if (!identical(tmp, tmp2)) {
		addLogComment("Edited data object ", blobName, " interactively.")
		hsp$data[[thisIndex]][,-1] <<- tmp2
		setStatusBar(sprintf('Edited data object "%s"', blobName))
		datasetModificationUpdate()
	}
}

.hs_on_import_file_button_clicked <- function(button) {
	APPWIN$setSensitive(F)
	on.exit(APPWIN$setSensitive(T))
	setStatusBar("")
	
	filenames <- choose.files()
	APPWIN$present()
	if (length(filenames)==0) { return() }
	
	import.cmd.str <- rep("", length(filenames))
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
		importFn <- importSpec[1]
		# user may have changed options in GUI, so use myOptionString
		for (i in seq(along=filenames)) {
			import.cmd.str[i] <- sprintf(
				'hsp$data[["%s"]] <- %s("%s"%s)', 
				dataName[i], importFn, filenames[i], myOptionString)
		}
	}
	else if (theWidget("import_file_with_time_radio")$getActive()) {
		myTimeCol <- theWidget("import_time_column_spinbutton")$getValue()
		myTimeFormat <- theWidget("import_time_format_comboboxentry")$getActiveText()
		for (i in seq(along=filenames)) {
			import.cmd.str[i] <- sprintf(
				'hsp$data[["%s"]] <- read.timeblob("%s", timeCol=%i, timeFormat="%s"%s)',
				dataName[i], filenames[i], myTimeCol, myTimeFormat, myOptionString)
		}
	}
	else if (theWidget("import_file_seq_radio")$getActive()) {
		myStartTime <- theWidget("import_time_start_entry")$getText()
		myTimeSeqBy <- theWidget("import_time_step_comboboxentry")$getActiveText()
		for (i in seq(along=filenames)) {
			import.cmd.str[i] <- sprintf(
				'hsp$data[["%s"]] <- read.timeblob("%s", startTime="%i", timeSeqBy="%s"%s)',
				dataName[i], filenames[i], myStartTime, myTimeSeqBy, myOptionString)
		}
	}
	
	addLogComment("Import data from file")
	for (i in seq(along=filenames)) {
		result <- guiDo(import.cmd.str[i], isParseString=T)
		setStatusBar(sprintf('Imported file "%s" to hsp$data[["%s"]]', 
			basename(filenames[i]), dataName[i]))
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
	
	datasetModificationUpdate()
}

.hs_on_import_summary_treeview_name_edited <- function(cell, path.string, new.text, user.data) {
	blobIndex <- as.numeric(path.string)+1
	blobName <- names(hsp$data)[blobIndex]
	if (new.text == blobName) { return() }
	if (new.text == "") { return() }
	addLogComment(paste("Rename data object", blobName))
	guiDo(isExpression=T, bquote(
		names(hsp$data)[.(blobIndex)] <- .(new.text)
	))
	
	setStatusBar(sprintf('Renamed data object "%s" to "%s"', blobName, new.text))
	datasetModificationUpdate()
}

.hs_on_import_summary_treeview_dataname_edited <- function(cell, path.string, new.text, user.data) {
	blobIndex <- as.numeric(path.string)+1
	blobName <- names(hsp$data)[blobIndex]
	blobDataName <- attr(hsp$data[[blobIndex]], "dataname")
	if (new.text == blobDataName) { return() }
	if (new.text == "") { return() }
	addLogComment(paste("Set data name for object", blobName))
	guiDo(isExpression=T, bquote(
		attr(hsp$data[[.(blobName)]], "dataname") <- .(new.text)
	))
	
	setStatusBar(sprintf('Set data name for object "%s" to "%s"', blobName, new.text))
	datasetModificationUpdate()
}

.hs_on_import_summary_treeview_role_edited <- function(cell, path.string, new.text, user.data) {
	blobIndex <- as.numeric(path.string)+1
	blobName <- names(hsp$data)[blobIndex]
	if (attr(hsp$data[[blobIndex]], "role") == new.text) { return() }
	setDataRole(blobName, new.text)
	datasetModificationUpdate()
}

.hs_on_import_edit_metadata_button_clicked <- function(button) {
	APPWIN$setSensitive(F)
	on.exit(APPWIN$setSensitive(T))
	setStatusBar("")
	
	blobIndices <- treeViewGetSelectedIndices(theWidget("import_summary_treeview"))
	if (length(blobIndices)==0) {
		errorDialog("No items selected.")
		return()
	}
	
	dfName <- dfData <- dfRole <- character(length(blobIndices))
	dfLocX <- dfLocY <- numeric(length(blobIndices))
	
	for (k in seq(along=blobIndices)) {
		# 'i' indexes hsp$data; 'k' indexes metadata (subset)
		i <- blobIndices[k]
		dfName[k] <- names(hsp$data)[i]
		dfData[k] <- attr(hsp$data[[i]], "dataname")
		dfRole[k] <- attr(hsp$data[[i]], "role")
		if (is.null(dfRole[k])) { dfRole[k] <- "" }
		myLoc <- attr(hsp$data[[i]], "location.xy")
		if (is.null(myLoc)) { myLoc <- c(NA, NA) }
		dfLocX[k] <- myLoc[1]
		dfLocY[k] <- myLoc[2]
	}
	dfRole <- factor(dfRole, levels=c("RAIN", "FLOW", "OTHER"))
	
	metadata <- data.frame(
		ItemName=dfName,
		DataName=dfData,
		Role=dfRole,
		X_Long=dfLocX,
		Y_Lat=dfLocY,
		check.names=F,
		stringsAsFactors=F
	)
	
	newMeta <- guiDo(editAsText(metadata), doLog=F)
	if (identical(metadata, newMeta)) {
		return()
	}
	
	# TODO: check that number of rows is the same...
	
	addLogComment(paste("Edited metadata for", length(blobIndices), "objects"))
	for (k in seq(along=blobIndices)) {
		# 'i' indexes hsp$data; 'k' indexes metadata (subset)
		i <- blobIndices[k]
		if (!is.null(newMeta$ItemName)
		&& !is.na(newMeta$ItemName[k])
		&& (newMeta$ItemName[k] != "")) {
			names(hsp$data)[i] <<- newMeta$ItemName[k]
		}
		if (!is.null(newMeta$DataName)
		&& !is.na(newMeta$DataName[k])) {
			attr(hsp$data[[i]], "dataname") <<- newMeta$DataName[k]
		}
		if (!is.null(newMeta$Role)
		&& !is.na(newMeta$Role[k])) {
			attr(hsp$data[[i]], "role") <<- as.character(newMeta$Role[k])
		}
		if (!is.null(newMeta$X_Long)
		&& !is.null(newMeta$Y_Lat)) {
			myLoc <- c(newMeta$X_Long[k], newMeta$Y_Lat[k])
			attr(hsp$data[[i]], "location.xy") <<- 
				if (any(is.na(myLoc))) { NULL } else { myLoc }
		}
	}
	
	setStatusBar(paste("Edited metadata for", length(blobIndices), "objects"))
	datasetModificationUpdate()
}

.hs_on_import_remove_blob_button_clicked <- function(button) {
	APPWIN$setSensitive(F)
	on.exit(APPWIN$setSensitive(T))
	setStatusBar("")
	
	blobIndices <- treeViewGetSelectedIndices(theWidget("import_summary_treeview"))
	if (length(blobIndices)==0) {
		errorDialog("No items selected.")
		return()
	}
	
	blobNames <- names(hsp$data)[blobIndices]
	if (is.null(questionDialog("Remove item(s) ", 
		paste(blobNames,collapse=', '), "?"))) {
		return()
	}
	addLogComment("Remove data object(s)")
	guiDo(isExpression=T, bquote(
		hsp$data[.(blobNames)] <- NULL
	))
	
	setStatusBar(sprintf('Removed data object(s) %s', paste(blobNames,collapse=', ')))
	datasetModificationUpdate()
}

.hs_on_import_makefactor_button_clicked <- function(button) {
	APPWIN$setSensitive(F)
	on.exit(APPWIN$setSensitive(T))
	setStatusBar("")
	
	blobIndices <- treeViewGetSelectedIndices(theWidget("import_summary_treeview"))
	if (length(blobIndices)==0) {
		errorDialog("No items selected.")
		return()
	}
	factorCmdRaw <- theWidget("import_makefactor_comboboxentry")$getActiveText()
	
	addLogComment("Convert quality codes")
	factor_fn.cmd.str <- sprintf("tmp.factor <- function(x){ %s }", factorCmdRaw)
	guiDo(factor_fn.cmd.str, isParseString=T)
	
	for (i in blobIndices) {
		blobName <- names(hsp$data)[i]
		guiDo(isExpression=T, bquote(
			hsp$data[[.(i)]]$Qual <- factor(tmp.factor(hsp$data[[.(i)]]$Qual), exclude=NULL)
		))
		setStatusBar(sprintf('Converted quality codes of object "%s"', blobName))
	}
	addToLog('rm(tmp.factor)')
	rm(tmp.factor, envir=.GlobalEnv)
	datasetModificationUpdate()
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
	
	if (doLogComment) { addLogComment("Set data role") }
	
	guiDo(isExpression=T, bquote(
		attr(hsp$data[[.(blobName)]], "role") <- .(role)
	))
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
	
	theWidget("import_file_radio_options_notebook")$setCurrentPage(newPageIdx)
}

.hs_on_import_robj_radio_clicked <- function(button) {
	infoDialog(isMarkup=T, paste(sep='',
	'Importing data from R must be done from the R console, using a command like:',
	'\n\n',
	'<tt>hsp$data[["myName"]] &lt;- timeblob(Time=myTime, Data=myData)</tt>',
	'\n\n',
	'See <tt>help(timeblob)</tt> for details.'))
}

.hs_on_import_known_format_combobox_changed <- function(widget) {
	kfIndex <- widget$getActive()+1
	theWidget("import_options_entry")$setText(.KNOWN_FORMATS[[kfIndex]][2])
}


