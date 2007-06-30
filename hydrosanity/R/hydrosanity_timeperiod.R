## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

updateTimePeriodPage <- function() {
	TXV <- theWidget("timeperiod_summary_textview")
	TRV <- theWidget("timeperiod_summary_treeview")
	
	setTextview(TXV, "")
	TRV$setModel(rGtkDataFrame())
	
	if (is.null(hsp$timePeriod)) {
		theWidget("timeperiod_chosenperiod_entry")$setText("")
	} else {
		timePeriodString <- paste(format(hsp$timePeriod), collapse=" to ")
		theWidget("timeperiod_chosenperiod_entry")$setText(timePeriodString)
		theWidget("timeperiod_updateperiod_button")$setSensitive(FALSE)
	}
	
	if (is.null(hsp$region)) {
		theWidget("scope_region_x_entry")$setText("")
		theWidget("scope_region_y_entry")$setText("")
	} else {
		regionXString <- paste(format(hsp$region$xlim), collapse=" to ")
		regionYString <- paste(format(hsp$region$ylim), collapse=" to ")
		theWidget("scope_region_x_entry")$setText(regionXString)
		theWidget("scope_region_y_entry")$setText(regionYString)
		theWidget("scope_set_region_button")$setSensitive(FALSE)
	}
	
	StateEnv$update$timeperiod <- F
	StateEnv$win$present()
	
	if (length(hsp$data) == 0) {
		return()
	}
	
	# overall time period
	wholePeriod <- c(start.timeblobs(hsp$data), end.timeblobs(hsp$data))
	wholePeriodString <- paste(format(wholePeriod), collapse=" to ")
	theWidget("timeperiod_overallperiod_entry")$setText(wholePeriodString)
	
	# overall spatial extent
	loc <- lapply(hsp$data, attr, "location.xy")
	ok <- (sapply(loc, length) == 2)
	if (any(ok)) {
		tmp.locs <- sapply(hsp$data[ok], attr, "location.xy")
		tmp.locs <- data.frame(x=tmp.locs[1,], y=tmp.locs[2,])
		wholeX <- round(range(tmp.locs$x), digits=3)
		wholeY <- round(range(tmp.locs$y), digits=3)
		regionXString <- paste(format(wholeX), collapse=" to ")
		regionYString <- paste(format(wholeY), collapse=" to ")
		theWidget("scope_overall_x_entry")$setText(regionXString)
		theWidget("scope_overall_y_entry")$setText(regionYString)
	}
	
	# don't generate summary until period has been set explicitly
	if (is.null(hsp$timePeriod)) { return() }
	
	# generate summary
	missingSummary <- capture.output(
		missingFrac <- guiDo(summary.missing.timeblobs(hsp$data, 
			timelim=hsp$timePeriod), doLog=F)
	)
	addTextview(TXV, paste(missingSummary, collapse="\n"))
	
	dfName <- dfMin <- dfQ25 <- dfMedian <- dfQ75 <- dfMax <- dfMissing <- character(length(hsp$data))
	
	for (i in seq(along=hsp$data)) {
		dfName[i] <- names(hsp$data)[i]
		subBlob <- window(hsp$data[[i]], hsp$timePeriod[1], hsp$timePeriod[2])
		myQuantiles <- round(quantile(
			subBlob$Data, probs=c(0, 0.25, 0.5, 0.75, 1), na.rm=T), 
			digits=1)
		dfMin[i] <- myQuantiles[1]
		dfQ25[i] <- myQuantiles[2]
		dfMedian[i] <- myQuantiles[3]
		dfQ75[i] <- myQuantiles[4]
		dfMax[i] <- myQuantiles[5]
		dfMissing[i] <- sprintf('%.1f%%', missingFrac[i]*100)
	}
	
	dfModel <- rGtkDataFrame(data.frame(
		Name=dfName,
		Min=dfMin,
		Q25=dfQ25,
		Median=dfMedian,
		Q75=dfQ75,
		Max=dfMax,
		Missing=dfMissing,
		stringsAsFactors=F)
		)
	TRV$setModel(dfModel)
}


.hs_on_scope_viewdbsitemap_button_clicked <- function(button) {
	StateEnv$win$setSensitive(F)
	on.exit(StateEnv$win$setSensitive(T))
	setStatusBar("")
	
	siteListFile <- theWidget("scope_sitelist_filechooserbutton")$getFilename()
	siteListFormatIndex <- theWidget("scope_sitelist_format_combobox")$getActive()+1
	dataYears <- theWidget("scope_datayears_spinbutton")$getValue()
	doInterpElev <- theWidget("scope_sitemap_elevation_checkbutton")$getActive()
	
	if (is.null(siteListFile)) {
		errorDialog("Choose the site list file.")
		return()
	}
	
	addLogComment("Generate site map from database and select region")
	
	select.call <- call(SITELIST_FORMATS[[siteListFormatIndex]])
	select.call$siteListFile <- siteListFile
	select.call$timelim <- quote(hsp$timePeriod)
	select.call$min.years <- dataYears
	
	select.assign.call <- quote(tmp.sites <- foo)
	select.assign.call[[3]] <- select.call
	
	guiDo(call=select.assign.call)
	
	plot.call <- call('xyplot')
	plot.call[[2]] <- quote(y ~ x)
	plot.call[[3]] <- quote(tmp.sites)
	plot.call$pch <- quote(ifelse(tmp.sites$ok, 19, 21))
	plot.call$aspect <- "iso"
	if (!is.null(hsp$region)) {
		plot.call$xlim <- quote(hsp$region$xlim)
		plot.call$ylim <- quote(hsp$region$ylim)
	}
	
	if (doInterpElev) {
		plot.call$z <- quote(tmp.sites$elev)
	}
	
	plot.call$panel <- quote(panel.pointmap)
	
	addToLog(paste(deparse(plot.call), collapse="\n"))
	guiDo(plotAndPlay(plot.call=plot.call, name="site map", 
		extra.buttons=hydrosanityButtons[c('setregion')],
		eval.args="^tmp"), doLog=F)
	
	guiDo(rm(tmp.sites))
	
	setStatusBar("Generated site map from database")
}

.hs_on_scope_viewdbtimeline_button_clicked <- function(button) {
	StateEnv$win$setSensitive(F)
	on.exit(StateEnv$win$setSensitive(T))
	setStatusBar("")
	
	siteListFile <- theWidget("scope_sitelist_filechooserbutton")$getFilename()
	siteListFormatIndex <- theWidget("scope_sitelist_format_combobox")$getActive()+1
	dataYears <- theWidget("scope_datayears_spinbutton")$getValue()
	
	if (is.null(siteListFile)) {
		errorDialog("Choose the site list file.")
		return()
	}
	
	addLogComment("Generate site map from database and select region")
	
	select.call <- call(SITELIST_FORMATS[[siteListFormatIndex]])
	select.call$siteListFile <- siteListFile
	select.call$xlim <- hsp$region$xlim
	select.call$ylim <- hsp$region$ylim
	select.call$min.years <- dataYears
	
	select.assign.call <- quote(tmp.sites <- foo)
	select.assign.call[[3]] <- select.call
	
	guiDo(call=select.assign.call)
	
	# make rough annual series from start and end dates
	guiDo({
		tmp.coverage <- list()
		for (i in which(tmp.sites$ok)) {
			years <- with(tmp.sites, paste(
				first.year[i]:last.year[i], "-01-01", sep=''))
			tmp.coverage[[tmp.sites$name[i]]] <- timeblob(years, 1)
		}
	})
	
	plot.call <- quote(grid.timeline.plot(tmp.coverage, xscale=hsp$timePeriod))
	
	addToLog(paste(deparse(plot.call), collapse="\n"))
	guiDo(plotAndPlay(plot.call=plot.call, name="timeline", 
		buttons=hydrosanityButtons[c('zoomin','zoomout','centre','setperiod')],
		extra.buttons=NULL, eval.args="^tmp"), doLog=F)
	
	guiDo(rm(tmp.sites, tmp.coverage))
	
	setStatusBar("Generated timeline plot from database")
}

.hs_on_scope_import_button_clicked <- function(button) {
	StateEnv$win$setSensitive(F)
	on.exit(StateEnv$win$setSensitive(T))
	setStatusBar("")
	
	siteListFile <- theWidget("scope_sitelist_filechooserbutton")$getFilename()
	siteListFormatIndex <- theWidget("scope_sitelist_format_combobox")$getActive()+1
	siteDataArchive <- theWidget("scope_sitearchive_filechooserbutton")$getFilename()
	isArchive <- (theWidget("scope_sitearchive_type_combobox")$getActive() == 0)
	if (!isArchive) {
		siteDataArchive <- theWidget("scope_sitearchive_filechooserbutton")$getCurrentFolder()
	}
	dataYears <- theWidget("scope_datayears_spinbutton")$getValue()
	
	if (is.null(siteListFile)) {
		errorDialog("Choose the site list file.")
		return()
	}
	
	if (is.null(siteDataArchive)) {
		errorDialog("Choose the site data archive.")
		return()
	}
	
	select.call <- call(SITELIST_FORMATS[[siteListFormatIndex]])
	select.call$siteListFile <- siteListFile
	select.call$archivePath <- siteDataArchive
	select.call$xlim <- hsp$region$xlim
	select.call$ylim <- hsp$region$ylim
	select.call$timelim <- quote(hsp$timePeriod)
	select.call$min.years <- dataYears
	
	nSites <- sum(eval(select.call)$ok)
	if (nSites >= 10) {
		if (is.null(questionDialog("Import ", nSites, 
			" time series from file? This might take a long time, ",
			"and R will appear to freeze. ",
			"Watch the R console for progress."))) {
			return()
		}
	}
	
	select.call$return.data <- T
	
	select.assign.call <- quote(tmp.sites <- foo)
	select.assign.call[[3]] <- select.call
	
	guiDo(call=select.assign.call)
	guiDo(hsp$data[names(tmp.sites)] <- tmp.sites)
	
	setIsImportMode(FALSE)
	
	datasetModificationUpdate()
}

.hs_on_timeperiod_updateperiod_button_clicked <- function(button) {
	StateEnv$win$setSensitive(F)
	on.exit(StateEnv$win$setSensitive(T))
	setStatusBar("")
	
	myText <- theWidget("timeperiod_chosenperiod_entry")$getText()
	myTimeStrings <- strsplit(myText, " to ")[[1]]
	if (length(myTimeStrings) != 2) {
		errorDialog("Give time period in form \"START to END\".")
		return()
	}
	addLogComment("Set time period for analysis")
	guiDo(call=bquote(
		hsp$timePeriod <- as.POSIXct(.(myTimeStrings))
	))
	setStatusBar("Set time period for analysis: ", myText)
	
	timeperiodModificationUpdate()
}

.hs_on_timeperiod_reset_button_clicked <- function(button) {
	StateEnv$win$setSensitive(F)
	on.exit(StateEnv$win$setSensitive(T))
	setStatusBar("")
	
	addToLog("\n")
	guiDo(hsp$timePeriod <- NULL)
	
	timeperiodModificationUpdate()
}

.hs_on_scope_set_region_button_clicked <- function(button) {
	StateEnv$win$setSensitive(F)
	on.exit(StateEnv$win$setSensitive(T))
	setStatusBar("")
	
	myXText <- theWidget("scope_region_x_entry")$getText()
	myYText <- theWidget("scope_region_y_entry")$getText()
	myXStrings <- strsplit(myXText, " to ")[[1]]
	myYStrings <- strsplit(myYText, " to ")[[1]]
	if ((length(myXStrings) != 2) || (length(myYStrings) != 2)) {
		errorDialog("Give bounds in form \"LOWER to UPPER\".")
		return()
	}
	
	addLogComment("Set region for analysis")
	guiDo(call=bquote(
		hsp$region <- list(xlim=as.numeric(.(myXStrings)), 
			ylim=as.numeric(.(myYStrings)))
	))
	setStatusBar("Set region for analysis: X = ", myXText, 
		", Y = ", myYText)
	
	regionModificationUpdate()
}

.hs_on_scope_reset_region_button_clicked <- function(button) {
	StateEnv$win$setSensitive(F)
	on.exit(StateEnv$win$setSensitive(T))
	setStatusBar("")
	
	addToLog("\n")
	guiDo(hsp$region <- NULL)
	
	regionModificationUpdate()
}


.hs_on_timeperiod_viewtimeline_button_clicked <- function(button) {
	StateEnv$win$setSensitive(F)
	on.exit(StateEnv$win$setSensitive(T))
	setStatusBar("")
	
	plotQualCodes <- theWidget("timeperiod_plotqualitycodes_checkbutton")$getActive()
	
	addLogComment("Generate timeline plot")
	
	plot.call <- call('grid.timeline.plot')
	plot.call[[2]] <- quote(hsp$data)
	plot.call$xscale <- quote(hsp$timePeriod)
	plot.call$colMap <- if (!plotQualCodes) { NA }
	
	addToLog(paste(deparse(plot.call), collapse="\n"))
	guiDo(plotAndPlay(plot.call=plot.call, name="timeline", 
		buttons=hydrosanityButtons[c('zoomin','zoomout','centre','setperiod')],
		extra.buttons=NULL, eval.args="^tmp"), doLog=F)
	
	setStatusBar("Generated timeline plot")
}

.hs_on_scope_viewsitemap_button_clicked <- function(button) {
	StateEnv$win$setSensitive(F)
	on.exit(StateEnv$win$setSensitive(T))
	setStatusBar("")
	
	doInterpElev <- theWidget("scope_sitemap_elevation_checkbutton")$getActive()
	doRainOnly <- theWidget("scope_sitemap_rainonly_checkbutton")$getActive()
	
	selNames <- names(hsp$data)
	
	if (doRainOnly) {
		role <- sapply(hsp$data, attr, "role")
		selNames <- names(hsp$data)[role=="RAIN"]
	}
	
	loc <- lapply(hsp$data[selNames], attr, "location.xy")
	ok <- (sapply(loc, length) == 2)
	
	if (any(!ok)) {
		infoDialog(paste("Some items do not have a valid 'location.xy' attribute:",
			paste(selNames[!ok], collapse=", "),
			". You can fix them with 'edit metadata' in the 'Dataset' tab."))
	}
	
	selNames <- selNames[ok]
	
	if (length(selNames) < 4) {
		# need at least 4 locations to interpolate
		doInterpElev <- FALSE
	}
	
	addLogComment("Generate site map")
	
	tmpObjs <- c('tmp.names', 'tmp.locs')
	guiDo(call=bquote({
		tmp.names <- .(selNames)
		tmp.locs <- sapply(hsp$data[tmp.names], attr, "location.xy")
		tmp.locs <- data.frame(x=tmp.locs[1,], y=tmp.locs[2,])
	}))
	
	plot.call <- quote(
		xyplot(y ~ x, tmp.locs, aspect="iso", points.labels=rownames(tmp.locs))
	)
	if (doInterpElev) {
		tmpObjs <- c(tmpObjs, 'tmp.elev')
		guiDo({
			tmp.elev <- lapply(hsp$data[tmp.names], attr, "elevation")
			tmp.elev <- unlist(ifelse(sapply(tmp.elev,is.null),NA,tmp.elev))
		})
		plot.call$z <- quote(tmp.elev)
	}
	plot.call$panel <- quote(panel.pointmap)
	plot.call$prepanel <- quote(prepanel.extend.10)
	
	addToLog(paste(deparse(plot.call), collapse="\n"))
	guiDo(plotAndPlay(plot.call=plot.call, name="site map", 
		extra.buttons=NULL,
		eval.args="^tmp"), doLog=F)
	
	if (length(tmpObjs) > 0) {
		guiDo(call=bquote(rm(list=.(tmpObjs))))
	}
	
	setStatusBar("Generated site map")
}

## NON-ACTIONS, just interface bits and pieces

.hs_on_timeperiod_chosenperiod_entry_changed <- function(widget) {
	theWidget("timeperiod_updateperiod_button")$setSensitive(TRUE)
}

.hs_on_scope_region_entry_changed <- function(widget) {
	theWidget("scope_set_period_button")$setSensitive(TRUE)
}

.hs_on_scope_sitearchive_type_combobox_changed <- function(widget) {
	isArchive <- (theWidget("scope_sitearchive_type_combobox")$getActive() == 0)
	if (!isArchive) {
		infoDialog("To choose the folder, select one file inside the folder.")
	}
}

