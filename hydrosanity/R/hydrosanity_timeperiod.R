## Hydrosanity: an interface for exploring hydrological time series in R
##
## Time-stamp: <2007-03-05 00:00:00 Felix>
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

updateTimePeriodPage <- function() {
	if (length(hsp$data) == 0) {
		setTextview("timeperiod_summary_textview", "")
		return()
	}
	# overall time period
	wholePeriod <- c(start.timeblobs(hsp$data), end.timeblobs(hsp$data))
	wholePeriodString <- sprintf('%s to %s', 
		format(wholePeriod[1]), format(wholePeriod[2]))
	theWidget("timeperiod_overallperiod_entry")$setText(wholePeriodString)
	
	# time period for analysis
	doUpdateTimePeriod <- F
	if (is.null(hsp$timePeriod)) {
		hsp$timePeriod <<- wholePeriod
		doUpdateTimePeriod <- T
	}
	chosenPeriodString <- sprintf('%s to %s', 
		format(hsp$timePeriod[1]), format(hsp$timePeriod[2]))
	theWidget("timeperiod_chosenperiod_entry")$setText(chosenPeriodString)
	theWidget("timeperiod_updateperiod_button")$setSensitive(FALSE)
	
	if (doUpdateTimePeriod) {
		on_timeperiod_updateperiod_button_clicked()
		return()
	}
	
	# generate summary
	TV <- "timeperiod_summary_textview"
	setTextview(TV, "")
	
	summmary.cmd <- sprintf('summary.missing.timeblob.list(hsp$data, timelim=hsp$timePeriod)')
	addToLog("## View summary of data coverage in selected period")
	missingSummary <- capture.output(
		missingFrac <- guiTryEval(summmary.cmd)
	)
	if (inherits(missingFrac, "error")) { return() }
	
	addTextview(TV, paste(missingSummary, collapse="\n"), "\n")
	
	dfName <- dfMin <- dfQ25 <- dfMedian <- dfQ75 <- dfMax <- dfMissing <- character(length(hsp$data))
	
	for (i in seq(along=hsp$data)) {
		dfName[i] <- names(hsp$data)[i]
		subBlob <- window.timeblob(hsp$data[[i]], hsp$timePeriod[1], hsp$timePeriod[2])
		myQuantiles <- format(quantile(subBlob[,2], probs=c(0, 0.25, 0.5, 0.75, 1), na.rm=T), digits=2)
		dfMin[i] <- myQuantiles[1]
		dfQ25[i] <- myQuantiles[2]
		dfMedian[i] <- myQuantiles[3]
		dfQ75[i] <- myQuantiles[4]
		dfMax[i] <- myQuantiles[5]
		dfMissing[i] <- sprintf('%.0f%%', missingFrac[i]*100)
		#sprintf('%.2f%%', sum(is.na(subBlob[,2])) / nrow(subBlob)) i.e. gaps
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
	myTreeView <- theWidget("timeperiod_summary_treeview")
	myTreeView$setModel(dfModel)
	
	.hydrosanity$update$timeperiod <<- F
	theWidget("hs_window")$present()
}


on_timeperiod_updateperiod_button_clicked <- function(button) {
	theWidget("hs_window")$setSensitive(F)
	on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	myText <- theWidget("timeperiod_chosenperiod_entry")$getText()
	myTimeStrings <- strsplit(myText, " to ")[[1]]
	if (length(myTimeStrings) != 2) {
		errorDialog("Give time period in form \"START to END\".")
		return()
	}
	update.cmd <- sprintf('hsp$timePeriod <<- as.POSIXct(c("%s", "%s"))',
		myTimeStrings[1], myTimeStrings[2])
	
	addLogComment("Set time period for analysis")
	result <- guiTryEval(update.cmd)
	if (inherits(result, "error")) { return() }
	setStatusBar("Set time period for analysis:", 
		myTimeStrings[1], "to", myTimeStrings[2])
	
	updateTimePeriodPage()
}

on_timeperiod_reset_button_clicked <- function(button) {
	theWidget("hs_window")$setSensitive(F)
	on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	hsp$timePeriod <<- NULL
	updateTimePeriodPage()
}


on_timeperiod_viewtimeline_button_clicked <- function(button) {
	theWidget("hs_window")$setSensitive(F)
	on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	# generate timeline image
	
	setPlotDevice("timeline")
	
	plotQualCodes <- theWidget("timeperiod_plotqualitycodes_checkbutton")$getActive()
	colMapText <- theWidget("timeperiod_colmap_entry")$getText()
	plot.cmd <- sprintf('grid.timeline.plot(hsp$data, xscale=hsp$timePeriod, colMap=%s)',
		if (plotQualCodes) { colMapText } else { 'NULL' }
	)
	
	addLogComment("Generate timeline plot")
	result <- guiTryEval(plot.cmd)
	if (inherits(result, "error")) { return() }
	.hydrosanity$call[["timeline"]] <<- parse(text=plot.cmd)[[1]]
	setStatusBar("Generated timeline plot")
}


## NON-ACTIONS, just interface bits and pieces

on_timeperiod_chosenperiod_entry_changed <- function(widget) {
	theWidget("timeperiod_updateperiod_button")$setSensitive(TRUE)
}

