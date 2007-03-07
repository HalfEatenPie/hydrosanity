## HydroSanity: an interface for exploring hydrological time series in R
##
## Time-stamp: <2007-03-05 00:00:00 Felix>
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

updateTimePeriodPage <- function() {
	if (length(hsp$data) == 0) { return() }
	# overall time period
	wholePeriod <- c(start.timeblob(hsp$data), end.timeblob(hsp$data))
	wholePeriodString <- sprintf("%s to %s", 
		format(wholePeriod[1]), format(wholePeriod[2]))
	theWidget("timeperiod_overallperiod_entry")$setText(wholePeriodString)
	
	# time period for analysis
	doUpdateTimePeriod <- F
	if (is.null(hsp$timePeriod)) {
		hsp$timePeriod <<- wholePeriod
		doUpdateTimePeriod <- T
	}
	chosenPeriodString <- sprintf("%s to %s", 
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
	mySummary <- capture.output(summary(hsp$data[[1]][,2]))
	addTextview(TV, paste(mySummary, collapse="\n"), "\n")
	
	theWidget("hs_window")$present()
}

on_timeperiod_viewtimeline_button_clicked <- function(button) {
	theWidget("hs_window")$setSensitive(F)
	on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	# generate timeline image
	
	setPlotDevice("timeline")
	
	plotQualCodes <- theWidget("timeperiod_plotqualitycodes_checkbutton")$getActive()
	colMapText <- theWidget("timeperiod_colmap_entry")$getText()
	plot.cmd <- sprintf('timelineplot(hsp$data, timeStart="%s", timeEnd="%s", plotQualCodes=%s%s)',
		format(hsp$timePeriod[1]), format(hsp$timePeriod[2]),
		ifelse(plotQualCodes,'T','F'),
		ifelse(plotQualCodes, paste(', colMap=', colMapText, sep=''), '')
	)
	
	addLogItem("View timeline plot", plot.cmd)
	result <- guiTryEval(plot.cmd)
	if (inherits(result, "try-error")) { return() }
	setStatusBar("Generated timeline plot")
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
	update.cmd <- sprintf('hsp$timePeriod <<- c(as.POSIXct("%s"), as.POSIXct("%s"))',
		myTimeStrings[1], myTimeStrings[2])
	
	addLogItem("Set time period for analysis", update.cmd)
	result <- guiTryEval(update.cmd)
	if (inherits(result, "try-error")) { return() }
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

## NON-ACTIONS, just interface bits and pieces

on_timeperiod_chosenperiod_entry_changed <- function(widget) {
	theWidget("timeperiod_updateperiod_button")$setSensitive(TRUE)
}

