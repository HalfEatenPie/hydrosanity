## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

updateTimePeriodPage <- function() {
	TXV <- theWidget("timeperiod_summary_textview")
	TRV <- theWidget("timeperiod_summary_treeview")
	if (length(hsp$data) == 0) {
		setTextview(TXV, "")
		TRV$setModel(rGtkDataFrame())
		return()
	}
	# overall time period
	wholePeriod <- c(start.timeblobs(hsp$data), end.timeblobs(hsp$data))
	wholePeriodString <- sprintf('%s to %s', 
		format(wholePeriod[1]), format(wholePeriod[2]))
	theWidget("timeperiod_overallperiod_entry")$setText(wholePeriodString)
	
	# time period for analysis
	chosenPeriodString <- wholePeriodString
	theWidget("timeperiod_updateperiod_button")$setSensitive(TRUE)
	if (!is.null(hsp$timePeriod)) {
		chosenPeriodString <- sprintf('%s to %s', 
			format(hsp$timePeriod[1]), format(hsp$timePeriod[2]))
		theWidget("timeperiod_updateperiod_button")$setSensitive(FALSE)
	}
	theWidget("timeperiod_chosenperiod_entry")$setText(chosenPeriodString)
	
	setTextview(TXV, "")
	TRV$setModel(rGtkDataFrame())
	
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
	
	.hydrosanity$update$timeperiod <<- F
	theWidget(APPWIN)$present()
}


.hs_on_timeperiod_updateperiod_button_clicked <- function(button) {
	theWidget(APPWIN)$setSensitive(F)
	on.exit(theWidget(APPWIN)$setSensitive(T))
	setStatusBar("")
	
	myText <- theWidget("timeperiod_chosenperiod_entry")$getText()
	myTimeStrings <- strsplit(myText, " to ")[[1]]
	if (length(myTimeStrings) != 2) {
		errorDialog("Give time period in form \"START to END\".")
		return()
	}
	addLogComment("Set time period for analysis")
	guiDo(isExpression=T, bquote(
		hsp$timePeriod <- as.POSIXct(.(myTimeStrings))
	))
	setStatusBar("Set time period for analysis:", myText)
	
	timeperiodModificationUpdate()
}

.hs_on_timeperiod_reset_button_clicked <- function(button) {
	theWidget(APPWIN)$setSensitive(F)
	on.exit(theWidget(APPWIN)$setSensitive(T))
	setStatusBar("")
	
	guiDo(hsp$timePeriod <- NULL)
	
	timeperiodModificationUpdate()
}


.hs_on_timeperiod_viewtimeline_button_clicked <- function(button) {
	theWidget(APPWIN)$setSensitive(F)
	on.exit(theWidget(APPWIN)$setSensitive(T))
	setStatusBar("")
	
	plotQualCodes <- theWidget("timeperiod_plotqualitycodes_checkbutton")$getActive()
	
	addLogComment("Generate timeline plot")
	
	plot.cmd <- call('grid.timeline.plot')
	plot.cmd[[2]] <- quote(hsp$data)
	plot.cmd$xscale <- quote(hsp$timePeriod)
	plot.cmd$colMap <- if (!plotQualCodes) { NA }
	
	setPlotDevice("timeline")
	setCairoWindowButtons("timeline", centre=T, zoomin=T, setperiod=T)
	
	guiDo(plot.cmd, isExpression=T)
	.hydrosanity$call[["timeline"]] <<- plot.cmd
	
	setStatusBar("Generated timeline plot")
}


## NON-ACTIONS, just interface bits and pieces

.hs_on_timeperiod_chosenperiod_entry_changed <- function(widget) {
	theWidget("timeperiod_updateperiod_button")$setSensitive(TRUE)
}

