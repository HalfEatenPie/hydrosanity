## Hydrosanity: an interface for exploring hydrological time series in R
##
## Time-stamp: <2007-03-05 00:00:00 Felix>
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

updateExplorePage <- function() {
	
	rainPixbuf <- gdkPixbufNewFromFile(getpackagefile("icon_RAIN.png"))$retval
	flowPixbuf <- gdkPixbufNewFromFile(getpackagefile("icon_FLOW.png"))$retval
	otherPixbuf <- gdkPixbufNewFromFile(getpackagefile("icon_OTHER.png"))$retval
	# (or NULL)
	
	#dfModel <- rGtkDataFrame(as.data.frame(names(hsp$data)))
	#dfModel$appendColumns(myPixbuf)
	
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
	theWidget("explore_iconview")$setModel(list_store)
	theWidget("explore_iconview")$setTextColumn(0)
	theWidget("explore_iconview")$setPixbufColumn(1)
	theWidget("explore_iconview")$setItemWidth(50)	
	theWidget("explore_iconview")$selectAll()
	
	.hydrosanity$update$explore <<- F
	theWidget("hs_window")$present()
}

on_explore_timeseries_button_clicked <- function(button) {
	theWidget("hs_window")$setSensitive(F)
	on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	selNames <- iconViewGetSelectedNames(theWidget("explore_iconview"))
	if (length(selNames) == 0) {
		errorDialog("No items selected.")
		return()
	}
	myN <- length(selNames)
	rawdata.cmd <- paste('hsp$data[c("', paste(selNames, collapse='", "'), '")]', sep='')
	if (myN == 1) { rawdata.cmd <- paste('hsp$data["', selNames, '"]', sep='') }
	if (myN == length(hsp$data)) { rawdata.cmd <- 'hsp$data' }
	
	doLog <- theWidget("explore_timeseries_log_checkbutton")$getActive()
	doCommonScale <- theWidget("explore_timeseries_commonscale_radiobutton")$getActive()
	doSuperpose <- theWidget("explore_timeseries_superpose_radiobutton")$getActive()
	doRawData <- theWidget("explore_timeseries_rawdata_checkbutton")$getActive()
	doAggr1 <- theWidget("explore_timeseries_aggr1_checkbutton")$getActive()
	doAggr2 <- theWidget("explore_timeseries_aggr2_checkbutton")$getActive()
	aggr1By <- theWidget("explore_timeseries_aggr1_comboboxentry")$getActiveText()
	aggr2By <- theWidget("explore_timeseries_aggr2_comboboxentry")$getActiveText()
	myOptions <- theWidget("explore_timeseries_options_entry")$getText()
	myM <- (doRawData + doAggr1 + doAggr2)
	if (myM == 0) { return() }
	
	addLogComment("Generate timeseries plot")
	
	tmpObjs <- c()
	
	# compute and store aggregated series
	if (doAggr1 || doAggr2) {
		addToLog("## Compute and store aggregated series")
	}
	if (doAggr1) {
		tmpObjs <- c(tmpObjs, 'tmp.aggr1')
		pre_plot.cmd <- sprintf(
			'tmp.aggr1 <<- lapply(%s, aggregate.timeblob, by="%s")',
			rawdata.cmd, aggr1By)
		result <- guiTryEval(pre_plot.cmd)
		if (inherits(result, "error")) { return() }
	}
	if (doAggr2) {
		tmpObjs <- c(tmpObjs, 'tmp.aggr2')
		pre_plot.cmd <- sprintf(
			'tmp.aggr2 <<- lapply(%s, aggregate.timeblob, by="%s")',
			rawdata.cmd, aggr2By)
		result <- guiTryEval(pre_plot.cmd)
		if (inherits(result, "error")) { return() }
	}
	
	# store data specifications for each superposed plot series in order
	data.cmd <- c()
	dataBits <- c(
		if(doRawData){rawdata.cmd},
		if(doAggr1){'tmp.aggr1'},
		if(doAggr2){'tmp.aggr2'})
	all_data.cmd <- paste('c(',paste(dataBits,collapse=', '),')',sep='')
	if (length(dataBits) == 1) { all_data.cmd <- dataBits }
	
	if (doSuperpose) {
		# superpose blobs and transforms all in one plot
		data.cmd <- c(
			if(doRawData){paste('hsp$data["',selNames,'"]',sep='')},
			if(doAggr1){paste('tmp.aggr1["',selNames,'"]',sep='')},
			if(doAggr2){paste('tmp.aggr2["',selNames,'"]',sep='')})
		# simpler code if only one blob (same effect)
		if (myN == 1) {
			data.cmd <- dataBits
		}
	} else {
		# do multiple plots
		if (myN == 1) {
			# multiple plots for transforms
			data.cmd <- all_data.cmd
		} else {
			# multiple plots for blobs, but superpose transforms
			data.cmd <- dataBits
		}
	}

	# set up common y scale for superposed blobs
	yscale.cmd <- ''
	if (doCommonScale && doSuperpose) {
		addToLog("## Find global range of data")
		tmpObjs <- c(tmpObjs, 'tmp.yscale')
		makescale.cmd <- sprintf(
			'tmp.yscale <<- range(sapply(lapply(%s, window.timeblob, hsp$timePeriod[1], hsp$timePeriod[2]), range.timeblob, na.rm=T))',
			all_data.cmd)
		if (doLog) {
			addToLog("## and limit by minimum non-zero value (for log scale)")
			makescale.cmd <- paste(makescale.cmd, "\n", sprintf(
			'tmp.yscale[1] <<- min(sapply(%s, function(x){min(x[,2][x[,2]>0], na.rm=T)}))',
				all_data.cmd), sep='')
		}
		result <- guiTryEval(makescale.cmd)
		if (inherits(result, "error")) { return() }
		yscale.cmd <- ', yscale=tmp.yscale'
	}
	
	samescale.cmd <- if (doCommonScale) { '' } else { ', sameScales=F' }
	log.cmd <- if (doLog) { ', logScale=T' } else { '' }
	
	setPlotDevice("explore")
	
	# do initial plot (may be multiple plots, but none superposed)
	addToLog("## Plot")
	plot.cmd <- sprintf('grid.timeseries.plot(%s, %s%s%s%s)',
		data.cmd[1], 'xscale=hsp$timePeriod', yscale.cmd, samescale.cmd, log.cmd)
	result <- guiTryEval(plot.cmd)
	if (inherits(result, "error")) { return() }
	
	# from lattice show.settings(): superpose.line$col
	colSet <- c("#0080ff", "#ff00ff", "darkgreen", "#ff0000", "orange", "#00ff00", "brown")
	
	# plot superposed series
	for (i in seq(along=data.cmd)) {
		if (i==1) { next }
		newscale.cmd <- ''
		if (doCommonScale && doSuperpose) {
			newscale.cmd <- ', newScale=F'
		}
		superpose.cmd <- sprintf(
			'grid.timeseries.plot(%s%s%s%s, superPos=%i, gp=gpar(col="%s"))', 
			data.cmd[i], newscale.cmd, samescale.cmd, log.cmd, i, colSet[i])
		result <- guiTryEval(superpose.cmd)
		if (inherits(result, "error")) { return() }
	}
	
	if (length(tmpObjs) > 0) {
		guiTryEval(paste('rm(', paste(tmpObjs, collapse=', '), ')', sep=''))
	}
	setStatusBar("Generated timeseries plot")
}

on_explore_cdf_button_clicked <- function(button) {
	theWidget("hs_window")$setSensitive(F)
	on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	selNames <- iconViewGetSelectedNames(theWidget("explore_iconview"))
	if (length(selNames) == 0) {
		errorDialog("No items selected.")
		return()
	}
	mySelNames <- paste('c("', paste(selNames, collapse='", "'), '")', sep='')
	
	setPlotDevice("explore")
	plot.cmd <- sprintf('fdcplot(hsp$data[%s], timelim=hsp$timePeriod, plotQualCodes=T)',
		mySelNames)
	
	addLogComment("Generate CDF plot")
	result <- guiTryEval(plot.cmd)
	if (inherits(result, "error")) { return() }
	setStatusBar("Generated CDF plot")
}

on_explore_seasonal_button_clicked <- function(button) {
	theWidget("hs_window")$setSensitive(F)
	on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	selNames <- iconViewGetSelectedNames(theWidget("explore_iconview"))
	
	setPlotDevice("explore")
	monthlyboxplot(hsp$data[[selNames[1]]])
}

