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
	
	log.cmd <- if (doLog) { ', logScale=T' } else { '' }
	samescale.cmd <- if (doCommonScale) { '' } else { ', sameScales=F' }
	newscale.cmd <- if (doCommonScale && doSuperpose) { ', newScale=F' } else { '' }
	
	setPlotDevice("timeseries")
	
	# do initial plot (may be multiple plots, but none superposed)
	addToLog("## Plot")
	if (length(data.cmd) == 1) {
		plot.cmd <- sprintf('grid.timeseries.plot(%s, %s%s%s%s)',
			data.cmd[1], 'xscale=hsp$timePeriod', yscale.cmd, 
			samescale.cmd, log.cmd)
	} else {
		plot.cmd <- sprintf('grid.timeseries.plot.superpose(list(%s), %s%s%s%s)',
			paste(data.cmd, collapse=", "),
			'xscale=hsp$timePeriod', yscale.cmd, 
			samescale.cmd, log.cmd, newscale.cmd)
	}
	
	result <- guiTryEval(plot.cmd)
	if (inherits(result, "error")) { return() }
	
	tmpCall <- parse(text=plot.cmd)[[1]]
	.hydrosanity$call[["timeseries"]] <<- evalCallArgs(tmpCall, pattern="^tmp")
	
	if (length(tmpObjs) > 0) {
		addToLog(paste('rm(', paste(tmpObjs, collapse=', '), ')', sep=''))
		rm(list=tmpObjs, envir=.GlobalEnv)
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
	myN <- length(selNames)
	rawdata.cmd <- paste('hsp$data[["', selNames, '"]][,2]', sep='')
	rawdata.formula.cmd <- paste(rawdata.cmd, collapse=" + ")
	
	doNormal <- theWidget("explore_cdf_normal_radiobutton")$getActive()
	
	dist.cmd <- if (doNormal) { '' } else { ', distribution=qunif' }
	
	addLogComment("Generate CDF plot")
	
	setPlotDevice("distribution")
	
	#title(xlab="Probability of exceedence (%)")
	#axis(2, las=2, at = c(0.1, 1, 10, 100, 1000, 10^4, 10^5, 10^6), labels = c("0.1", "1", "10", "100", "1000", "10^4", "10^5", "10^6"))
	#axis(1, at=probFn(c(0.001, 0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.99, 0.999)), 
	#	labels=c("0.1", "1", "10", "20", "30", "40", "50", "60", "70", "80", "90", "99", "99.9"))
		
	plot.cmd <- sprintf('qqmath(~ %s%s, ylim=c(0.1, 1000), scales=list(y=list(log=T)), auto.key=T)',
		rawdata.formula.cmd, dist.cmd)
	
	result <- guiTryEval(plot.cmd)
	if (inherits(result, "error")) { return() }
	.hydrosanity$call[["distribution"]] <<- parse(text=plot.cmd)[[1]]
	setStatusBar("Generated CDF plot")
}

on_explore_seasonal_button_clicked <- function(button) {
	theWidget("hs_window")$setSensitive(F)
	on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	selNames <- iconViewGetSelectedNames(theWidget("explore_iconview"))
	
	setPlotDevice("explore")
	
	
	#monthBlob <- aggregate.timeblob(myBlob, by="months", aggrFun=aggrFun)
	#monthBlob[,2] <- pmax(monthBlob[,2], zeroLevel/10) # 0 values would be lost by log transform
	##myMonths <- months(monthBlob$Time, abbreviate=TRUE)
	#monthNums <- sapply(monthBlob$Time, function(x) { as.POSIXlt(as.POSIXct.raw(x))$mon })
	#monthList <- split(monthBlob[,2], monthNums)
	#names(monthList) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
	#globalMax <- max(monthBlob[,2], na.rm=T)
	#boxplot(monthList, ylim=c(zeroLevel, globalMax), range=0, log=log, ...)
	
	#monthlyboxplot(hsp$data[[selNames[1]]])
}

