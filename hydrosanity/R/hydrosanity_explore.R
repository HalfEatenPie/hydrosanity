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

.hs_on_explore_timeseries_button_clicked <- function(button) {
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
	yearStartMonthNum <- theWidget("explore_timeseries_yearstart_combobox")$getActive()
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
			'tmp.yscale <<- range(sapply.timeblob.data(lapply(%s, window.timeblob, hsp$timePeriod[1], hsp$timePeriod[2]), range, na.rm=T))',
			all_data.cmd)
		if (doLog) {
			addToLog("## and limit by minimum non-zero value (for log scale)")
			makescale.cmd <- paste(makescale.cmd, "\n", sprintf(
			'tmp.yscale[1] <<- min(sapply.timeblob.data(%s, function(x){ min(x[x>0], na.rm=T) }))',
				all_data.cmd), sep='')
		}
		result <- guiTryEval(makescale.cmd)
		if (inherits(result, "error")) { return() }
		yscale.cmd <- ', yscale=tmp.yscale'
	}
	
	log.cmd <- if (doLog) { ', logScale=T' } else {''}
	samescale.cmd <- if (doCommonScale) {''} else { ', sameScales=F' }
	newscale.cmd <- if (doCommonScale && doSuperpose) { ', newScale=F' } else {''}
	
	setPlotDevice("timeseries")
	.hydrosanity$call[["timeseries"]] <<- NULL
	.hydrosanity$win.gui[["timeseries"]]$getWidget("plot_log_togglebutton")$setActive(doLog)
	
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

.hs_on_explore_cdf_button_clicked <- function(button) {
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
	
	doNormal <- theWidget("explore_cdf_normal_radiobutton")$getActive()
	doUniform <- theWidget("explore_cdf_uniform_radiobutton")$getActive()
	doCDF <- doNormal || doUniform
	doLine <- theWidget("explore_cdf_line_checkbutton")$getActive()
	doBoxPlot <- theWidget("explore_cdf_bwplot_radiobutton")$getActive()
	doStripPlot <- theWidget("explore_cdf_stripplot_radiobutton")$getActive()
	doViolinPlot <- theWidget("explore_cdf_violinplot_radiobutton")$getActive()
	doRawData <- theWidget("explore_cdf_rawdata_radiobutton")$getActive()
	doAggr1 <- theWidget("explore_cdf_aggr1_radiobutton")$getActive()
	doAggr2 <- theWidget("explore_cdf_aggr2_radiobutton")$getActive()
	aggr1By <- theWidget("explore_cdf_aggr1_comboboxentry")$getActiveText()
	aggr2By <- theWidget("explore_cdf_aggr2_comboboxentry")$getActiveText()
	
	addLogComment("Generate distribution plot")
	
	tmpObjs <- c('tmp.data')
	
	# initalise data
	cmd <- sprintf('tmp.data <<- %s', rawdata.cmd)
	if (inherits(guiTryEval(cmd), "error")) { return() }
	
	# apply time period window
	if (!is.null(hsp$timePeriod)) {
		if (inherits(guiTryEval(
			'tmp.data <<- lapply(tmp.data, window, hsp$timePeriod[1], hsp$timePeriod[2])'
		), "error")) { return() }
	}
	
	# compute and store aggregated series
	if (doAggr1 || doAggr2) {
		cmd <- sprintf(
			'tmp.data <<- lapply(tmp.data, aggregate.timeblob, by="%s")', 
			if (doAggr1) { aggr1By } else { aggr2By }
		)
		if (inherits(guiTryEval(cmd), "error")) { return() }
	}
	
	# make.groups
	tmpObjs <- c(tmpObjs, 'tmp.groups')
	cmd <- 'tmp.groups <<- do.call(make.groups, lapply(tmp.data, function(x) x$Data ))'
	if (inherits(guiTryEval(cmd), "error")) { return() }
	
	# plot type specifications
	
	plotfn.cmd <- if (doCDF) { 'qqmath' } else 
		if (doStripPlot) { 'stripplot' } else { 'bwplot' }
	formula.cmd <- if (doCDF) { '~ data, groups=which' } else
		{ 'data ~ which' }
	dist.cmd <- if (doNormal) { ', distribution=qnorm' } else 
		if (doUniform) { ', distribution=qunif' } else {''}
	panel.cmd <- if (doCDF && doLine) {
		', panel=function(...) { panel.qqmathline(...); panel.qqmath(...) }'
		} else if (doViolinPlot) {
		', panel=function(...){ panel.violin(varwidth=T, ...); panel.stripplot(pch=3, ...) }'
		} else {''}
	prepanel.cmd <- if (doCDF) { ', prepanel=prepanel.qqmath.fix' } else {''}
	jitter.cmd <- if (doStripPlot) { ', jitter=T' } else {''}
	
	if (doCDF) {
		tmpObjs <- c(tmpObjs, 'tmp.probs')
		if (inherits(guiTryEval(
			'tmp.probs <<- c(0.01, 0.1, 0.3, 0.5, 0.7, 0.9, 0.99)'
		), "error")) { return() }
	}
	
	# scale and annotation specifications
	
	xscale.cmd <- if (doCDF) { sprintf(
		'x=list(at=%s(tmp.probs), labels=tmp.probs*100), ',
			if (doNormal) { 'qnorm' } else {'c'} )
		} else { '' }
	yscale.cmd <- 'y=list(log=T)'
	xlab.cmd <- if (doCDF) { ', xlab="Probability of non-exceedence (%)"' } else {''}
	ylab.cmd <- sprintf(', ylab="%s"', attr(hsp$data[[selNames[1]]], "dataname"))
	
	# hydrosanity caption
	tmpObjs <- c(tmpObjs, 'tmp.timelim', 'tmp.n')
	if (inherits(guiTryEval(
		'tmp.timelim <<- c(start.timeblobs(tmp.data), end.timeblobs(tmp.data))'
	), "error")) { return() }
	if (inherits(guiTryEval(
		'tmp.n <<- sum(sapply.timeblob.data(tmp.data, is.na)==F)'
	), "error")) { return() }
	sub.cmd <- sprintf(', sub=hydrosanity.caption(%s, by="%s", n=%s, series=%s)',
		'tmp.timelim', attr(tmp.data[[1]], "timestep"), 'tmp.n', myN)
	
	setPlotDevice("distribution")
	.hydrosanity$call[["distribution"]] <<- NULL
	.hydrosanity$win.gui[["distribution"]]$getWidget("plot_log_togglebutton")$setActive(T)
	
	plot.cmd <- sprintf('%s(%s, data=tmp.groups%s%s%s, scales=list(%s%s)%s%s%s, yscale.components=lattice.y.prettylog%s, auto.key=T)',
		plotfn.cmd, formula.cmd, dist.cmd, panel.cmd, jitter.cmd, xscale.cmd, yscale.cmd, xlab.cmd, ylab.cmd, sub.cmd, prepanel.cmd)
	
	result <- guiTryEval(plot.cmd)
	if (inherits(result, "error")) { return() }
	print(result) # plot trellis object
	
	tmpCall <- parse(text=plot.cmd)[[1]]
	.hydrosanity$call[["distribution"]] <<- evalCallArgs(tmpCall, pattern="^tmp")
	
	if (length(tmpObjs) > 0) {
		addToLog(paste('rm(', paste(tmpObjs, collapse=', '), ')', sep=''))
		rm(list=tmpObjs, envir=.GlobalEnv)
	}
	setStatusBar("Generated CDF plot")
}

.hs_on_explore_seasonal_button_clicked <- function(button) {
	theWidget("hs_window")$setSensitive(F)
	on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	selNames <- iconViewGetSelectedNames(theWidget("explore_iconview"))
	myN <- length(selNames)
	rawdata.cmd <- paste('hsp$data[c("', paste(selNames, collapse='", "'), '")]', sep='')
	if (myN == 1) { rawdata.cmd <- paste('hsp$data["', selNames, '"]', sep='') }
	if (myN == length(hsp$data)) { rawdata.cmd <- 'hsp$data' }
	
	doMonths <- theWidget("explore_seasonal_months_radiobutton")$getActive()
	doBoxPlot <- theWidget("explore_seasonal_bwplot_radiobutton")$getActive()
	doStripPlot <- theWidget("explore_seasonal_stripplot_radiobutton")$getActive()
	doViolinPlot <- theWidget("explore_seasonal_violinplot_radiobutton")$getActive()
	
	addLogComment("Generate seasonal plot")
	
	tmpObjs <- c('tmp.data')
	
	# initalise data
	cmd <- sprintf('tmp.data <<- %s', rawdata.cmd)
	if (inherits(guiTryEval(cmd), "error")) { return() }
	
	cmd <- ''
	if (doMonths) {
		cmd <- paste(sep='\n', 
			'tmp.data <<- sync.timeblobs(lapply(tmp.data, aggregate.timeblob, by="months"), timelim=hsp$timePeriod)',
			'tmp.data$Season <<- factor(months(tmp.data$Time, abbreviate=TRUE),',
			'	levels=c("Jan","Feb","Mar","Apr","May","Jun",',
			'	"Jul","Aug","Sep","Oct","Nov","Dec"), ordered=T)')
	} else {
		cmd <- paste(sep='\n', 
			'tmp.data <<- lapply(tmp.data, function(x) {',
			'	window(x, trunc.year(start(x)), end(x), extend=T) })',
			'tmp.data <<- sync.timeblobs(lapply(tmp.data, aggregate.timeblob, by="3 months"), timelim=hsp$timePeriod)',
			'tmp.data$Season <<- factor(quarters(tmp.data$Time), ordered=T)')
	}
	if (inherits(guiTryEval(cmd), "error")) { return() }
	
	data.formula.cmd <- paste(names(tmp.data)[-c(1,ncol(tmp.data))], collapse=" + ")
	plotfn.cmd <- if (doStripPlot) { 'stripplot' } else { 'bwplot' }
	panel.cmd <- if (doViolinPlot) {
		', panel=function(...){ panel.violin(varwidth=T, ...); panel.stripplot(pch=3, ...) }'
	} else {''}
	jitter.cmd <- if (doStripPlot) { ', jitter=T' } else {''}
	layout.cmd <- if (doMonths) { sprintf(', layout=c(1, %i)', myN) } else {''}
	ylab.cmd <- sprintf(', ylab="%s"', attr(hsp$data[[selNames[1]]], "dataname"))
	
	# hydrosanity caption
	tmpObjs <- c(tmpObjs, 'tmp.timelim', 'tmp.n')
	if (inherits(guiTryEval(
		'tmp.timelim <<- range(tmp.data$Time)' # TODO: should add timestep to this
	), "error")) { return() }
	if (inherits(guiTryEval(
		sprintf('tmp.n <<- sum(sapply(tmp.data[2:%i], is.na)==F)', myN+1)
	), "error")) { return() }
	sub.cmd <- sprintf(', sub=hydrosanity.caption(%s, by="%s", n=%s, series=%s)',
		'tmp.timelim', attr(tmp.data, "timestep"), 'tmp.n', myN)
	
	setPlotDevice("seasonality")
	.hydrosanity$call[["seasonality"]] <<- NULL
	.hydrosanity$win.gui[["seasonality"]]$getWidget("plot_log_togglebutton")$setActive(T)
	
	plot.cmd <- sprintf('%s(%s ~ Season, tmp.data, outer=T%s%s%s, scales=list(y=list(log=T))%s%s, yscale.components=lattice.y.prettylog)',
		plotfn.cmd, data.formula.cmd, panel.cmd, layout.cmd, jitter.cmd, ylab.cmd, sub.cmd)
	result <- guiTryEval(plot.cmd)
	if (inherits(result, "error")) { return() }
	print(result) # plot trellis object
	
	tmpCall <- parse(text=plot.cmd)[[1]]
	.hydrosanity$call[["seasonality"]] <<- evalCallArgs(tmpCall, pattern="^tmp")
	
	if (length(tmpObjs) > 0) {
		addToLog(paste('rm(', paste(tmpObjs, collapse=', '), ')', sep=''))
		rm(list=tmpObjs, envir=.GlobalEnv)
	}
	setStatusBar("Generated seasonal plot")
}

