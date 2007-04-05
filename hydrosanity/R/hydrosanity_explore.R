## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

updateExplorePage <- function() {
	
	setupIconView(theWidget("explore_iconview"))
	
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
	doCommonScale <- theWidget("explore_timeseries_commonscale_radiobutton")$getActive()
	doSuperpose <- theWidget("explore_timeseries_superpose_radiobutton")$getActive()
	doQual <- theWidget("explore_timeseries_qual_checkbutton")$getActive()
	doRawData <- theWidget("explore_timeseries_rawdata_checkbutton")$getActive()
	doAggr1 <- theWidget("explore_timeseries_aggr1_checkbutton")$getActive()
	doAggr2 <- theWidget("explore_timeseries_aggr2_checkbutton")$getActive()
	aggr1By <- theWidget("explore_timeseries_aggr1_comboboxentry")$getActiveText()
	aggr2By <- theWidget("explore_timeseries_aggr2_comboboxentry")$getActiveText()
	yearStartMonthNum <- theWidget("explore_timeseries_yearstart_combobox")$getActive()
	myM <- (doRawData + doAggr1 + doAggr2)
	if (myM == 0) { return() }
	
	addLogComment("Generate timeseries plot")
	
	rawdata.cmd <- if (myN == length(hsp$data)) {
		quote(hsp$data)
	} else {
		bquote(hsp$data[.(selNames)])
	}
	
	# note: we cannot apply time series window to the data here;
	#       need to keep whole dataset to allow interaction (zoom out, etc)
	
	# compute and store aggregated series
	if (doAggr1) {
		tmpObjs <- c(tmpObjs, 'tmp.aggr1')
		guiDo(isExpression=T, bquote(
			tmp.aggr1 <- lapply(.(rawdata.cmd), aggregate.timeblob, 
				by=.(aggr1By))
		))
	}
	if (doAggr2) {
		tmpObjs <- c(tmpObjs, 'tmp.aggr2')
		guiDo(isExpression=T, bquote(
			tmp.aggr2 <- lapply(.(rawdata.cmd), aggregate.timeblob, 
				by=.(aggr2By))
		))
	}
	
	# each item in dataList makes a timeseries plot, each superposed.
	# (so each item should specify a list of timeblobs)
	dataList <- list()
	if (doSuperpose) {
		# superpose blobs and aggregates all in one plot
		if (doRawData) {
			dataList <- c(dataList, lapply(selNames,
				function(name) {
				bquote(hsp$data[.(name)]) }
			))
		}
		if (doAggr1) {
			if (myN == 1) { # simplify code if only one
				dataList <- c(dataList, quote(tmp.aggr1))
			} else {
				dataList <- c(dataList, lapply(selNames,
					function(name) {
					bquote(tmp.aggr1[.(name)]) }
				))
			}
		}
		if (doAggr2) {
			if (myN == 1) { # simplify code if only one
				dataList <- c(dataList, quote(tmp.aggr2))
			} else {
				dataList <- c(dataList, lapply(selNames,
					function(name) {
					bquote(tmp.aggr2[.(name)]) }
				))
			}
		}
	} else {
		# do multiple plots
		if (myN == 1) {
			# multiple plots for transforms (all in one layer)
			if (myM == 1) {
				dataList[[1]] <- if (doRawData) { rawdata.cmd } else
					if (doAggr1) { quote(tmp.aggr1) } else
					if (doAggr2) { quote(tmp.aggr2) }
			} else {
				dataList[[1]] <- c(if (doRawData) { rawdata.cmd },
					if (doAggr1) { quote(tmp.aggr1) },
					if (doAggr2) { quote(tmp.aggr2) })
			}
		} else {
			# multiple plots for blobs, but superpose transforms
			dataList <- c(if (doRawData) { rawdata.cmd },
				if (doAggr1) { quote(tmp.aggr1) },
				if (doAggr2) { quote(tmp.aggr2) })
		}
	}
	
	# plot specifications
	plot.cmd <- NULL
	if (length(dataList) == 1) {
		plot.cmd <- call('grid.timeseries.plot')
		plot.cmd[[2]] <- dataList[[1]]
	} else {
		plot.cmd <- call('grid.timeseries.plot.superpose')
		plot.cmd[[2]] <- dataList
	}
	
	# plot scales and annotation specifications
	plot.cmd$xscale <- quote(hsp$timePeriod)
	plot.cmd$sameScales <- if (doCommonScale) { T } else { F }
	plot.cmd$sameScalesGlobal <- if (doCommonScale && doSuperpose
		&& (length(dataList) > 1)) { T }
	
	setPlotDevice("timeseries")
	setCairoWindowButtons("timeseries", centre=T, zoomin=T, setperiod=T, log=F)
	
	result <- guiDo(plot.cmd, isExpression=T)
	
	.hydrosanity$call[["timeseries"]] <<- evalCallArgs(plot.cmd, pattern="^tmp")
	
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
	if (myN == length(hsp$data)) {
		guiDo(tmp.data <- hsp$data)
	} else {
		guiDo(isExpression=T, bquote(
			tmp.data <- hsp$data[.(selNames)]
		))
	}
	
	# apply time period window
	if (!is.null(hsp$timePeriod)) {
		guiDo(tmp.data <- lapply(tmp.data, window, 
			hsp$timePeriod[1], hsp$timePeriod[2]))
	}
	
	# compute and store aggregated series
	if (doAggr1 || doAggr2) {
		aggrBy <- if (doAggr1) { aggr1By } else { aggr2By }
		guiDo(isExpression=T, bquote(
			tmp.data <- lapply(tmp.data, aggregate.timeblob, by=.(aggrBy))
		))
	}
	
	# make.groups
	tmpObjs <- c(tmpObjs, 'tmp.groups')
	guiDo(tmp.groups <- do.call(make.groups, 
		lapply(tmp.data, function(x) x$Data )))
	
	# plot specifications
	plotFn <- if (doCDF) { 'qqmath' } else 
		if (doStripPlot) { 'stripplot' } else { 'bwplot' }
	plot.cmd <- call(plotFn)
	plot.cmd[[2]] <- if (doCDF) { quote(~ data) } else { quote(data ~ which) }
	plot.cmd$groups <- if (doCDF) { quote(which) }
	plot.cmd$data <- quote(tmp.groups)
	plot.cmd$distribution <- if (doNormal) { quote(qnorm) } else
		if (doUniform) { quote(qunif) }
	plot.cmd$panel <- if (doCDF && doLine) {
			function(x, ...) {
				panel.qqmathline(x[is.finite(x)], ...)
				panel.qqmath(x, ...)
			}
		} else if (doViolinPlot) {
			function(...) {
				panel.violin(varwidth=T, ...)
				panel.stripplot(pch=3, ...)
			}
		}
	plot.cmd$jitter <- if (doStripPlot) { T }
	
	# plot scales and annotation specifications
	if (doCDF) {
		tmpObjs <- c(tmpObjs, 'tmp.probs')
		if (doNormal) {
			guiDo(tmp.probs <- c(0.001, 0.01, 0.1, 0.3, 0.5, 
				0.7, 0.9, 0.99, 0.999))
			plot.cmd$scales$x <- quote(list(
				at=qnorm(tmp.probs), labels=tmp.probs * 100))
		} else {
			guiDo(tmp.probs <- seq(0, 1, by=0.1))
			plot.cmd$scales$x <- quote(list(
				at=tmp.probs, labels=tmp.probs * 100))
		}
	}
	plot.cmd$scales$y$log <- T
	plot.cmd$yscale.components <- quote(lattice.y.prettylog)
	plot.cmd$xlab <- if (doCDF) { "Probability of non-exceedence (%)" }
	plot.cmd$ylab <- attr(hsp$data[[selNames[1]]], "dataname")
	plot.cmd$auto.key <- T
	
	# hydrosanity caption
	addToLog("## Make hydrosanity caption")
	tmpObjs <- c(tmpObjs, 'tmp.n', 'tmp.caption')
	guiDo(isExpression=T, bquote({
		tmp.n <- sum(unlist(lapply.timeblob.data(tmp.data, is.na))==F)
		tmp.caption <- hydrosanity.caption(
			c(start.timeblobs(tmp.data), end.timeblobs(tmp.data)), 
			by=.(attr(tmp.data[[1]], "timestep")), n=tmp.n, 
			series=.(myN))
	}))
	plot.cmd$sub <- quote(tmp.caption)
	plot.cmd$prepanel <- if (doCDF) { quote(prepanel.qqmath.fix) }
	
	setPlotDevice("distribution")
	setCairoWindowButtons("distribution", identify=T, log=T)
	
	result <- guiDo(plot.cmd, isExpression=T)
	# plot trellis object
	guiDo(bquote(print(.(result))), isExpression=T, doLog=F)
	
	.hydrosanity$call[["distribution"]] <<- evalCallArgs(plot.cmd, pattern="^tmp")
	
	if (length(tmpObjs) > 0) {
		addToLog(paste('rm(', paste(tmpObjs, collapse=', '), ')', sep=''))
		rm(list=tmpObjs, envir=.GlobalEnv)
	}
	setStatusBar("Generated distribution plot")
}

.hs_on_explore_seasonal_button_clicked <- function(button) {
	theWidget("hs_window")$setSensitive(F)
	on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	selNames <- iconViewGetSelectedNames(theWidget("explore_iconview"))
	myN <- length(selNames)
	doMonths <- theWidget("explore_seasonal_months_radiobutton")$getActive()
	doBoxPlot <- theWidget("explore_seasonal_bwplot_radiobutton")$getActive()
	doStripPlot <- theWidget("explore_seasonal_stripplot_radiobutton")$getActive()
	doViolinPlot <- theWidget("explore_seasonal_violinplot_radiobutton")$getActive()
	
	addLogComment("Generate seasonal plot")
	
	tmpObjs <- c('tmp.data')
	
	# initalise data
	if (myN == length(hsp$data)) {
		guiDo(tmp.data <- hsp$data)
	} else {
		guiDo(isExpression=T, bquote(
			tmp.data <- hsp$data[.(selNames)]
		))
	}
	
	if (doMonths) {
		guiDo({
			tmp.data <- sync.timeblobs(lapply(tmp.data, aggregate.timeblob, 
				by="months"), timelim=hsp$timePeriod)
			tmp.data$Season <- factor(months(tmp.data$Time, abbreviate=TRUE),
				levels=c("Jan","Feb","Mar","Apr","May","Jun",
				"Jul","Aug","Sep","Oct","Nov","Dec"), ordered=T)
		})
	} else {
		guiDo({
			tmp.data <- lapply(tmp.data, function(x) {
				window(x, start=trunc.year(start(x)), extend=T) })
			tmp.data <- sync.timeblobs(lapply(tmp.data, aggregate.timeblob, 
				by="3 months"), timelim=hsp$timePeriod)
			tmp.data$Season <- factor(quarters(tmp.data$Time), ordered=T)
		})
	}
	
	# plot specifications
	plotFn <- if (doStripPlot) { 'stripplot' } else { 'bwplot' }
	plot.cmd <- call(plotFn)
	plot.cmd[[2]] <- parse(text=paste(
		paste(make.names(selNames), collapse=" + "), "~ Season"
	))[[1]]
	plot.cmd[[3]] <- quote(tmp.data)
	plot.cmd$outer <- T
	plot.cmd$panel <- if (doViolinPlot) {
		function(...) {
			panel.violin(varwidth=T, ...)
			panel.stripplot(pch=3, ...)
		}}
	plot.cmd$layout <- if (doMonths) { bquote(c(1, .(myN))) }
	plot.cmd$jitter <- if (doStripPlot) { T }
	
	# plot scales and annotation specifications
	plot.cmd$scales$y$log <- T
	plot.cmd$ylab <- attr(hsp$data[[selNames[1]]], "dataname")
	plot.cmd$yscale.components <- quote(lattice.y.prettylog)
	
	# hydrosanity caption
	addToLog("## Make hydrosanity caption")
	tmpObjs <- c(tmpObjs, 'tmp.n', 'tmp.caption')
	guiDo(isExpression=T, bquote({
		tmp.n <- sum(sapply(tmp.data[2:.(myN+1)], is.na)==F)
		tmp.caption <- hydrosanity.caption(
			range(tmp.data$Time), # TODO: should add timestep to this
			by=.(attr(tmp.data, "timestep")), n=tmp.n, 
			series=.(myN))
	}))
	plot.cmd$sub <- quote(tmp.caption)
	
	setPlotDevice("seasonality")
	setCairoWindowButtons("seasonality", identify=T, log=T)
	
	result <- guiDo(plot.cmd, isExpression=T)
	# plot trellis object
	guiDo(bquote(print(.(result))), isExpression=T, doLog=F)
	
	.hydrosanity$call[["seasonality"]] <<- evalCallArgs(plot.cmd, pattern="^tmp")
	
	if (length(tmpObjs) > 0) {
		addToLog(paste('rm(', paste(tmpObjs, collapse=', '), ')', sep=''))
		rm(list=tmpObjs, envir=.GlobalEnv)
	}
	setStatusBar("Generated seasonal plot")
}

