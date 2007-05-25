## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

updateImputePage <- function() {
	
	setupIconView(theWidget("impute_iconview"))
	
	.hs_on_impute_iconview_selection_changed()
	
	.hydrosanity$update$impute <<- F
	theWidget(APPWIN)$present()
}

.hs_on_impute_view_error_scatter_button_clicked <- function(button) {
	theWidget(APPWIN)$setSensitive(F)
	on.exit(theWidget(APPWIN)$setSensitive(T))
	setStatusBar("")
	
	selNames <- iconViewGetSelectedNames(theWidget("impute_iconview"))
	if (length(selNames) == 0) {
		errorDialog("No items selected.")
		return()
	}
	
	doRawData <- theWidget("impute_rawdata_radiobutton")$getActive()
	doAggr1 <- theWidget("impute_aggr1_radiobutton")$getActive()
	doAggr2 <- theWidget("impute_aggr2_radiobutton")$getActive()
	aggr1By <- theWidget("impute_aggr1_comboboxentry")$getActiveText()
	aggr2By <- theWidget("impute_aggr2_comboboxentry")$getActiveText()
	startMonth <- theWidget("explore_yearstart_combobox")$getActive() + 1
	
	addLogComment("View imputed vs actual values scatterplot")
	
	roles <- sapply(hsp$data, attr, "role")
	if (length(unique(roles[selNames])) > 1) {
		errorDialog("All selected items must have same role (e.g RAIN / FLOW).")
		return()
	}
	
	tmpObjs <- c("tmp.predictors")
	sameRole <- (roles == roles[selNames[1]])
	guiDo(isExpression=T, bquote({
		tmp.vars <- .(selNames)
		tmp.predictors <- .(names(hsp$data)[sameRole])
	}))
	
	impute.cmd <- call('impute.timeblobs')
	impute.cmd[[2]] <- quote(hsp$data[tmp.predictors])
	impute.cmd$which.impute <- quote(tmp.vars)
	impute.cmd$timelim <- if (!is.null(hsp$timePeriod)) { quote(hsp$timePeriod) }
	impute.cmd$extend <- T
	impute.cmd$method <- "distance"
	
	impute.assign.cmd <- quote(tmp.data <- foo)
	impute.assign.cmd[[3]] <- impute.cmd
	
	guiDo(impute.assign.cmd, isExpression=T)
	
	# compute and store aggregated series
	if (doAggr1 || doAggr2) {
		aggrBy <- if (doAggr1) { aggr1By } else { aggr2By }
		aggr.cmd <- bquote(
			tmp.data <- lapply(tmp.data, aggregate.timeblob, by=.(aggrBy))
		)
		if (any(grep(" month", aggrBy)) || any(grep("year", aggrBy))) {
			if (startMonth != 1) {
				aggr.cmd[[3]]$start.month <- startMonth
			}
		}
		guiDo(aggr.cmd, isExpression=T)
	}
	
	# prepare data for plot
	guiDo(tmp.groups <- do.call(make.groups, 
		lapply(tmp.data, function(x) x$Data )))
	guiDo(tmp.groups$imputed <- unlist(lapply(tmp.data, function(x) x$Imputed )))

	plot.cmd <- quote(xyplot(data ~ imputed | which, tmp.groups, aspect="iso",
		panel=function(...) {
			panel.abline(a=0, b=1,
				col.line=trellis.par.get("superpose.line")$col[2],
				lty=trellis.par.get("superpose.line")$lty[2])
			panel.xyplot(...)
		}
	))
	
	setPlotDevice("imputed-vs-actual")
	setCairoWindowButtons("imputed-vs-actual", identify=T, zoomin=T, centre=T, log=F)
	
	result <- guiDo(plot.cmd, isExpression=T)
	# plot trellis object
	guiDo(print(result), doLog=F)
	
	.hydrosanity$call[["imputed-vs-actual"]] <<- evalCallArgs(plot.cmd, pattern="^tmp")
	
	# construct call to panel.identify() for later use
	id.cmd <- call('panel.identify')
	id.cmd$labels <- unlist(lapply(tmp.data, function(x) {
			format(x$Time, timestepTimeFormat(attr(x, "timestep")))
		}))
	.hydrosanity$id.call[["imputed-vs-actual"]] <<- id.cmd
	
	if (length(tmpObjs) > 0) {
		addToLog(paste('rm(', paste(tmpObjs, collapse=', '), ')', sep=''))
		rm(list=tmpObjs, envir=.GlobalEnv)
	}
	setStatusBar("Generated imputed vs actual values scatterplot")
	
}

.hs_on_impute_missing_button_clicked <- function(button, justDisaccumulate=F) {
	theWidget(APPWIN)$setSensitive(F)
	on.exit(theWidget(APPWIN)$setSensitive(T))
	setStatusBar("")
	
	selNames <- iconViewGetSelectedNames(theWidget("impute_iconview"))
	if (length(selNames) == 0) {
		errorDialog("No items selected.")
		return()
	}
	
	maxGapLength <- theWidget("impute_missing_gaps_comboboxentry")$getActiveText()
	doInternalGapsOnly <- theWidget("impute_missing_gaps_internal_checkbutton")$getActive()
	doLocally <- theWidget("impute_locally_checkbutton")$getActive()
	doImputeByDistance <- theWidget("impute_missing_distance_radiobutton")$getActive()
	doImputeByLM <- theWidget("impute_missing_regression_radiobutton")$getActive()
	doImputeByConstant <- theWidget("impute_missing_constant_radiobutton")$getActive()
	constTypeIdx <- theWidget("impute_missing_constant_combobox")$getActive() + 1
	constType <- switch(constTypeIdx, "mean", "mean", "zero", "extend")
	doTrim <- (constTypeIdx == 2)
	imputeMethod <- c(
		if (doImputeByDistance) { "distance" },
		if (doImputeByLM) { "regression" },
		if (doImputeByConstant) { "constant" })
	
	addLogComment("Impute missing values")
	
	roles <- sapply(hsp$data, attr, "role")
	if (length(unique(roles[selNames])) > 1) {
		errorDialog("All selected items must have same role (e.g RAIN / FLOW).")
		return()
	}
	
	tmpObjs <- c("tmp.predictors")
	sameRole <- (roles == roles[selNames[1]])
	guiDo(isExpression=T, bquote({
		tmp.vars <- .(selNames)
		tmp.predictors <- .(names(hsp$data)[sameRole])
	}))
	
	imputeFn <- if (justDisaccumulate) {
		'disaccumulate.timeblobs'
	} else {
		'imputeGaps.timeblobs'
	}
	impute.cmd <- call(imputeFn)
	impute.cmd[[2]] <- quote(hsp$data[tmp.predictors])
	impute.cmd$which.impute <- quote(tmp.vars)
	impute.cmd$timelim <- if (!is.null(hsp$timePeriod)) { quote(hsp$timePeriod) }
	impute.cmd$extend <- if (!doInternalGapsOnly) { T }
	impute.cmd$method <- imputeMethod
	impute.cmd$constant <- if (doImputeByConstant) { constType }
	impute.cmd$trim <- if (doTrim) { 0.01 }
	
	impute.assign.cmd <- quote(hsp$data[tmp.vars] <- foo)
	impute.assign.cmd[[3]] <- impute.cmd
	
	guiDo(impute.assign.cmd, isExpression=T)
	
	guiDo(rm(tmp.vars, tmp.predictors))
	
	datasetModificationUpdate()
}

.hs_on_impute_disaccumulate_button_clicked <- function(button) {
	.hs_on_impute_missing_button_clicked(button, justDisaccumulate=T)
}

.hs_on_impute_undo_imputed_button_clicked <- function(button) {
	theWidget(APPWIN)$setSensitive(F)
	on.exit(theWidget(APPWIN)$setSensitive(T))
	setStatusBar("")
	
	selNames <- iconViewGetSelectedNames(theWidget("impute_iconview"))
	if (length(selNames) == 0) {
		errorDialog("No items selected.")
		return()
	}
	
	addLogComment("Revert imputed values")
	
	guiDo(isExpression=T, bquote({
		tmp.vars <- .(selNames)
		hsp$data[tmp.vars] <- unimpute.timeblobs(hsp$data[tmp.vars], 
			timelim=hsp$timePeriod, type="imputed")
		rm(tmp.vars)
	}))
	
	datasetModificationUpdate()
}

.hs_on_impute_undo_accumulated_button_clicked <- function(button) {
	theWidget(APPWIN)$setSensitive(F)
	on.exit(theWidget(APPWIN)$setSensitive(T))
	setStatusBar("")
	
	selNames <- iconViewGetSelectedNames(theWidget("impute_iconview"))
	if (length(selNames) == 0) {
		errorDialog("No items selected.")
		return()
	}
	
	addLogComment("Revert disaccumulated values")
	
	guiDo(isExpression=T, bquote({
		tmp.vars <- .(selNames)
		hsp$data[tmp.vars] <- unimpute.timeblobs(hsp$data[tmp.vars], 
			timelim=hsp$timePeriod, type="disaccumulated")
		rm(tmp.vars)
	}))
	
	datasetModificationUpdate()
}


.hs_on_impute_iconview_selection_changed <- function(...) {
	TXV <- theWidget("impute_textview")
	setTextview(TXV, "")
	
	selNames <- iconViewGetSelectedNames(theWidget("impute_iconview"))
	
	for (x in selNames) {
		gapInfo <- gaps(window(hsp$data[[x]], hsp$timePeriod[1], 
			hsp$timePeriod[2], extend=T)$Data, internal.only=T)
		myHeader <- sprintf("%s gap length counts in specified period (total gaps: %i)",
			x, length(gapInfo$length))
		#mySumm <- paste(capture.output(
		#	print(table(gapInfo$length, dnn=myHeader))
		#), collapse='\n')
		gapTable <- table(gapInfo$length)
		mySumm <- paste(sep='', 
			myHeader, "\n  ",
			paste(sep='', collapse=', ',
				gapTable, 'x[', dimnames(gapTable)[[1]], ']')
		)
		if (length(gapInfo$length) == 0) {
			mySumm <- paste(x, "has no gaps in the specified time period.")
		}
		addTextview(TXV, mySumm, "\n")
	}
}


