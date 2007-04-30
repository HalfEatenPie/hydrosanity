## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

updateImputePage <- function() {
	
	setupIconView(theWidget("impute_iconview"))
	
	.hs_on_impute_iconview_selection_changed()
	
	.hydrosanity$update$impute <<- F
	APPWIN$present()
}

.hs_on_impute_missing_button_clicked <- function(button) {
	APPWIN$setSensitive(F)
	on.exit(APPWIN$setSensitive(T))
	setStatusBar("")
	
	selNames <- iconViewGetSelectedNames(theWidget("impute_iconview"))
	if (length(selNames) == 0) {
		errorDialog("No items selected.")
		return()
	}
	
	maxGapLength <- theWidget("impute_missing_gaps_comboboxentry")$getActiveText()
	doInternalGapsOnly <- theWidget("impute_missing_gaps_internal_checkbutton")$getActive()
	doImputeByLM <- theWidget("impute_missing_regression_radiobutton")$getActive()
	doLMLocalWeighting <- theWidget("impute_missing_regression_local_checkbutton")$getActive()
	doImputeByDistance <- theWidget("impute_missing_distance_radiobutton")$getActive()
	byDistanceNSites <- theWidget("impute_missing_distance_nsites_spinbutton")$getValue()
	doImputeByConstant <- theWidget("impute_missing_constant_radiobutton")$getActive()
	constTypeIdx <- theWidget("impute_missing_constant_combobox")$getActive() + 1
	constType <- switch(constTypeIdx, "mean", "trim.mean", "zero", "extend")
	imputeMethod <- c(
		if (doImputeByLM) { "regression" },
		if (doImputeByDistance) { "distance" },
		if (doImputeByConstant) { "constant" })
	
	doAccumByColumn <- theWidget("impute_accum_column_radiobutton")$getActive()
	accumStepsColumn <- theWidget("impute_accum_column_entry")$getText()
	doAccumByGaps <- theWidget("impute_accum_gaps_radiobutton")$getActive()
	maxAccumLength <- theWidget("impute_accum_gaps_comboboxentry")$getActiveText()
	disaccMethod <- c(
		if (doAccumByColumn) { "column" },
		if (doAccumByGaps) { "gaps" },
		if (!(doAccumByColumn || doAccumByGaps)) { "no" })
	
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
	
	impute.call <- call('impute.timeblobs')
	impute.call[[2]] <- quote(hsp$data[tmp.predictors])
	impute.call$which.impute <- quote(tmp.vars)
	impute.call$timelim <- if (!is.null(hsp$timePeriod)) { quote(hsp$timePeriod) }
	impute.call$maxGapLength <- if (!identical(maxGapLength, "any length")) {
		maxGapLength
	}
	impute.call$method <- imputeMethod
	impute.call$constant <- if (doImputeByConstant) { constType }
	impute.call$maxPredictors <- if (doImputeByDistance) { byDistanceNSites }
	impute.call$disaccumulate <- disaccMethod
	impute.call$accumStepsColumn <- if (doAccumByColumn) { accumStepsColumn }
	impute.call$maxGapLengthAccum <- if (doAccumByGaps) {
		if (identical(maxAccumLength, "any length")) { NA } else
		{ maxAccumLength }
	}
	
	impute.assign.call <- quote(hsp$data[tmp.vars] <- foo)
	impute.assign.call[[3]] <- impute.call
	
	guiDo(impute.assign.call, isExpression=T)
	
	guiDo(rm(tmp.vars, tmp.predictors))
	
	datasetModificationUpdate()
}

.hs_on_impute_undo_imputed_button_clicked <- function(button) {
	APPWIN$setSensitive(F)
	on.exit(APPWIN$setSensitive(T))
	setStatusBar("")
	
	selNames <- iconViewGetSelectedNames(theWidget("impute_iconview"))
	if (length(selNames) == 0) {
		errorDialog("No items selected.")
		return()
	}
	
	guiDo(isExpression=T, bquote({
		tmp.vars <- .(selNames)
		hsp$data[tmp.vars] <- unimpute.timeblobs(hsp$data[tmp.vars], 
			timelim=hsp$timePeriod, type="imputed")
		rm(tmp.vars)
	}))
	
	datasetModificationUpdate()
}

.hs_on_impute_undo_accumulated_button_clicked <- function(button) {
	APPWIN$setSensitive(F)
	on.exit(APPWIN$setSensitive(T))
	setStatusBar("")
	
	selNames <- iconViewGetSelectedNames(theWidget("impute_iconview"))
	if (length(selNames) == 0) {
		errorDialog("No items selected.")
		return()
	}
	
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
			hsp$timePeriod[2])$Data, internal.only=F)
		myHeader <- sprintf("%s gap length counts in specified period (total gaps: %i)",
			x, length(gapInfo$gap.length))
		#mySumm <- paste(capture.output(
		#	print(table(gapInfo$gap.length, dnn=myHeader))
		#), collapse='\n')
		gapTable <- table(gapInfo$gap.length)
		mySumm <- paste(sep='', 
			myHeader, "\n  ",
			paste(sep='', collapse=', ',
				gapTable, 'x[', dimnames(gapTable)[[1]], ']')
		)
		if (length(gapInfo$gap.length) == 0) {
			mySumm <- paste(x, "has no gaps in the specified time period.")
		}
		addTextview(TXV, mySumm, "\n")
	}
}


