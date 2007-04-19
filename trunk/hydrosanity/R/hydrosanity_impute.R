## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

updateImputePage <- function() {
	setTextview(theWidget("impute_textview"), "")
	
	setupIconView(theWidget("impute_iconview"))
	.hs_on_impute_iconview_selection_changed()
	
	.hydrosanity$update$impute <<- F
	APPWIN$present()
}

.hs_on_impute_accum_button_clicked <- function(button) {
	APPWIN$setSensitive(F)
	on.exit(APPWIN$setSensitive(T))
	setStatusBar("")
	
	selNames <- iconViewGetSelectedNames(theWidget("impute_iconview"))
	if (length(selNames) == 0) {
		errorDialog("No items selected.")
		return()
	}
	nBlobs <- length(selNames)
	
	doAccumByGaps <- theWidget("impute_accum_gaps_radiobutton")$getActive()
	accumGapsSteps <- theWidget("impute_accum_gaps_spinbutton")$getValue()
	doAccumByColumn <- theWidget("impute_accum_column_radiobutton")$getActive()
	accumStepsColumn <- theWidget("impute_accum_column_entry")$getText()
	doImputeByCorr <- theWidget("impute_missing_corr_radiobutton")$getActive()
	doImputeByLinear <- theWidget("impute_missing_linear_radiobutton")$getActive()
	doImputeByConstant <- theWidget("impute_missing_constant_radiobutton")$getActive()
	
	addLogComment("Redistribute accumulated values")
	addToLog("## (not shown)")
	
	for (x in selNames) {
		subBlob <- window(hsp$data[[x]], hsp$timePeriod[1], hsp$timePeriod[2])
		# get subBlob offset
		subLim <- window(hsp$data[[x]], hsp$timePeriod[1], 
			hsp$timePeriod[2], return.indices=T)
		spanEnd <- c()
		spanLength <- c()
		spanAccum <- c()
		if (doAccumByGaps) {
			gapInfo <- gaps(subBlob$Data)
			ok <- (gapInfo$gapLength <= accumGapsSteps)
			spanEnd <- gapInfo$gapEnd[ok] + 1 # end at accum step
			spanLength <- gapInfo$gapLength[ok] + 1 # includes accum
		}
		if (doAccumByColumn) {
			spanEnd <- which(subBlob[[accumStepsColumn]] > 1)
			spanLength <- subBlob[[accumStepsColumn]][spanEnd]
			# check that first gap is complete
			if ((length(spanEnd) > 0) && (spanEnd[1] <= spanLength[1])) {
				spanEnd <- spanEnd[-1]
				spanLength <- spanLength[-1]
			}
		}
		if (length(spanEnd) == 0) { next }
		spanStart <- spanEnd - spanLength + 1
		spanAccum <- subBlob$Data[spanEnd]
		
		if (doImputeByConstant) {
			allSpanIndices <- sequence(spanLength) + rep(spanStart, times=spanLength) - 1
			allSpanValues <- rep(spanAccum / spanLength, times=spanLength)
			# add offset to account for window (hsp$timePeriod)
			allSpanIndices <- allSpanIndices + subLim[1] - 1
			# set imputed values
			hsp$data[[x]]$Data[allSpanIndices] <<- allSpanValues
			levels(hsp$data[[x]]$Qual) <<- union(levels(hsp$data[[x]]$Qual), "disaccumulated")
			hsp$data[[x]]$Qual[allSpanIndices] <<- "disaccumulated"
			if (doAccumByColumn) {
				hsp$data[[x]][allSpanIndices,accumStepsColumn] <<- 1
			}
		} else {
			# impute as usual and then scale
		}
	}
	
	datasetModificationUpdate()
}


.hs_on_impute_iconview_selection_changed <- function(...) {
	TXV <- theWidget("impute_textview")
	setTextview(TXV, "")
	
	selNames <- iconViewGetSelectedNames(theWidget("impute_iconview"))
	
	for (x in selNames) {
		gapInfo <- gaps(window(hsp$data[[x]], 
			hsp$timePeriod[1], hsp$timePeriod[2])$Data)
		myHeader <- sprintf("%s gap length counts (total gaps: %i)",
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


