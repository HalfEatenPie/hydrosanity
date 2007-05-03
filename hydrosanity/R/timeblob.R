## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL


timeblob <- function(Time, Data, Qual=NULL, extras=NULL, timestep=NULL, dataname=NULL) {
	# check types
	Time <- as.POSIXct(Time)
	if (any(is.na(Time))) { stop("'Time' must be a vector of valid times (POSIXt)") }
	if (is.list(Data)) {
		extras <- Data[-1]
		Data <- Data[[1]]
	}
	if (!inherits(Data, "numeric")) { stop("'Data' must be numeric") }
	# construct timeblob
	blob <- data.frame(
		Time=Time,
		Data=Data
	)
	if (!is.null(Qual)) {
		blob$Qual <- Qual
	}
	if (!is.null(extras) && (length(extras) > 0)) {
		blob <- cbind(blob, extras)
	}
	class(blob) <- c("timeblob", "data.frame")
	# set timestep
	if (is.null(timestep)) {
		timestep <- as.byString(Time[2] - Time[1], digits=1)
		# check whether series is irregular
		timeStepDiffs <- diff(as.numeric(Time))
		# trim by 10% because of anomolies in seq.POSIXt with DSTdays
		timeStepRange <- c(quantile(timeStepDiffs, c(0.1, 0.9)))
		# up to 11% difference expected in regular series (feb vs jan)
		if (timeStepRange[2] > 1.11 * timeStepRange[1]) {
			timestep <- "irregular"
		}
	}
	attr(blob, "timestep") <- timestep
	# set dataname
	if (is.null(dataname)) { dataname <- deparse(substitute(Data)) }
	attr(blob, "dataname") <- dataname
	# check that it is.timeblob()!
	if (!is.timeblob(blob)) { stop("oops, timeblob() function made an invalid timeblob") }
	return(blob)
}

is.timeblob <- function(x) {
	return (!is.null(x) &&
		is(x, "timeblob") &&
		is.data.frame(x) &&
		(ncol(x) >= 2) &&
		inherits(x$Time, "POSIXct") &&
		inherits(x$Data, "numeric") &&
		!is.null(attr(x, "timestep")) &&
		!is.null(attr(x, "dataname")))
}

lapply.timeblob.data <- function(blob.list, FUN, ...) {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	# lapply to blob data
	lapply(lapply(blob.list, function(x) { x$Data }), FUN, ...)
}

sapply.timeblob.data <- function(blob.list, FUN, ...) {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	# sapply to blob data
	sapply(lapply(blob.list, function(x) { x$Data }), FUN, ...)
}

read.timeblob <- function(file, skip=1, sep=",", dataname="Data", dataCol=2, qualCol=3, extraCols=c(), extraNames=paste("Extra",extraCols), readTimesFromFile=T, timeCol=1, timeFormat="%d %b %Y", startTime=NA, timeSeqBy="DSTday", ...) {
	# check types
	if (!is.numeric(dataCol)) { stop("'dataCol' must be numeric (column number)") }
	if (readTimesFromFile) {
		if (!is.numeric(timeCol)) { stop("'timeCol' must be numeric (column number)") }
	} else {
		if (!is.list(startTime)) {
			startTime <- as.POSIXct(startTime)
			if (is.na(startTime)) { stop("could not convert 'startTime' to a time") }
		}
	}
	# make sure extra column names correspond to given columns
	length(extraNames) <- length(extraCols)
	extraNames[is.na(extraNames)] <- paste("Extra", which(is.na(extraNames)))
	# number of columns in file
	firstLine <- read.table(file, header=F, skip=skip, sep=sep, strip.white=T, nrows=1, ...)
	fileCols <- ncol(firstLine)
	if (dataCol > fileCols) {
		stop("Column ", dataCol, " ('dataCol') not found on line ", skip+1, 
		"; maybe 'sep'=\"", sep, "\" or 'skip'=", skip, " is wrong?")
	}
	# drop variables for which column does not exist in file
	if (qualCol > fileCols) { qualCol <- NULL }
	extraNames <- extraNames[!(extraCols > fileCols)]
	extraCols <- extraCols[!(extraCols > fileCols)]
	# define which columns to import and which to ignore
	fileColClasses <- rep("NULL", fileCols)
	if (readTimesFromFile) { fileColClasses[timeCol] <- "character" }
	fileColClasses[dataCol] <- "numeric" #, but then "" in file gives error
	fileColClasses[qualCol] <- NA # qualCol may be NULL
	fileColClasses[extraCols] <- NA # extraCols may be NULL
	# read file
	rawData <- read.table(file, header=F, skip=skip, sep=sep, colClasses=fileColClasses, strip.white=T, ...)
	# work out which column of rawData has the data (from dataCol)
	dataIndex <- dataCol - sum(fileColClasses[1:dataCol]=="NULL", na.rm=T)
	qualIndex <- NULL
	timeIndex <- NULL
	# extract quality codes or set to default (factor("NA"))
	myQual <- NA
	if (!is.null(qualCol) && !is.na(qualCol)) {
		qualIndex <- qualCol - sum(fileColClasses[1:qualCol]=="NULL", na.rm=T)
		myQual <- rawData[[qualIndex]]
		if (is.factor(myQual)) {
			myQual <- factor(myQual, exclude=NULL)
		}
	} else {
		myQual <- rep(factor(NA, exclude=NULL), nrow(rawData))
	}
	# convert or construct the time sequence
	myTime <- NA
	if (readTimesFromFile) {
		timeIndex <- timeCol - sum(fileColClasses[1:timeCol]=="NULL", na.rm=T)
		myTime <- strptime(rawData[[timeIndex]], format=timeFormat)
		if (any(is.na(myTime))) {
			firstNA <- which(is.na(myTime))[1]
			stop('could not convert "', rawData[firstNA,timeIndex],
			'" to time with format string "', timeFormat, '"')
		}
	} else {
		if ("list" %in% class(startTime)) {
			timeBits <- lapply(startTime, function(i) {
				if (is.numeric(i)) { firstLine[1,i] } else { i }
			})
			if (is.null(timeBits$hour)) { timeBits$hour <- 0 }
			if (is.null(timeBits$min)) { timeBits$min <- 0 }
			if (is.null(timeBits$sec)) { timeBits$sec <- 0 }
			startTime <- do.call(ISOdatetime, timeBits)
			if (is.na(startTime)) {
				myBits <- paste(paste(names(unlist(timeBits)), '=', unlist(timeBits)), collapse=', ')
				stop("could not construct starting time from columns given in 'startTime': ", myBits)
			}
		}
		myTime <- seq.POSIXt(from=startTime, by=timeSeqBy, length=nrow(rawData))
	}
	extras <- rawData[-c(timeIndex, dataIndex, qualIndex)]
	names(extras) <- extraNames
	blob <- timeblob(Time=myTime, Data=rawData[[dataIndex]], Qual=myQual, 
		extras=extras, dataname=dataname)
	return(blob)
}

# returns length 0 if blob is empty
start.timeblob <- function(blob) {
	if (!is.timeblob(blob)) { stop("'blob' must be a timeblob") }
	blob$Time[min(1,nrow(blob))]
}

# returns length 0 if blob is empty
end.timeblob <- function(blob) {
	if (!is.timeblob(blob)) { stop("'blob' must be a timeblob") }
	if (identical(attr(blob, "timestep"), "irregular")) {
		blob$Time[nrow(blob)]
	} else {
		if (nrow(blob)==0) { return(blob$Time[0]) }
		# extrapolate last time step
		seq.POSIXt(from=blob$Time[nrow(blob)], by=attr(blob, "timestep"), 
			length=2)[2]
	}
}

start.timeblobs <- function(blob.list) {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	globalStart <- as.POSIXct(min(unlist(lapply(blob.list, start.timeblob))))
	return(globalStart)
}

end.timeblobs <- function(blob.list) {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	globalEnd <- as.POSIXct(max(unlist(lapply(blob.list, end.timeblob))))
	return(globalEnd)
}


window.timeblob <- function(blob, start=NULL, end=NULL, inclusive=F, return.indices=F, extend=F) {
	# check types
	if (!is.timeblob(blob)) { stop("'blob' must be a timeblob") }
	if (is.null(start)) { start <- start(blob) }
	if (is.null(end)) { end <- end(blob) }
	start <- as.POSIXct(start)
	end <- as.POSIXct(end)
	if (any(is.na(c(start, end)))) { stop("'start' and 'end' must be valid times (POSIXt)") }
	if (extend) {
		# pad with NA values out to specified limits
		timestep <- attr(blob, "timestep")
		if (is.null(timestep)) {
			stop("'blob' needs a timestep attribute for 'extend=T'")
		}
		negTimestep <- paste("-1", timestep)
		if (length(grep("^[0-9]", timestep)>0)) {
			negTimestep <- paste("-", timestep, sep='')
		}
		if (start < start(blob)) {
			extendTimes <- seq.POSIXt(start(blob), start, by=negTimestep)[-1]
			extendTimes <- rev(extendTimes)
			extendBlob <- blob[c(0,rep(NA,length(extendTimes))),]
			extendBlob$Time <- extendTimes
			blob <- rbind(extendBlob, blob)
		}
		if (end > end(blob)) {
			extendTimes <- seq.POSIXt(end(blob), end, by=timestep)[-1]
			extendBlob <- blob[c(0,rep(NA,length(extendTimes))),]
			extendBlob$Time <- extendTimes
			blob <- rbind(blob, extendBlob)
		}
	}
	
	windowIdx <- findIntervalPeriod(start, end, blob$Time, inclusive=inclusive)
	
	if (!identical(attr(blob, "timestep"), "irregular")) {
		# TODO: need to handle last time step inclusive
	}
	if (return.indices) {
		return(windowIdx)
	}
	return(blob[seq(windowIdx[1],windowIdx[2]),])
}

findIntervalPeriod <- function(xLo, xHi, vec, inclusive=F) {
	if (xLo > xHi) { stop("'xHi' must be greater than 'xLo'") }
	# check whether vec has any elements
	if (length(vec)==0) {
		return(c(0,0))
	}
	# check whether the period intersects at all with 'vec'
	if ((xHi < vec[1]) || (xLo > vec[length(vec)])) {
		return(c(0,0))
	}
	windowIdx <- findInterval(c(xLo,xHi), vec)
	if (inclusive) {
		# round up at end (findInterval rounds down)
		test <- vec[windowIdx[2]]
		if ((length(test)>0) && (test != xHi) && (windowIdx[2] < length(vec))) {
			windowIdx[2] <- windowIdx[2] + 1
		}
	} else {
		# round up at start (findInterval rounds down)
		test <- vec[windowIdx[1]]
		if ((length(test)>0) && (test != xLo) && (windowIdx[1] < length(vec))) {
			windowIdx[1] <- windowIdx[1] + 1
		}
	}
	# if there are no complete intervals (inclusive==F)
	if (windowIdx[1] > windowIdx[2]) { return(c(0,0)) }
	return(windowIdx)
}


syncTo.timeblobs <- function(blob.list, blob, extractColumn="Data") {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	if (!is.timeblob(blob)) { stop("'blob' must be a timeblob") }
	# construct data frame
	syncData <- data.frame(
		Time=blob$Time,
		lapply(blob.list, function(x) {
			mySyncIndices <- matchtimes.timeblob(x, blob$Time)
			x[mySyncIndices, extractColumn]
		})
	)
	attr(syncData, "timestep") <- attr(blob, "timestep")
	return(syncData)
}



# timestep needs to be fast enough for all blobs (does not aggregate to slower time steps)
sync.timeblobs <- function(blob.list, timestep=NULL, timelim=NULL, extractColumn="Data") {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	if (is.null(timelim)) {
		timelim <- c(start.timeblobs(blob.list), end.timeblobs(blob.list))
	} else {
		timelim <- as.POSIXct(timelim)
		if (any(is.na(timelim))) { stop("'timelim' must be a pair of valid times (POSIXt)") }
		blob.list <- lapply(blob.list, window, min(timelim), max(timelim))
	}
	# setup
	if (is.null(timestep)) {
		timestep <- common.timestep.timeblobs(blob.list)
	}
	times <- seq.POSIXt(min(timelim), max(timelim), 
		by=timestep)
	# omit last time since the 'timelim' extends to *end* of last period
	times <- times[-length(times)]
	# construct data frame
	syncData <- data.frame(
		Time=times,
		lapply(blob.list, function(x) {
			mySyncIndices <- matchtimes.timeblob(x, times)
			x[mySyncIndices, extractColumn]
		})
	)
	attr(syncData, "timestep") <- timestep
	return(syncData)
}

# find indices into timeblob 'blob' for each time in 'times'
# values of NA are used for times outside 'blob'
#
# it resamples blob$Time to correspond to each time in 'times'
#
# x <- seq(as.POSIXct("1970-01-01"), by="2 years", length=5)
# blob <- timeblob(Time=x, Data=seq(70, by=2, length=5))
# yearseq <- seq(as.POSIXct("1965-01-01"), by="years", length=10)
# data.frame(yearseq, 
# 	blob.index=matchtimes.timeblob(blob, yearseq), 
# 	blob.data=blob$Data[matchtimes.timeblob(blob, yearseq)])
#
# sixseq <- seq(as.POSIXct("1970-06-06"), as.POSIXct("1981-06-06"), by="6 months")
# data.frame(sixseq, 
# 	blob.index=matchtimes.timeblob(blob, sixseq), 
# 	blob.data=blob$Data[matchtimes.timeblob(blob, sixseq)])

matchtimes.timeblob <- function(blob, times) {
	# check types
	if (!is.timeblob(blob)) { stop("'blob' must be a timeblob") }
	times <- as.POSIXct(times)
	if (any(is.na(times))) { stop("'times' must be a vector of valid times (POSIXt)") }
	# default for indices is NA
	periodIndices <- rep(as.integer(NA), length(times))
	# each blob here may be outside 'times', and may be empty
	if ((nrow(blob)==0)
	  || (start(blob) > times[length(times)])
	  || (end(blob) < times[1])) {
		return(periodIndices)
	}
	# each blob here is not entirely outside 'times'
	blobBounds <- findIntervalPeriod(start(blob), end(blob), times)
	blobWindowIndices <- seq(blobBounds[1], blobBounds[2])
	# now times[blobWindowIndices] is not outside blob$Time
	periodIndices[blobWindowIndices] <- findInterval(times[blobWindowIndices], blob$Time)
	return(periodIndices)
}

common.timestep.timeblobs <- function(blob.list, default="DSTdays") {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	timestep <- NULL
	for (i in seq(along=blob.list)) {
		if (identical(attr(blob.list[[i]], "timestep"), "irregular")) { next }
		thisStepDelta <- as.numeric.byString(
			attr(blob.list[[i]], "timestep"))
		if (is.null(timestep) || (thisStepDelta < timestep)) {
			timestep <- thisStepDelta
		}
	}
	if (is.null(timestep)) { return(default) }
	else { return(as.byString(timestep)) }
}


# invisibly returns missing fraction for each series
summary.missing.timeblobs <- function(blob.list, timelim=NULL, timestep=NULL) {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	if (is.null(timelim)) {
		timelim <- c(start.timeblobs(blob.list), end.timeblobs(blob.list))
	} else {
		timelim <- as.POSIXct(timelim)
		if (any(is.na(timelim))) { stop("'timelim' must be a pair of valid times (POSIXt)") }
		blob.list <- lapply(blob.list, window, timelim[1], timelim[2])
	}
	# setup
	nBlobs <- length(blob.list)
	if (is.null(timestep)) {
		timestep <- common.timestep.timeblobs(blob.list)
	}
	times <- seq.POSIXt(timelim[1], timelim[2], by=timestep)
	myLengths <- sapply(blob.list, nrow)
	myNAs <- lapply(blob.list, function(x) { is.na(x$Data) })
	myDataPoints <- myLengths - sapply(myNAs, sum)
	myDataFrac <- myDataPoints / length(times)
	overallDataFrac <- mean(myDataFrac)
	myCompleteN <- sum(myDataFrac >= 1)
	my95PctN <- sum(myDataFrac > 0.95)
	my75PctN <- sum(myDataFrac > 0.75)
	
	# find whether data exists for each timeblob for each time in myPeriod
	# (note: this rounds down if times do not match)
	dataMatrix <- sync.timeblobs(blob.list, timestep=timestep)
	dataExistsMatrix <- !is.na(as.matrix(dataMatrix[-1]))
	
	activeNs <- apply(dataExistsMatrix, 1, sum)
	allActiveSteps <- sum(activeNs == nBlobs)
	allActiveFrac <- allActiveSteps / length(times)
	activeNQ <- quantile(activeNs, probs=c(0.25, 0.5, 0.75))
	activeNQFrac <- activeNQ / nBlobs
	
	cat(sprintf('Overall, %.0f%% of data is missing.\n', (1-overallDataFrac)*100))
	cat(sprintf('There are %i time series, of which %i %s complete.\n', 
		nBlobs, myCompleteN, ifelse(myCompleteN==1,'is','are')))
	cat(sprintf('...%i %s > 95%% complete and %i %s > 75%% complete.\n', 
		my95PctN, ifelse(my95PctN==1,'is','are'), my75PctN, ifelse(my75PctN==1,'is','are')))
	cat('\n')
	cat(sprintf('%i time steps (%.1f%%) have data from all series.\n', allActiveSteps, allActiveFrac*100))
	cat(sprintf('The median number of active sites is %i (%.0f%%).\n', activeNQ[2], activeNQFrac[2]*100))
	cat(sprintf('...Half the time, the number of active sites is between %i and %i.\n', activeNQ[1], activeNQ[3]))
	#cat(sprintf('The number of active sites ranges from %i to %i.\n', activeNQ[1], activeNQ[3]))
	
	# gap length distribution
	
	# quality code summary
	
	# missing fraction for each series
	missingFrac <- (length(times) - apply(dataExistsMatrix, 2, sum)) / length(times)
	
	invisible(missingFrac)
}


# this only handles regular series (the calculation of NA proportion requires it)
# column 3 = "Quality (mode)"; cols 4+ = "%good", "%maybe", "%poor", "%disaccumulated", "%imputed"
aggregate.timeblob <- function(blob, by="1 year", FUN=NULL, start.month=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"), max.na.proportion=0.05) {
	# check types
	if (!is.timeblob(blob)) { stop("'blob' must be a timeblob") }
	if (is.null(FUN)) {
		if (identical(attr(blob, "role"), "RAIN")) {
			FUN <- sum
		} else {
			FUN <- mean
		}
	}
	start.month <- match.arg(start.month)
	if (length(grep("( month|year)", by)) > 0) {
		newStart <- as.POSIXlt(trunc.month(start(blob)))
		oldMon <- newStart$mon
		newMon <- match(start.month, c("Jan","Feb","Mar","Apr","May",
			"Jun","Jul","Aug","Sep","Oct","Nov","Dec")) - 1
		newStart$mon <- newMon
		if (newMon > oldMon) { newStart$year <- newStart$year - 1 }
		blob <- window(blob, start=newStart, extend=T)
	}
	# find expected number of old timesteps in each new timestep
	oldDelta <- as.numeric.byString(attr(blob, "timestep"))
	newDelta <- as.numeric.byString(by)
	freqN <- floor(newDelta / oldDelta)
	# construct groups
	dateGroups <- cut.POSIXt(blob$Time, breaks=by)
	newDates <- as.POSIXct(levels(dateGroups))
	# aggregate
	newVals <- aggregate(blob$Data, by=list(dateGroups), FUN=FUN, na.rm=T)[,2]
	# set NA values
	eachNA <- aggregate(blob$Data, by=list(dateGroups), FUN=function(x) {sum(is.na(x))})[,2]
	newVals[eachNA > freqN * max.na.proportion] <- NA
	# check that first and last groups have enough data points
	firstN <- sum(dateGroups==dateGroups[1], na.rm=T)
	if (firstN < freqN * (1-max.na.proportion)) {
		newVals[1] <- NA
	}
	lastN <- sum(dateGroups==dateGroups[length(dateGroups)], na.rm=T)
	if (lastN < freqN * (1-max.na.proportion)) {
		newVals[length(newVals)] <- NA
	}
	# construct new blob
	newBlob <- timeblob(Time=newDates, Data=newVals,
		timestep=by, dataname=attr(blob, "dataname"))
	return(newBlob)
}


impute.timeblobs <- function(blob.list, which.impute=names(blob.list), timelim=NULL, withinTimeframe=NA, maxGapLength=NA, internalGapsOnly=F, method=c("constant", "distance", "correlation", "regression"), constant=c("mean", "trim.mean", "zero", "extend"), trim=0.01, maxPredictors=3, disaccumulate=c("column", "gaps", "no"), accumStepsColumn="AccumSteps", maxGapLengthAccum="4 days") {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	if (!is.null(timelim)) {
		timelim <- as.POSIXct(timelim)
		if (any(is.na(timelim))) { stop("'timelim' must be a pair of valid times (POSIXt)") }
	}
	method <- match.arg(method)
	disaccumulate <- match.arg(disaccumulate)
	constant <- match.arg(constant)
	doAccum <- (disaccumulate != "no")
	
	# a list will be returned with the imputed blobs
	newBlobs <- blob.list[which.impute]
	
	# take just the time-period window of all timeblobs, 
	#   but keep its offset in the original data (so can update later)
	blobs <- NULL
	subLims <- NULL
	if (!is.null(timelim)) {
		blobs <- lapply(blob.list, window, timelim[1], timelim[2])
		subLims <- lapply(blob.list, window, timelim[1], timelim[2],
			return.indices=T)
	} else {
		blobs <- blob.list
	}
	
	# find all multiple accumulations (according to user definition)
	# and extract each accumulated value into a new column "AccumValue"
	# (so that raw data correctly indicates a missing value there)
	# (also make AccumSteps column if does not already exist)
	for (x in names(blobs)) {
		if (!doAccum) { break }
		spanEnd <- c()
		spanLength <- c()
		spanAccum <- c()
		# find all gaps less than maxGapLengthAccum
		if (disaccumulate == "gaps") {
			gapInfo <- gaps(blobs[[x]]$Data,
				internal.only=internalGapsOnly)
			maxGapStepsAccum <- if (is.na(maxGapLengthAccum))
				{ Inf } else { round(
				as.numeric.byString(maxGapLengthAccum)
				/ as.numeric.byString(attr(blobs[[x]], "timestep"))) }
			ok <- (gapInfo$gap.length <= maxGapStepsAccum)
			spanEnd <- gapInfo$gap.end[ok] + 1 # end at accum step
			spanLength <- gapInfo$gap.length[ok] + 1 # includes accum
		}
		# find gaps according to specified column (accumStepsColumn)
		if (disaccumulate == "column") {
			spanEnd <- which(blobs[[x]][[accumStepsColumn]] > 1)
			spanLength <- blobs[[x]][[accumStepsColumn]][spanEnd]
			# drop first gap if it is incomplete (cut off by 'window')
			if ((length(spanEnd) > 0) && (spanEnd[1] <= spanLength[1])) {
				spanEnd <- spanEnd[-1]
				spanLength <- spanLength[-1]
			}
		}
		# store length and sum of accumulations
		accumSteps <- rep(integer(1), nrow(blobs[[x]]))
		accumSteps[spanEnd] <- spanLength
		blobs[[x]][["AccumSteps"]] <- accumSteps
		accumVal <- rep(0, nrow(blobs[[x]]))
		accumVal[spanEnd] <- blobs[[x]]$Data[spanEnd]
		blobs[[x]][["AccumVal"]] <- accumVal
		# set accumulated observation to NA (it is part of the gap)
		blobs[[x]]$Data[spanEnd] <- NA
	}
	
	# exclude any "imputed" values: do not use imputed values to impute
	rawBlobs <- blobs
	for (x in names(rawBlobs)) {
		rawBlobs[[x]]$Data[ (blobs[[x]]$Qual == "imputed")
				| (blobs[[x]]$Qual == "disaccumulated") ] <- NA
	}
	
	for (blobName in which.impute) {
		print(paste("going to impute site", blobName))
		
		blob <- blobs[[blobName]]
		blobIndex <- match(blobName, names(blobs))
		levels(blob$Qual) <- union(levels(blob$Qual), c("imputed", "disaccumulated"))
		# synchronise all other data to this blob
		rawSync <- syncTo.timeblobs(rawBlobs, blob)
		
		print("original gaps:")
		str(gaps(blob$Data))
		
		repeat {
			
			# find all gaps up to maxGapLength
			maxGapSteps <- if (is.na(maxGapLength)) { Inf } else
				{ round(as.numeric.byString(maxGapLength)
				/ as.numeric.byString(attr(blob, "timestep"))) }
			gapInfo <- gaps(blob$Data, internal.only=internalGapsOnly)
			ok <- (gapInfo$gap.length <= maxGapSteps)
			gapEnd <- gapInfo$gap.end[ok]
			gapLength <- gapInfo$gap.length[ok]
			gapStart <- gapEnd - gapLength + 1
			print(paste(length(gapEnd), "gaps"))
			# if there are not any more gaps, then stop
			if (length(gapEnd)==0) { break }
			# concatenated indices of all the time steps in gaps (whee!)
			allGaps <- sequence(gapLength) + rep(gapStart, times=gapLength) - 1
			
			if (method == "regression") {
				# Linear Model imputation:
				# start by predicting with as many sites as possible,
				# but some gaps will need to use fewer sites, so repeat
				
				# include this blob in the data frame in current form
				# (may have gaps already filled in, unlike rawBlobs)
				rawSync[[blobName]] <- blob$Data
				
				# find which other sites have data to fill in these gaps
				hasGapData <- !is.na(rawSync[allGaps, -c(1,blobIndex)])
				# find biggest set of sites which have simultaneous data
				# (so can use them all to predict)
				maxSet <- hasGapData[which.max(apply(hasGapData, 1, sum)),]
				print(paste("predicting with", sum(maxSet), "sites"))
				# if there are is more available data for imputing, then stop
				if (sum(maxSet) == 0) { break }
				predictors <- colnames(hasGapData)[maxSet]
				# construct model
				newFormula <- as.formula(paste(blobName, "~ 0 +", 
					paste(predictors, collapse=" + ")))
				print(newFormula)
				dataOK <- complete.cases(rawSync[c(blobName, predictors)])
				if (sum(dataOK)==0) {
					print("gah -- suddenly no data here")
					next
				}
				newModel <- lm(newFormula, rawSync[dataOK,])#, na.action=na.omit)
				#print(summary(newModel))
				#print(summary(step(newModel, k=log(sum(dataOK)), trace=0)))
				gapPred <- predict(newModel, newdata=rawSync[allGaps,])
				# limit below by zero; but should not have negative coef! (TODO)
				gapPred <- pmax(0, gapPred)
				gapPredOK <- !is.na(gapPred)
				print(paste("predicted", sum(gapPredOK), "out of", length(allGaps), "timesteps"))
				# TODO: store upper and lower confidence bounds in extra columns
				blob$Data[ allGaps[gapPredOK] ] <- gapPred[gapPredOK]
				blob$Qual[ allGaps[gapPredOK] ] <- "imputed"
			}
			
			if (method == "correlation") {
				# each gap: find predictor sites (complete coverage...)
				
				# calculate scaling matrix:
				# ratio of some function (eg mean) between each pair of sites
				# (using only pairwise-complete observations)
				# (be sure to account for AccumSteps in calculating mean)
				
				
			}
			
			if (method == "constant") {
				val <- NA
				if (constant %in% c("mean", "trim.mean")) {
					if (constant == "mean") { trim <- 0 }
					if (doAccum) {
						accums <- (blob$AccumSteps > 1)
						allVals <- with(blob, c(
							Data[!is.na(Data)],
							rep(AccumVal[accums] / AccumSteps[accums],
								times=AccumSteps[accums])
						))
						val <- mean(allVals, trim=trim)
					} else {
						val <- mean(blob$Data, trim=trim, na.rm=T)
					}
				}
				if (constant == "zero") {
					val <- 0
				}
				if (constant == "extend") {
					# find value preceding each gap
					# if data begins with gap, just use 0
					gapAnte <- blob$Data[ifelse(gapStart==1,1,gapStart-1)]
					gapAnte[is.na(gapAnte)] <- 0
					val <- rep(gapAnte, times=gapLength)
				}
				blob$Data[allGaps] <- val
				blob$Qual[allGaps] <- "imputed"
			}
		}
		
		if (doAccum) {
			accumSteps <- blob$AccumSteps
			accumVal <- blob$AccumVal
			spanEnd <- which(blob$AccumSteps > 1)
			spanLength <- blob$AccumSteps[spanEnd]
			spanStart <- spanEnd - spanLength + 1
			spanAccum <- blob$AccumVal[spanEnd]
			cumSum <- cumsum(ifelse(is.na(blob$Data), 0, blob$Data))
			spanSum <- cumSum[spanEnd] - 
				ifelse(spanStart==0, 0, cumSum[spanStart-1])
			
			# TODO: check whether the whole gap was filled in;
			#       otherwise cancel it
			
			# concatenated indices of all the time steps in accums (whee!)
			allSpans <- sequence(spanLength) + rep(spanStart, times=spanLength) - 1
			
			spanReScale <- rep(spanAccum / spanSum, times=spanLength)
			blob$Data[allSpans] <- blob$Data[allSpans] * spanReScale
			blob$Qual[allSpans] <- "disaccumulated"
			blob$AccumSteps[allSpans] <- 1
		}
		
		# update the copy of original data ('newBlobs')
		levels(newBlobs[[blobName]]$Qual) <- union(
			levels(newBlobs[[blobName]]$Qual), levels(blob$Qual[,drop=T]))
		# find subscripts in original data for specified 'timelim'
		blobWindow <- if (is.null(subLims)) {
			seq(along=newBlobs[[blobName]]$Data)
		} else {
			seq(subLims[[blobName]][1], subLims[[blobName]][2])
		}
		newBlobs[[blobName]]$Data[blobWindow] <- blob$Data
		newBlobs[[blobName]]$Qual[blobWindow] <- blob$Qual
		if (disaccumulate == "column") {
			newBlobs[[blobName]][[accumStepsColumn]][blobWindow] <- blob$AccumSteps
		}
	}
	
	return(newBlobs)
}

unimpute.timeblobs <- function(blob.list, timelim=NULL, type=c("imputed", "disaccumulated")) {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	if (!is.null(timelim)) {
		timelim <- as.POSIXct(timelim)
		if (any(is.na(timelim))) { stop("'timelim' must be a pair of valid times (POSIXt)") }
	}
	type <- match.arg(type, several.ok=T)
	
	for (x in names(blob.list)) {
		if (!("imputed" %in% type)) { break }
		imputed <- (blob.list[[x]]$Qual == "imputed")
		imputed[is.na(imputed)] <- F
		if (!is.null(timelim)) {
			# restrict to be within time window
			lim <- window(blob.list[[x]], timelim[1], timelim[2], 
				return.indices=T)
			iRow <- seq(along=blob.list[[x]]$Data)
			imputed <- imputed & (lim[1] <= iRow) & (iRow <= lim[2])
		}
		# set imputed values back to NA
		blob.list[[x]]$Data[imputed] <- NA
		blob.list[[x]]$Qual[imputed] <- "good" # or whatever
	}
	
	for (x in names(blob.list)) {
		if (!("disaccumulated" %in% type)) { break }
		accumd <- (blob.list[[x]]$Qual == "disaccumulated")
		accumd[is.na(accumd)] <- F
		if (!is.null(timelim)) {
			# restrict to be within time window
			lim <- window(blob.list[[x]], timelim[1], timelim[2], 
				return.indices=T)
			iRow <- seq(along=blob.list[[x]]$Data)
			accumd <- accumd & (lim[1] <= iRow) & (iRow <= lim[2])
		}
		# indices and lengths of strings of consecutive "disaccumulated"
		spanInfo <- gaps(ifelse(accumd, NA, T), internal.only=F)
		spanEnd <- spanInfo$gap.end
		spanLength <- spanInfo$gap.length
		spanStart <- spanEnd - spanLength + 1
		spanSum <- mapply(
			function(start,len) {
				sum(blob.list[[x]]$Data[seq(start,length=len)])
			}, spanStart, spanLength)
		# set values back to NA
		blob.list[[x]]$Data[accumd] <- NA
		blob.list[[x]]$Qual[accumd] <- "good" # or whatever
		# set accum value
		blob.list[[x]]$Data[spanEnd] <- spanSum
		blob.list[[x]]$Qual[spanEnd] <- "suspect" # or whatever
	}
	
	# remove quality code levels if possible
	for (x in names(blob.list)) {
		blob.list[[x]]$Qual <- blob.list[[x]]$Qual[,drop=T]
	}
	
	return(blob.list)
}

running.average.timeblob <- function(blob, by="1 year", max.na.proportion=0.05) {
	# check types
	if (!is.timeblob(blob)) { stop("'blob' must be a timeblob") }
	# find expected number of timesteps in smoothing kernel window
	delta <- as.numeric.byString(attr(blob, "timestep"))
	smoothDelta <- as.numeric.byString(by)
	winSize <- round(smoothDelta / delta)
	if (winSize <= 1) {
		stop("'by' must be a longer interval than 'blob' timestep")
	}
	winRear <- ceiling(winSize/2)
	winFore <- floor(winSize/2)
	# do smoothing
	cumSum <- blob$Data
	cumSum[is.na(cumSum)] <- 0
	cumSum <- cumsum(cumSum)
	cumNAs <- cumsum(is.na(blob$Data))
	newBlob <- blob[,1:2]
	newBlob$Data <- NA
	winNAs <- cumNAs[seq(1+winSize,nrow(blob))] - cumNAs[seq(1,nrow(blob)-winSize)]
	winSum <- cumSum[seq(1+winSize,nrow(blob))] - cumSum[seq(1,nrow(blob)-winSize)]
	winSum[winNAs > max.na.proportion * winSize] <- NA
	newBlob[seq(1+winRear,nrow(blob)-winFore)] <- winSum / (winSize - winNAs)
	return(newBlob)
}

## general functions for time series as numeric vectors

gaps <- function(x, internal.only=T) {
	seriesNA <- is.na(x)
	# diffNA is 1 at start of gap, -1 at end of gap, 0 otherwise
	diffNA <- c(0, diff(seriesNA))
	preDataGap <- match(F, seriesNA) - 1
	postDataGap <- match(F, rev(seriesNA)) - 1
	# so we don't detect a gap-end at start of data:
	diffNA[preDataGap+1] <- 0
	# find indices where NA is followed by data
	gapEnd <- which(diffNA==-1) - 1
	nGaps <- length(gapEnd)
	naCumSum <- cumsum(seriesNA)
	gapLength <- naCumSum[gapEnd] - 
		naCumSum[c(1,gapEnd[-nGaps])]
	if (internal.only) {
		return(list(gap.length=gapLength, gap.end=gapEnd, 
			pre.data=preDataGap, post.data=postDataGap))
	} else {
		gapLength <- c(if (preDataGap>0) { preDataGap },
			gapLength, if (postDataGap>0) { postDataGap })
		gapEnd <- c(if (preDataGap>0) { preDataGap },
			gapEnd, if (postDataGap>0) { length(x) })
		return(list(gap.length=gapLength, gap.end=gapEnd))
	}
}

peaks <- function(x) {
	xBackDiff <- c(NA, diff(x)) # backwards difference
	xFwdDiff <- c(xBackDiff[-1], NA) # forwards difference
	peakIdx <- which((xBackDiff > 0) & (xFwdDiff <= 0))
	peakIdx
}

rises <- function(x) {
	# backwards difference, i.e. rises are placed at their end time
	c(NA, pmax(0, diff(x)))
}

rises_old <- function(x) {
	xBackDiff <- c(NA, diff(x)) # backwards difference
	#xFwdDiff <- c(xBackDiff[-1], NA) # forwards difference
	#peaksIdx <- which((xBackDiff > epsilon) & (xFwdDiff < -epsilon))
	
	# TODO: find start of rise and take increase
	isRising <- (xBackDiff > 0)
	isRising[is.na(isRising)] <- F # take NAs as non-rises
	isFalling <- (xBackDiff < 0)
	isFalling[is.na(isFalling)] <- F # take NAs as non-falls
	# total rises and falls
	cumRise <- cumsum(ifelse(isRising, xBackDiff, 0))
	cumFall <- cumsum(ifelse(isFalling, xBackDiff, 0))
	# isRisingFwdDiff: is 1 at start of rise, -1 at end of rise
	isRisingFwdDiff <- c(diff(isRising), NA)
	# find when rises end (may or may not be a peak)
	stopIdx <- which(isRisingFwdDiff == -1)
	stopIdx_prev <- c(1, stopIdx[-length(stopIdx)])
	stopIdx_next <- c(stopIdx[-1], length(x))
	
	rise <- cumRise[stopIdx] - cumRise[stopIdx_prev]
	
	#peakIdx <- which(
	#	((cumRise[stops] - cumRise[stops_prev]) > epsilon) &
	#	((cumFall[stops] - cumFall[stops_next]) > epsilon)
	#)
	
	#peakIdx <- which(isRisingBackDiff == -1)
	#prevPeakIdx <- c(1, peakIdx[-length(peakIdx)])
	#cumRise <- cumsum(ifelse(isRising, xBackDiff, 0))
	#rise <- cumRise[peakIdx] - cumRise[prevPeakIdx]
	#if (!is.na(epsilon)) {
	#	ok <- (rise >= epsilon)
	#	rise <- rise[ok]
	#	peakIdx <- peakIdx[ok]
	#}
	return(list(rise=rise, peak.index=stopIdx))
}


lastTime <- function(x) {
	x[is.na(x)] <- F
	theTimes <- which(x == TRUE)
	preInter <- theTimes[1] - 1
	finalInter <- length(x) - theTimes[length(theTimes)] + 1
	interTimes <- c(diff(theTimes), finalInter)
	lastTime <- c(rep(NA, preInter), rep(theTimes, times=interTimes))
	lastTime
}


## other useful functions not specific to timeblobs

as.byString <- function(x, digits=getOption("digits"), explicit=F) {
	if (!identical(class(x), "difftime")) {
		if (!is.numeric(x)) { stop("'x' must be difftime or numeric") }
		x <- diff(as.POSIXct(c(0,x)))
	}
	it <- paste(format(unclass(x), digits=digits), attr(x, "units"))
	x <- as.numeric.byString(x)
	if (x > 28*24*60*60) {
		it <- paste(round(x / (30*24*60*60)), "months")
	}
	if (x > 363*24*60*60) {
		it <- paste(round(x / (365*24*60*60)), "years")
	}
	if (!explicit) { it <- sub("^1 ", "", it) }
	it <- sub(" day", " DSTday", it)
	return(it)
}

as.numeric.byString <- function(x) {
	if (identical(x, "irregular")) { return(0) }
	timeseq <- seq.POSIXt(from=ISOdate(1970,1,1,0,0,0), by=x, length=2)
	return(as.numeric(timeseq[2]) - as.numeric(timeseq[1]))
}

as.POSIXct.numeric <- function(secs_since_1970, tz="") {
	if (!is.numeric(secs_since_1970)) {
		stop("'secs_since_1970' must be numeric")
	}
	secs_since_1970 <- as.numeric(secs_since_1970)
	class(secs_since_1970) <- c("POSIXt", "POSIXct")
	attr(secs_since_1970, "tzone") <- tz
	return(secs_since_1970)
}

trunc.month <- function(thisPOSIXt) {
	zz <- as.POSIXlt(thisPOSIXt)
	zz$mday <- 1
        zz$hour <- zz$min <- zz$sec <- 0
	zz$isdst <- -1
	return(zz)
}

trunc.year <- function(thisPOSIXt) {
	zz <- as.POSIXlt(thisPOSIXt)
	zz$mday <- 1
	zz$mon <- 0
        zz$hour <- zz$min <- zz$sec <- 0
	zz$isdst <- -1
	return(zz)
}

trunc.decade <- function(thisPOSIXt) {
	zz <- as.POSIXlt(trunc.year(thisPOSIXt))
	zz$year <- (zz$year %/% 10) * 10
	return(zz)
}

one.step.acf <- function(blob) {
	acf(blob$Data, na.action=na.pass, lag.max=1, plot=F)$acf[2]
}

