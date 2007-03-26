## Hydrosanity: an interface for exploring hydrological time series in R
##
## Time-stamp: <2007-03-05 00:00:00 Felix>
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

redistributeAccumulatedRainfall <- function() {
	accums <- which(thisBlob[,"Accum days"] > 1)
	#cat(length(accums), "multi-day accumulations, at positions: ", accums, "\n")
	if (redistributeAccumulatedRainfall) {
		for (i in accums) {
			iAccum <- thisBlob[i, "Accum days"]
			iRains <- thisBlob[i, "Rain days"]
			iQual <- thisBlob[i, "Qual"]
			if (is.na(iRains)) { iRains <- iAccum }
			eachRain <- thisBlob[i,2] / iRains
			thisBlob[seq(i-iAccum+1,i),2] <- 0
			thisBlob[seq(i-iAccum+1,i),"Qual"] <- "S"
			#thisBlob[i,c("Accum days","Rain days")] <- 1
			thisBlob[(i-iAccum)+(1:iRains),2] <- eachRain
		}
	}
	thisBlob[["Rain days"]] <- NULL
	thisBlob[["Accum days"]] <- NULL
	return(thisBlob)
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
	blob$Time[nrow(blob)]
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


window.timeblob <- function(blob, start, end, inclusive=F, extend=F) {
	# check types
	if (!is.timeblob(blob)) { stop("'blob' must be a timeblob") }
	start <- as.POSIXct(start)
	end <- as.POSIXct(end)
	if (any(is.na(c(start, end)))) { stop("'start' and 'end' must be valid times (POSIXt)") }
	if (extend) {
		timestep <- attr(blob, "timestep")
		if (is.null(timestep)) { stop("'blob' needs a timestep attribute for 'extend=T'") }
		negTimestep <- paste("-1", timestep)
		if (length(grep("^[0-9]", timestep)>0)) {
			negTimestep <- paste("-", timestep, sep='')
		}
		if (start < start.timeblob(blob)) {
			extendTimes <- seq.POSIXt(start.timeblob(blob), start, by=negTimestep)[-1]
			extendTimes <- rev(extendTimes)
			extendBlob <- blob[c(0,rep(NA,length(extendTimes))),]
			extendBlob$Time <- extendTimes
			blob <- rbind(extendBlob, blob)
		}
		if (end > end.timeblob(blob)) {
			extendTimes <- seq.POSIXt(end.timeblob(blob), end, by=timestep)[-1]
			extendBlob <- blob[c(0,rep(NA,length(extendTimes))),]
			extendBlob$Time <- extendTimes
			blob <- rbind(blob, extendBlob)
		}
	}
	
	if (start > end.timeblob(blob)) {
		# entire window is after end
		return(blob[0,])
	}
	if (end < start.timeblob(blob)) {
		# entire window is before start
		return(blob[0,])
	}
	
	windowIdx <- findInterval(c(start,end), blob$Time)
	
	if (inclusive == F) {
		testDate <- blob$Time[windowIdx[1]]
		if ((length(testDate) > 0) && (testDate != start) && (windowIdx[1] < nrow(blob))) {
			# round up at start (findInterval rounds down)
			windowIdx[1] <- windowIdx[1] + 1
		}
	} else {
		testDate <- blob$Time[windowIdx[2]]
		if ((length(testDate) > 0) && (testDate != end) && (windowIdx[2] < nrow(blob))) {
			# round up at end (findInterval rounds down)
			windowIdx[2] <- windowIdx[2] + 1
		}
	}
	return(blob[seq(windowIdx[1],windowIdx[2]),])
}


# find indices into timeblob 'blob' for each time in 'times'
# values of NA are used for times outside 'blob'
#
# it resamples blob$Time to correspond to each time in 'times'
#
matchtimes.timeblob <- function(blob, times) {
	# check types
	if (!is.timeblob(blob)) { stop("'blob' must be a timeblob") }
	times <- as.POSIXct(times)
	if (any(is.na(times))) { stop("'times' must be a vector of valid times (POSIXt)") }
	# default for indices is NA
	periodIndices <- rep(as.integer(NA), length(times))
	# each blob here may be outside 'times', and may be empty
	if ((nrow(blob)==0)
	  || (start.timeblob(blob) > times[length(times)])
	  || (end.timeblob(blob) < times[1])) {
		return(periodIndices)
	}
	# each blob here is not entirely outside 'times'
	blobBounds <- findInterval(c(start.timeblob(blob), end.timeblob(blob)), times)
	# make sure blobBounds are really within blob$Time (findInterval rounds down)
	if (times[blobBounds[1]] < start.timeblob(blob)) {
		blobBounds[1] <- blobBounds[1] + 1
	}
	blobWindowIndices <- seq(blobBounds[1], blobBounds[2])
	# now times[blobWindowIndices] is not outside blob$Time
	periodIndices[blobWindowIndices] <- findInterval(times[blobWindowIndices], blob$Time)
	return(periodIndices)
}


sync.timeblobs <- function(blob.list, timestep=NULL, timelim=NULL, extractColumn=2) {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	if (is.null(timelim)) {
		timelim <- c(start.timeblobs(blob.list), end.timeblobs(blob.list))
	} else {
		timelim <- as.POSIXct(timelim)
		if (any(is.na(timelim))) { stop("'timelim' must be a pair of valid times (POSIXt)") }
		blob.list <- lapply(blob.list, window.timeblob, min(timelim), max(timelim))
	}
	# setup
	n <- length(blob.list)
	if (is.null(timestep)) {
		timestep <- common.timestep.timeblobs(blob.list)
	}
	times <- NULL
	times <- seq.POSIXt(min(timelim), max(timelim), by=timestep)
	syncData <- data.frame(
		Time=times,
		lapply(blob.list, function(x) {
			mySyncIndices <- matchtimes.timeblob(x, times)
			x[mySyncIndices,extractColumn]
		})
	)
	attr(syncData, "timestep") <- timestep
	return(syncData)
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
		blob.list <- lapply(blob.list, window.timeblob, timelim[1], timelim[2])
	}
	# setup
	n <- length(blob.list)
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
	allActiveSteps <- sum(activeNs == n)
	allActiveFrac <- allActiveSteps / length(times)
	activeNQ <- quantile(activeNs, probs=c(0.25, 0.5, 0.75))
	activeNQFrac <- activeNQ / n
	
	cat(sprintf('Overall, %.0f%% of data is missing.\n', (1-overallDataFrac)*100))
	cat(sprintf('There are %i time series, of which %i %s complete.\n', 
		n, myCompleteN, ifelse(myCompleteN==1,'is','are')))
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
aggregate.timeblob <- function(blob, by="1 year", FUN=NULL, max.na.proportion=0.05) {
	# check types
	if (!is.timeblob(blob)) { stop("'blob' must be a timeblob") }
	if (is.null(FUN)) {
		FUN <- mean
		if (identical(attr(blob, "role"), "RAIN")) {
			FUN <- sum
		}
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
	firstN <- sum(as.integer(dateGroups)==1, na.rm=T)
	if (firstN < freqN * (1-max.na.proportion)) {
		newVals[1] <- NA
	}
	# construct new blob
	newBlob <- timeblob(Time=newDates, Data=newVals,
		timestep=by, dataname=attr(blob, "dataname"))
	return(newBlob)
}


running.average.timeblob <- function(blob, by="1 year", max.na.proportion=0.05) {
	# check types
	if (!is.timeblob(blob)) { stop("'blob' must be a timeblob") }
	# find expected number of timesteps in smoothing kernel window
	delta <- as.numeric.byString(attr(blob, "timestep"))
	smoothDelta <- as.numeric.byString(by)
	winSize <- round(smoothDelta / delta)
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


## other useful functions not specific to timeblobs

as.byString <- function(x, digits=getOption("digits")) {
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
	it <- sub("^1 ", "", it)
	it <- sub(" day", " DSTday", it)
	return(it)
}

as.numeric.byString <- function(x) {
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

