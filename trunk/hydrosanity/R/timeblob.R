## Hydrosanity: an interface for exploring hydrological time series in R
##
## Time-stamp: <2007-03-05 00:00:00 Felix>
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL


is.timeblob <- function(x) {
	return (!is.null(x) &&
		is.data.frame(x) &&
		(ncol(x) >= 2) &&
		inherits(x[1,1], "POSIXct") &&
		inherits(x[1,2], "numeric") &&
		!is.null(x$Time) &&
		!is.null(attr(x, "timestep")))
}

as.timeblob <- function(x) {
	if (is.timeblob(x)) { return(x) }
	else { stop("do not know how to convert to timeblob") }
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

# if readTimesFromFile == FALSE then
# startTime should be able to be interpreted as a POSIXt, or else be a list like
# startTime=list(year=1,month=2,day=3,etc) where these give column numbers to read the start time from the first line in file
read.timeblob <- function(file, skip=1, sep=",", dataName="Data", dataCol=2, qualCol=3, extraCols=c(), extraNames=paste("Extra",extraCols), readTimesFromFile=T, timeCol=1, timeFormat="%d %b %Y", startTime=NA, timeSeqBy="DSTday", ...) {
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
	fileColClasses[dataCol] <- "numeric" #, but then "" gives error
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
	# make sure first three columns are Time, Data, Qual (in that order)
	blob <- data.frame(Time=myTime, Data=rawData[[dataIndex]], Qual=myQual, 
		rawData[,-c(timeIndex, dataIndex, qualIndex)])
	names(blob) <- c("Time", dataName, "Qual", extraNames)
	attr(blob, "timestep") <- as.byString(blob$Time[2] - blob$Time[1], digits=1)
	timeStepDiffs <- diff(as.numeric(blob$Time))
	# trim by 10% because of anomolies in seq.POSIXt with DSTdays
	timeStepRange <- c(quantile(timeStepDiffs, c(0.1, 0.9)))
	if (timeStepRange[2] > 1.11 * timeStepRange[1]) {
		# up to 11% difference expected in regular series (feb vs jan)
		attr(blob, "timestep") <- "inst"
	}
	return(blob)
}


start.timeblob <- function(blob) {
	if (!is.timeblob(blob)) { stop("'blob' must be a timeblob") }
	blob$Time[1]
}

end.timeblob <- function(blob) {
	if (!is.timeblob(blob)) { stop("'blob' must be a timeblob") }
	blob$Time[nrow(blob)]
}

start.timeblobs <- function(blob.list) {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	globalStart <- as.POSIXct.raw(min(sapply(blob.list, start.timeblob)))
	return(globalStart)
}

end.timeblobs <- function(blob.list) {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	globalEnd <- as.POSIXct.raw(max(sapply(blob.list, end.timeblob)))
	return(globalEnd)
}


window.timeblob <- function(blob, start, end, inclusive=F) {
	# check types
	if (!is.timeblob(blob)) { stop("'blob' must be a timeblob") }
	start <- as.POSIXct(start)
	end <- as.POSIXct(end)
	if (any(is.na(c(start, end)))) { stop("'start' and 'end' must be valid times (POSIXt)") }
	
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


# invisibly returns missing fraction for each series
summary.missing.timeblob.list <- function(blob.list, timelim=NULL, timeStep="1 DSTday") {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	if (is.null(timelim)) {
		timelim <- c(start.timeblobs(blob.list), end.timeblobs(blob.list))
	}
	timelim <- as.POSIXct(timelim)
	if (any(is.na(timelim))) { stop("'timelim' must be a pair of valid times (POSIXt)") }
	# setup
	n <- length(blob.list)
	subBlobs <- lapply(blob.list, window.timeblob, timelim[1], timelim[2])
	myPeriod <- seq.POSIXt(timelim[1], timelim[2], by=timeStep)
	myLengths <- sapply(subBlobs, nrow)
	myNAs <- lapply(subBlobs, function(x) { is.na(x[,2]) })
	myDataPoints <- myLengths - sapply(myNAs, sum)
	myDataFrac <- myDataPoints / length(myPeriod)
	overallDataFrac <- mean(myDataFrac)
	myCompleteN <- sum(myDataFrac >= 1)
	my95PctN <- sum(myDataFrac > 0.95)
	my75PctN <- sum(myDataFrac > 0.75)
	
	# find whether data exists for each timeblob for each time in myPeriod
	# (note: this rounds down if times do not match)
	dataExistsMatrix <- sapply(subBlobs, function(x) {
		myPeriodIndices <- matchperiod.timeblob(x, myPeriod)
		!is.na(x[myPeriodIndices,2])
	})
	
	activeNs <- apply(dataExistsMatrix, 1, sum)
	allActiveSteps <- sum(activeNs == n)
	allActiveFrac <- allActiveSteps / length(myPeriod)
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
	missingFrac <- (length(myPeriod) - apply(dataExistsMatrix, 2, sum)) / length(myPeriod)
	
	invisible(missingFrac)
}

# find indices into timeblob 'blob' for each time in refPeriod
# values of NA are used for times outside 'blob'
#
# it resamples blob$Time to correspond to each time in refPeriod
#
matchperiod.timeblob <- function(blob, refPeriod) {
	# check types
	if (!is.timeblob(blob)) { stop("'blob' must be a timeblob") }
	refPeriod <- as.POSIXct(refPeriod)
	if (any(is.na(refPeriod))) { stop("'refPeriod' must be a pair of valid times (POSIXt)") }
	# default for indices is NA
	periodIndices <- rep(as.integer(NA), length(refPeriod))
	# each blob here may be outside refPeriod, and may be empty
	if ((nrow(blob)==0)
	  || (start.timeblob(blob) > refPeriod[length(refPeriod)])
	  || (end.timeblob(blob) < refPeriod[1])) {
		return(periodIndices)
	}
	# each blob here is not entirely outside refPeriod
	blobBounds <- findInterval(c(start.timeblob(blob), end.timeblob(blob)), refPeriod)
	# make sure blobBounds are really within blob$Time (findInterval rounds down)
	if (refPeriod[blobBounds[1]] < start.timeblob(blob)) {
		blobBounds[1] <- blobBounds[1] + 1
	}
	blobWindowIndices <- seq(blobBounds[1], blobBounds[2])
	# now refPeriod[blobWindowIndices] is not outside blob$Time
	periodIndices[blobWindowIndices] <- findInterval(refPeriod[blobWindowIndices], blob$Time)
	return(periodIndices)
}


gaps <- function() {
	
}


# this only handles regular series (the calculation of NA proportion requires it)
# column 3 = "Quality (mode)"; cols 4+ = "%good", "%maybe", "%poor", "%disaccumulated", "%imputed"
aggregate.timeblob <- function(blob, by="1 year", FUN=mean, max.na.proportion=0.05) {
	# check types
	if (!is.timeblob(blob)) { stop("'blob' must be a timeblob") }
	# find expected number of old timesteps in each new timestep
	oldDelta <- as.numeric.byString(attr(blob, "timestep"))
	newDelta <- as.numeric.byString(by)
	freqN <- floor(newDelta / oldDelta)
	# construct groups
	dateGroups <- cut.POSIXt(blob$Time, breaks=by)
	newDates <- as.POSIXct(levels(dateGroups))
	# aggregate
	newVals <- aggregate(blob[,2], by=list(dateGroups), FUN=FUN, na.rm=T)[,2]
	# set NA values
	eachNA <- aggregate(blob[,2], by=list(dateGroups), FUN=function(x) {sum(is.na(x))})[,2]
	newVals[eachNA > freqN * max.na.proportion] <- NA
	firstN <- sum(as.integer(dateGroups)==1, na.rm=T)
	if (firstN < freqN * (1-max.na.proportion)) {
		newVals[1] <- NA
	}
	# construct new blob
	newBlob <- data.frame(newDates, newVals)
	names(newBlob) <- names(blob)[1:2]
	attr(newBlob, "timestep") <- by
	return(newBlob)
}


smooth.timeblob <- function(blob, by="1 year", max.na.proportion=0.05) {
	# check types
	if (!is.timeblob(blob)) { stop("'blob' must be a timeblob") }
	# find expected number of timesteps in smoothing kernel window
	delta <- as.numeric.byString(attr(blob, "timestep"))
	smoothDelta <- as.numeric.byString(by)
	winSize <- round(smoothDelta / delta)
	winRear <- ceiling(winSize/2)
	winFore <- floor(winSize/2)
	# do smoothing
	cumSum <- blob[,2]
	cumSum[is.na(cumSum)] <- 0
	cumSum <- cumsum(cumSum)
	cumNAs <- cumsum(is.na(blob[,2]))
	newBlob <- blob[,1:2]
	newBlob[,2] <- NA
	winNAs <- cumNAs[seq(1+winSize,nrow(blob))] - cumNAs[seq(1,nrow(blob)-winSize)]
	winSum <- cumSum[seq(1+winSize,nrow(blob))] - cumSum[seq(1,nrow(blob)-winSize)]
	winSum[winNAs > max.na.proportion * winSize] <- NA
	newBlob[seq(1+winRear,nrow(blob)-winFore)] <- winSum / (winSize - winNAs)
	return(newBlob)
}


range.timeblob <- function(blob, ...) {
	return(range(blob[,2], ...))
}

## other useful functions not specific to timeblobs

nonzeromin <- function(x, ...) { min(x[x>0], ...) }

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
	zz <- as.POSIXlt(floor.year(thisPOSIXt))
	zz$year <- (zz$year %/% 10) * 10
	return(zz)
}

as.byString <- function(x, digits=getOption("digits")) {
	it <- paste(format(unclass(x), digits=digits), attr(x, "units"))
	sub(" day", " DSTday", it)
}

as.byString.numeric <- function(x) {
	as.byString(diff(as.POSIXct.raw(c(0,x))))
}

as.numeric.byString <- function(x) {
	timeseq <- seq.POSIXt(from=ISOdate(1970,1,1), by=x, length=2)
	return(as.numeric(timeseq[2]) - as.numeric(timeseq[1]))
}

as.POSIXct.raw <- function(secs_since_1970, tz="") {
	if (!is.numeric(secs_since_1970)) {
		stop("'secs_since_1970' must be numeric")
	}
	class(secs_since_1970) <- c("POSIXt", "POSIXct")
	attr(secs_since_1970, "tzone") <- tz
	return(secs_since_1970)
}

