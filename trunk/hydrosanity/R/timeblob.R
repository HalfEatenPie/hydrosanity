## Hydrosanity: an interface for exploring hydrological time series in R
##
## Time-stamp: <2007-03-05 00:00:00 Felix>
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL


is.timeblob <- function(x) {
	return (!is.null(x) &&
		(is.data.frame(x)) &&
		(ncol(x) >= 3) &&
		inherits(x[1,1], "POSIXct") &&
		inherits(x[1,2], "numeric") &&
		(!is.null(x$Time)) &&
		(!is.null(x$Qual)))
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
# startTime=list(year=1,month=2,day=3,etc) where these give column numbers to read the start time from the first line in dataFile
read.timeblob <- function(dataFile, skip=1, sep=",", dataName="Data", dataCol=2, qualCol=3, extraCols=c(), extraNames=paste("Extra",extraCols), readTimesFromFile=T, timeCol=1, timeFormat="%d %b %Y", startTime=NA, timeSeqBy="DSTday", ...) {
	# check types
	if (!is.numeric(dataCol)) { stop("dataCol must be numeric (column number)") }
	if (readTimesFromFile) {
		if (!is.numeric(timeCol)) { stop("timeCol must be numeric (column number)") }
	} else {
		if (!is.list(startTime)) {
			startTime <- as.POSIXct(startTime)
			if (is.na(startTime)) { stop("could not convert startTime to a time") }
		}
	}
	# make sure extra column names correspond to given columns
	length(extraNames) <- length(extraCols)
	extraNames[is.na(extraNames)] <- paste("Extra", which(is.na(extraNames)))
	# number of columns in file
	firstLine <- read.table(dataFile, header=F, skip=skip, sep=sep, strip.white=T, nrows=1, ...)
	dataFileCols <- ncol(firstLine)
	if (dataCol > dataFileCols) {
		stop("Column ", dataCol, " (dataCol) not found on line ", 
		skip+1, "; maybe the column separator \"", sep, "\" is wrong?")
	}
	# drop variables for which column does not exist in file
	if (qualCol > dataFileCols) { qualCol <- NULL }
	extraNames <- extraNames[!(extraCols > dataFileCols)]
	extraCols <- extraCols[!(extraCols > dataFileCols)]
	# define which columns to import and which to ignore
	dataFileColClasses <- rep("NULL", dataFileCols)
	if (readTimesFromFile) { dataFileColClasses[timeCol] <- "character" }
	dataFileColClasses[dataCol] <- "numeric" #, but then "" gives error
	dataFileColClasses[qualCol] <- NA # qualCol may be NULL
	dataFileColClasses[extraCols] <- NA # extraCols may be NULL
	# read file
	rawData <- read.table(dataFile, header=F, skip=skip, sep=sep, colClasses=dataFileColClasses, strip.white=T, ...)
	# work out which column of rawData has the data (from dataCol)
	dataIndex <- dataCol - sum(dataFileColClasses[1:dataCol]=="NULL", na.rm=T)
	qualIndex <- NULL
	timeIndex <- NULL
	# extract quality codes or set to default (factor("NA"))
	myQual <- NA
	if (!is.null(qualCol) && !is.na(qualCol)) {
		qualIndex <- qualCol - sum(dataFileColClasses[1:qualCol]=="NULL", na.rm=T)
		myQual <- factor(rawData[,qualIndex], exclude=NULL)
	} else {
		myQual <- rep(factor(NA, exclude=NULL), nrow(rawData))
	}
	# convert or construct the time sequence
	myTime <- NA
	if (readTimesFromFile) {
		timeIndex <- timeCol - sum(dataFileColClasses[1:timeCol]=="NULL", na.rm=T)
		myTime <- strptime(rawData[,timeIndex], format=timeFormat)
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
				stop('could not construct starting time from columns given in startTime: ', myBits)
			}
		}
		myTime <- seq.POSIXt(from=startTime, by=timeSeqBy, length.out=nrow(rawData))
	}
	# make sure first three columns are Time, Data, Qual (in that order)
	myBlob <- data.frame(Time=myTime, Data=rawData[,dataIndex], Qual=myQual, 
		rawData[,-c(timeIndex, dataIndex, qualIndex)])
	names(myBlob) <- c("Time", dataName, "Qual", extraNames)
	attr(myBlob, "timestep") <- difftimeString(myBlob$Time[2] - myBlob$Time[1], digits=1)
	return(myBlob)
}


start.timeblob <- function(myBlob) {
	if (is.data.frame(myBlob)) { myBlob <- list(myBlob) }
	globalStart <- as.POSIXct.raw(min(sapply(myBlob, function(x) {return(x$Time[1])})))
	return(globalStart)
}


end.timeblob <- function(myBlob) {
	if (is.data.frame(myBlob)) { myBlob <- list(myBlob) }
	globalEnd <- as.POSIXct.raw(max(sapply(myBlob, function(x) {return(x$Time[nrow(x)])})))
	return(globalEnd)
}


window.timeblob <- function(myBlob, start, end, inclusive=F) {
	start <- as.POSIXct(start)
	end <- as.POSIXct(end)
	
	if (start > end.timeblob(myBlob)) {
		# entire window is after end
		return(myBlob[0,])
	}
	if (end < start.timeblob(myBlob)) {
		# entire window is before start
		return(myBlob[0,])
	}
	
	windowIdx <- findInterval(c(start,end), myBlob$Time)
	
	if (inclusive == F) {
		testDate <- myBlob$Time[windowIdx[1]]
		if ((length(testDate) > 0) && (testDate != start) && (windowIdx[1] < nrow(myBlob))) {
			# round up at start (findInterval rounds down)
			windowIdx[1] <- windowIdx[1] + 1
		}
	} else {
		testDate <- myBlob$Time[windowIdx[2]]
		if ((length(testDate) > 0) && (testDate != end) && (windowIdx[2] < nrow(myBlob))) {
			# round up at end (findInterval rounds down)
			windowIdx[2] <- windowIdx[2] + 1
		}
	}
	return(myBlob[seq(windowIdx[1],windowIdx[2]),])
}


# invisibly returns missing fraction for each series
summary.missing.timeblob.list <- function(blobs, startTime=start.timeblob(blobs), endTime=end.timeblob(blobs), timeStep=hsp$timeStep) {
	startTime <- as.POSIXct(startTime)
	endTime <- as.POSIXct(endTime)
	subBlobs <- lapply(blobs, window.timeblob, startTime, endTime)
	myN <- length(subBlobs)
	myPeriod <- seq.POSIXt(startTime, endTime, by=timeStep)
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
	allActiveSteps <- sum(activeNs == myN)
	allActiveFrac <- allActiveSteps / length(myPeriod)
	activeNQ <- quantile(activeNs, probs=c(0.25, 0.5, 0.75))
	activeNQFrac <- activeNQ / myN
	
	cat(sprintf('Overall, %.0f%% of data is missing.\n', overallDataFrac*100))
	cat(sprintf('There are %i time series, of which %i %s complete.\n', 
		myN, myCompleteN, ifelse(myCompleteN==1,'is','are')))
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

# find indices into timeblob x for each time in myPeriod
# values of NA are used for times outside the timeblob
#
# it resamples x$Time to correspond to each time in myPeriod
#
matchperiod.timeblob <- function(x, myPeriod) {
	# check types
	x <- as.timeblob(x)
	# default for indices is NA
	periodIndices <- rep(as.integer(NA), length(myPeriod))
	# each blob here may be outside myPeriod, and may be empty
	if ((nrow(x)==0)
	  || (start.timeblob(x) > myPeriod[length(myPeriod)])
	  || (end.timeblob(x) < myPeriod[1])) {
		return(periodIndices)
	}
	# each blob here is not entirely outside myPeriod
	blobBounds <- findInterval(c(start.timeblob(x), end.timeblob(x)), myPeriod)
	blobWindowIndices <- seq(blobBounds[1], blobBounds[2])
	# now myPeriod[blobWindowIdx] is not outside x$Time
	periodIndices[blobWindowIndices] <- findInterval(myPeriod[blobWindowIndices], x$Time)
	return(periodIndices)
}


gaps <- function() {
	
}


# TODO: how to handle other columns, e.g. quality codes?
# this only handles regular series (the calculation of NA proportion requires it)
# note that "from" and "to" refer to when the aggregated periods begin (e.g. for annual aggregation, the 1st day of each year)

# column 3 = "Quality (mode)"; cols 4+ = "%good", "%maybe", "%poor", "%disaccumulated", "%imputed"
aggregate.timeblob <- function(myBlob, by="1 year", from=floor.year(start.timeblob(myBlob)), to=end.timeblob(myBlob), max.na.proportion=0.05, aggrFun=mean) {
	from <- as.POSIXct(from)
	to <- as.POSIXct(to)
	fromPlusOne <- seq.POSIXt(from, by=by, length=2)[2]
	newDelta <- as.numeric(fromPlusOne) - as.numeric(from)
	oldDelta <- as.numeric(myBlob$Time[2]) - as.numeric(myBlob$Time[1])
	freqN <- trunc(newDelta / oldDelta)
	
	subBlob <- window.timeblob(myBlob, from, to)
	dateGroups <- cut.POSIXt(subBlob$Time, breaks=by)
	newDates <- as.POSIXct(levels(dateGroups))
	
	newVals <- aggregate(subBlob[,2], by=list(dateGroups), FUN=aggrFun, na.rm=T)[,2]
	eachNA <- aggregate(subBlob[,2], by=list(dateGroups), FUN=function(x) {sum(is.na(x))})[,2]
	newVals[eachNA > freqN * max.na.proportion] <- NA
	firstN <- sum(unclass(dateGroups)==1, na.rm=T)
	if (firstN < freqN * (1-max.na.proportion)) {
		newVals[1] <- NA
	}

	#newVals[eachN < freqN * (1-max.na.proportion)] <- NA
	
	#newDates <- seq.POSIXt(from, to, by=by)
	#newN <- length(newDates)
	#newVals <- rep(as.numeric(NA), newN)
	#dateGroups <- cut.POSIXt(myBlob$Time, breaks=by)
	
	#breaksIdx <- findInterval(newDates, myBlob$Time)
	# forget about new dates preceding data (before Time[1]), where breaksIdx == 0
	#preTimes <- sum(breaksIdx==0)

	#dateGroups <- cut.POSIXt(myBlob$Time, breaks=myBlob$Time[breaksIdx])
	
	
	
	#breaksEachN <- diff(breaksIdx)
	#breaksEachN[newN] <- nrow(myBlob) - breaksEachN[newN-1]
	#dateGroups <- rep(seq(1,newN), times=breaksEachN)
	
	newBlob <- data.frame(newDates, newVals)
	names(newBlob) <- names(myBlob)[1:2]
	return(newBlob)
}


smooth.timeblob <- function(myBlob, by="1 year", max.na.proportion=0.05) {
	periodSeq <- seq.POSIXt(start.timeblob(myBlob), by=by, length=2)
	sampleWindow <- window.timeblob(myBlob, periodSeq[1], periodSeq[2])
	winSize <- nrow(sampleWindow)
	winRear <- ceiling(winSize/2)
	winFore <- floor(winSize/2)
	cumSum <- myBlob[,2]
	cumSum[is.na(cumSum)] <- 0
	cumSum <- cumsum(cumSum)
	cumNAs <- cumsum(is.na(myBlob[,2]))
	newBlob <- myBlob[,c(1,2)]
	newBlob[,2] <- NA
	winNAs <- cumNAs[seq(1+winSize,nrow(myBlob))] - cumNAs[seq(1,nrow(myBlob)-winSize)]
	winSum <- cumSum[seq(1+winSize,nrow(myBlob))] - cumSum[seq(1,nrow(myBlob)-winSize)]
	winSum[winNAs > max.na.proportion * winSize] <- NA
	newBlob[seq(1+winRear,nrow(myBlob)-winFore)] <- winSum / (winSize - winNAs)
	return(newBlob)
}


range.timeblob <- function(myBlob, ...) {
	return(range(myBlob[,2], ...))
}


get.year <- function(thisPOSIXt) {
	return(as.POSIXlt(thisPOSIXt)$year + 1900)
}


floor.year <- function(thisPOSIXt) {
	zz <- as.POSIXlt(thisPOSIXt)
	zz$mday <- 1
	zz$mon <- 0
        zz$isdst <- zz$hour <- zz$min <- zz$sec <- 0
	return(zz)
}


ceiling.year <- function(thisPOSIXt) {
	zz <- as.POSIXlt(floor.year(thisPOSIXt))
	zz$year <- zz$year + 1
	return(zz)
}


floor.month <- function(thisPOSIXt) {
	zz <- as.POSIXlt(thisPOSIXt)
	zz$mday <- 1
        zz$isdst <- zz$hour <- zz$min <- zz$sec <- 0
	return(zz)
}

difftimeString <- function(x, digits=getOption("digits")) {
	paste(format(unclass(x), digits=digits), attr(x, "units"))
}


as.POSIXct.raw <- function(secs_since_1970, tz="") {
	class(secs_since_1970) <- c("POSIXt", "POSIXct")
	attr(secs_since_1970, "tzone") <- tz
	return(secs_since_1970)
}

