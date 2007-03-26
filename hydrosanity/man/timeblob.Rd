\name{timeblob}
\alias{timeblob}
\alias{is.timeblob}
\alias{lapply.timeblob.data}
\alias{sapply.timeblob.data}
\title{Time Series Object (Timeblob) with Times and Data}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
timeblob(Time, Data, Qual=NULL, extras=NULL, timestep=NULL, dataname=NULL)

is.timeblob(x)

lapply.timeblob.data(blob.list, FUN, ...)
sapply.timeblob.data(blob.list, FUN, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Time}{ ~~Describe \code{Time} here~~ }
  \item{Data}{ ~~Describe \code{Data} here~~ }
  \item{Qual}{ ~~Describe \code{Qual} here~~ }
  \item{extras}{ ~~Describe \code{extras} here~~ }
  \item{timestep}{ ~~Describe \code{timestep} here~~ }
  \item{dataname}{ ~~Describe \code{dataname} here~~ }
  \item{blob.list}{ ~~Describe \code{blob.list} here~~ }
  \item{FUN}{ ~~Describe \code{FUN} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(file, skip=1, sep=",", dataName="Data", dataCol=2, qualCol=3, extraCols=c(), extraNames=paste("Extra",extraCols), readTimesFromFile=T, timeCol=1, timeFormat="\%d \%b \%Y", startTime=NA, timeSeqBy="DSTday", ...) {
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
		if ("list" \%in\% class(startTime)) {
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
	# trim by 10\% because of anomolies in seq.POSIXt with DSTdays
	timeStepRange <- c(quantile(timeStepDiffs, c(0.1, 0.9)))
	if (timeStepRange[2] > 1.11 * timeStepRange[1]) {
		# up to 11\% difference expected in regular series (feb vs jan)
		attr(blob, "timestep") <- "inst"
	}
	return(blob)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
