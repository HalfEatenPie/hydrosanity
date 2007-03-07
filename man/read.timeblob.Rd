\name{read.timeblob}
\alias{read.timeblob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.timeblob(dataFile, skip = 1, sep = ",", dataName = "Data", dataCol = 2, qualCol = 3, extraCols = c(), extraNames = paste("Extra", extraCols), readTimesFromFile = T, timeCol = 1, timeFormat = "%d %b %Y", startTime = NA, timeSeqBy = "DSTday", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataFile}{ ~~Describe \code{dataFile} here~~ }
  \item{skip}{ ~~Describe \code{skip} here~~ }
  \item{sep}{ ~~Describe \code{sep} here~~ }
  \item{dataName}{ ~~Describe \code{dataName} here~~ }
  \item{dataCol}{ ~~Describe \code{dataCol} here~~ }
  \item{qualCol}{ ~~Describe \code{qualCol} here~~ }
  \item{extraCols}{ ~~Describe \code{extraCols} here~~ }
  \item{extraNames}{ ~~Describe \code{extraNames} here~~ }
  \item{readTimesFromFile}{ ~~Describe \code{readTimesFromFile} here~~ }
  \item{timeCol}{ ~~Describe \code{timeCol} here~~ }
  \item{timeFormat}{ ~~Describe \code{timeFormat} here~~ }
  \item{startTime}{ ~~Describe \code{startTime} here~~ }
  \item{timeSeqBy}{ ~~Describe \code{timeSeqBy} here~~ }
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
function(dataFile, skip=1, sep=",", dataName="Data", dataCol=2, qualCol=3, extraCols=c(), extraNames=paste("Extra",extraCols), readTimesFromFile=T, timeCol=1, timeFormat="\%d \%b \%Y", startTime=NA, timeSeqBy="DSTday", ...) {
	# check types
	if (!is.numeric(dataCol)) { stop("dataCol must be numeric (column number)") }
	if (readTimesFromFile && !is.numeric(timeCol)) { stop("timeCol must be numeric (column number)") }
	# make sure extra column names correspond to given columns
	length(extraNames) <- length(extraCols)
	extraNames[is.na(extraNames)] <- paste("Extra", which(is.na(extraNames)))
	# number of columns in file
	firstLine <- read.table(dataFile, header=F, skip=skip, sep=sep, strip.white=T, nrows=1)
	dataFileCols <- ncol(firstLine)
	#dataFileCols <- length(gregexpr(sep, readLines(dataFile, n=skip+1)[skip+1])[[1]]) + 1
	if (dataCol > dataFileCols) { stop("Column ", dataCol, " (dataCol) not found on line ", 
		skip+1, "; maybe the column separator \"", sep, "\" is wrong?") }
	# drop variables for which column does not exist in file
	if (qualCol > dataFileCols) { qualCol <- NULL }
	extraNames <- extraNames[!(extraCols > dataFileCols)]
	extraCols <- extraCols[!(extraCols > dataFileCols)]
	# define which columns to import and which to ignore
	dataFileColClasses <- rep("NULL", dataFileCols)
	if (readTimesFromFile) { dataFileColClasses[timeCol] <- character(1) }
	dataFileColClasses[dataCol] <- numeric(1)
	dataFileColClasses[qualCol] <- NA # qualCol may be NULL
	dataFileColClasses[extraCols] <- NA # extraCols may be NULL
	# read file
	rawData <- read.table(dataFile, header=F, skip=skip, sep=sep, colClasses=dataFileColClasses, strip.white=T, ...)
	# work out which column of rawData has the data (from dataCol)
	dataIndex <- sum(!is.null(dataFileColClasses[1:dataCol]))
	qualIndex <- NULL
	timeIndex <- NULL
	# convert or construct the time sequence
	myTime <- NA
	if (readTimesFromFile) {
		timeIndex <- sum(!is.null(dataFileColClasses[1:timeCol]))
		myTime <- strptime(rawData[,timeIndex], format=timeFormat)
	} else {
		if ("list" \%in\% class(startTime)) {
			timeBits <- lapply(startTime, function(i) {
				if (is.numeric(i)) { firstLine[1,i] } else { i }
			})
			if (is.null(timeBits$hour)) { timeBits$hour <- 0 }
			if (is.null(timeBits$min)) { timeBits$min <- 0 }
			if (is.null(timeBits$sec)) { timeBits$sec <- 0 }
			startTime <- do.call(ISOdatetime, timeBits)
		}
		myTime <- seq.POSIXt(from=startTime, by=timeSeqBy, length.out=nrow(rawData))
	}
	# extract quality codes or set to default (factor("NA"))
	myQual <- NA
	if (!is.null(qualCol) && !is.na(qualCol)) {
		qualIndex <- sum(!is.null(dataFileColClasses[1:qualCol]))
		myQual <- factor(rawData[,qualIndex], exclude=NULL)
	} else {
		myQual <- rep(factor(NA, exclude=NULL), nrow(rawData))
	}
	# make sure first three columns are Time, Data, Qual (in that order)
	myBlob <- cbind(Time=myTime, Data=rawData[,dataIndex], Qual=myQual, 
		rawData[,-c(timeIndex, dataIndex, qualIndex)])
	names(myBlob) <- c("Time", dataName, "Qual", extraNames)
	return(myBlob)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
