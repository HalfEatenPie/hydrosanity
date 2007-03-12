\name{aggregate.timeblob}
\alias{aggregate.timeblob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
aggregate.timeblob(myBlob, by = "1 year", from = floor.year(start.timeblob(myBlob)), to = end.timeblob(myBlob), max.na.proportion = 0.05, aggrFun = mean)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{myBlob}{ ~~Describe \code{myBlob} here~~ }
  \item{by}{ ~~Describe \code{by} here~~ }
  \item{from}{ ~~Describe \code{from} here~~ }
  \item{to}{ ~~Describe \code{to} here~~ }
  \item{max.na.proportion}{ ~~Describe \code{max.na.proportion} here~~ }
  \item{aggrFun}{ ~~Describe \code{aggrFun} here~~ }
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
function(myBlob, by="1 year", from=floor.year(start.timeblob(myBlob)), to=end.timeblob(myBlob), max.na.proportion=0.05, aggrFun=mean) {
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
