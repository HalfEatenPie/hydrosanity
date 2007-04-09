\name{window.timeblob}
\alias{window.timeblob}
\alias{start.timeblob}
\alias{end.timeblob}
\alias{start.timeblobs}
\alias{end.timeblobs}
\title{Time Windows of Time Series (Timeblobs)}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{window}{timeblob}(blob, start = NULL, end = NULL, inclusive = F, return.indices = F, extend = F)

\method{start}{timeblob}(blob)
\method{end}{timeblob}(blob)

start.timeblobs(blob.list)
end.timeblobs(blob.list)
}
\arguments{
  \item{blob}{ ~~Describe \code{blob} here~~ }
  \item{start}{ ~~Describe \code{start} here~~ }
  \item{end}{ ~~Describe \code{end} here~~ }
  \item{inclusive}{ ~~Describe \code{inclusive} here~~ }
  \item{return.indices}{ ~~Describe \code{return.indices} here~~ }
  \item{extend}{ ~~Describe \code{extend} here~~ }
  \item{blob.list}{ ~~Describe \code{blob.list} here~~ }
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
  returns length 0 if blob is empty
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
function(blob, start, end, inclusive=F, extend=F) {
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
