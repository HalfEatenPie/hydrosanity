\name{window.timeblob}
\alias{window.timeblob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
window.timeblob(myBlob, start, end, inclusive = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{myBlob}{ ~~Describe \code{myBlob} here~~ }
  \item{start}{ ~~Describe \code{start} here~~ }
  \item{end}{ ~~Describe \code{end} here~~ }
  \item{inclusive}{ ~~Describe \code{inclusive} here~~ }
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
function(myBlob, start, end, inclusive=F) {
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
