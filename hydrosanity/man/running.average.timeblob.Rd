\name{running.average.timeblob}
\alias{running.average.timeblob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
running.average.timeblob(blob, by = "1 year", max.na.proportion = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{blob}{ ~~Describe \code{blob} here~~ }
  \item{by}{ ~~Describe \code{by} here~~ }
  \item{max.na.proportion}{ ~~Describe \code{max.na.proportion} here~~ }
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
function(blob, by="1 year", max.na.proportion=0.05) {
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
