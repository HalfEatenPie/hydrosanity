\name{gaps}
\alias{gaps}

\title{Find Runs of Consecutive Missing Values}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gaps(x, internal.only = T)

rises(x)

lastTime(x)
}
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{internal.only}{ ~~Describe \code{internal.only} here~~ }
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
function(x) {
	seriesNA <- is.na(x)
	# diffNA is 1 at start of gap, -1 at end of gap, 0 otherwise
	diffNA <- c(0, diff(seriesNA))
	preDataGap <- match(F, seriesNA) - 1
	postDataGap <- match(F, rev(seriesNA)) - 1
	# so we don't detect a gap end at start of data:
	diffNA[preDataGap+1] <- 0
	# find indices where NA is followed by data
	gapEnd <- which(c(0,diffNA)==-1) - 1
	nGaps <- length(gapEnd)
	naCumSum <- cumsum(seriesNA)
	gapLength <- naCumSum[gapEnd] - 
		naCumSum[c(1,gapEnd[-nGaps])]
	return(list(gapLength=gapLength, gapEnd=gapEnd, 
		preDataGap=preDataGap, postDataGap=postDataGap))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
