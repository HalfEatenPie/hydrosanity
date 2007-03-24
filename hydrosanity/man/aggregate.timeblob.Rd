\name{aggregate.timeblob}
\alias{aggregate.timeblob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
aggregate.timeblob(blob, by = "1 year", FUN = NULL, max.na.proportion = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{blob}{ ~~Describe \code{blob} here~~ }
  \item{by}{ ~~Describe \code{by} here~~ }
  \item{FUN}{ ~~Describe \code{FUN} here~~ }
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
function(blob, by="1 year", FUN=NULL, max.na.proportion=0.05) {
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
