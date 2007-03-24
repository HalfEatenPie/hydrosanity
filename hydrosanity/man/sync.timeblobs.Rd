\name{sync.timeblobs}
\alias{sync.timeblobs}
\alias{matchtimes.timeblob}
\alias{common.timestep.timeblobs}
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sync.timeblobs(blob.list, timestep = NULL, timelim = NULL, extractColumn = 2)

matchtimes.timeblob(blob, times)

common.timestep.timeblobs(blob.list, default = "DSTdays")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{blob.list}{ ~~Describe \code{blob.list} here~~ }
  \item{timestep}{ ~~Describe \code{timestep} here~~ }
  \item{timelim}{ ~~Describe \code{timelim} here~~ }
  \item{extractColumn}{ ~~Describe \code{extractColumn} here~~ }
  \item{times}{ ~~Describe \code{times} here~~ }
  \item{default}{ ~~Describe \code{default} here~~ }
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
function(blob.list, timestep=NULL, timelim=NULL, extractColumn=2) {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	if (is.null(timelim)) {
		timelim <- c(start.timeblobs(blob.list), end.timeblobs(blob.list))
	} else {
		timelim <- as.POSIXct(timelim)
		if (any(is.na(timelim))) { stop("'timelim' must be a pair of valid times (POSIXt)") }
		blob.list <- lapply(blob.list, window.timeblob, min(timelim), max(timelim))
	}
	# setup
	n <- length(blob.list)
	if (is.null(timestep)) {
		timestep <- common.timestep.timeblobs(blob.list)
	}
	times <- NULL
	times <- seq.POSIXt(min(timelim), max(timelim), by=timestep)
	syncBlob <- data.frame(
		Time=times,
		lapply(blob.list, function(x) {
			mySyncIndices <- matchtimes.timeblob(x, times)
			x[mySyncIndices,extractColumn]
		})
		)
	attr(syncBlob, "timestep") <- timestep
	return(syncBlob)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
