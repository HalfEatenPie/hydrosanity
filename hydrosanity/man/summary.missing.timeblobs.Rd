\name{summary.missing.timeblobs}
\alias{summary.missing.timeblobs}
\title{Summarise Missing Data in a Set of Timeblobs}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary.missing.timeblobs(blob.list, timelim = NULL, timestep = NULL)
}
\arguments{
  \item{blob.list}{ ~~Describe \code{blob.list} here~~ }
  \item{timelim}{ ~~Describe \code{timelim} here~~ }
  \item{timestep}{ ~~Describe \code{timestep} here~~ }
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
function(blob.list, timelim=NULL, timestep=NULL) {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	if (is.null(timelim)) {
		timelim <- c(start.timeblobs(blob.list), end.timeblobs(blob.list))
	} else {
		timelim <- as.POSIXct(timelim)
		if (any(is.na(timelim))) { stop("'timelim' must be a pair of valid times (POSIXt)") }
		blob.list <- lapply(blob.list, window.timeblob, timelim[1], timelim[2])
	}
	# setup
	n <- length(blob.list)
	if (is.null(timestep)) {
		timestep <- common.timestep.timeblobs(blob.list)
	}
	times <- seq.POSIXt(timelim[1], timelim[2], by=timestep)
	myLengths <- sapply(blob.list, nrow)
	myNAs <- lapply(blob.list, function(x) { is.na(x[,2]) })
	myDataPoints <- myLengths - sapply(myNAs, sum)
	myDataFrac <- myDataPoints / length(times)
	overallDataFrac <- mean(myDataFrac)
	myCompleteN <- sum(myDataFrac >= 1)
	my95PctN <- sum(myDataFrac > 0.95)
	my75PctN <- sum(myDataFrac > 0.75)
	
	# find whether data exists for each timeblob for each time in myPeriod
	# (note: this rounds down if times do not match)
	dataMatrix <- sync.timeblobs(blob.list, timestep=timestep)
	dataExistsMatrix <- !is.na(as.matrix(dataMatrix[-1]))
	
	activeNs <- apply(dataExistsMatrix, 1, sum)
	allActiveSteps <- sum(activeNs == n)
	allActiveFrac <- allActiveSteps / length(times)
	activeNQ <- quantile(activeNs, probs=c(0.25, 0.5, 0.75))
	activeNQFrac <- activeNQ / n
	
	cat(sprintf('Overall, \%.0f\%\% of data is missing.\n', (1-overallDataFrac)*100))
	cat(sprintf('There are \%i time series, of which \%i \%s complete.\n', 
		n, myCompleteN, ifelse(myCompleteN==1,'is','are')))
	cat(sprintf('...\%i \%s > 95\%\% complete and \%i \%s > 75\%\% complete.\n', 
		my95PctN, ifelse(my95PctN==1,'is','are'), my75PctN, ifelse(my75PctN==1,'is','are')))
	cat('\n')
	cat(sprintf('\%i time steps (\%.1f\%\%) have data from all series.\n', allActiveSteps, allActiveFrac*100))
	cat(sprintf('The median number of active sites is \%i (\%.0f\%\%).\n', activeNQ[2], activeNQFrac[2]*100))
	cat(sprintf('...Half the time, the number of active sites is between \%i and \%i.\n', activeNQ[1], activeNQ[3]))
	#cat(sprintf('The number of active sites ranges from \%i to \%i.\n', activeNQ[1], activeNQ[3]))
	
	# gap length distribution
	
	# quality code summary
	
	# missing fraction for each series
	missingFrac <- (length(times) - apply(dataExistsMatrix, 2, sum)) / length(times)
	
	invisible(missingFrac)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
