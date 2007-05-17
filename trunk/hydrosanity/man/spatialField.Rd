\name{spatialField}
\alias{spatialField}
\title{ ~~function to do ... ~~ }
\description{
  Spatial interpolation of a set of time series.
}
\usage{
spatialField(blob.list, timelim = NULL, type = c("overall", "annual", "quarters", "months"), start.month = 1, linear = T, extrap = F, xo.length = 40, yo.length = xo.length, countSurface = F)
}
\arguments{
  \item{blob.list}{ ~~Describe \code{blob.list} here~~ }
  \item{timelim}{ ~~Describe \code{timelim} here~~ }
  \item{type}{ ~~Describe \code{type} here~~ }
  \item{start.month}{ ~~Describe \code{start.month} here~~ }
  \item{linear}{ ~~Describe \code{linear} here~~ }
  \item{extrap}{ ~~Describe \code{extrap} here~~ }
  \item{xo.length}{ ~~Describe \code{xo.length} here~~ }
  \item{yo.length}{ ~~Describe \code{yo.length} here~~ }
  \item{countSurface}{ ~~Describe \code{countSurface} here~~ }
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
function(blob.list, timelim=NULL, type=c("overall","annual","quarters","months"), start.month=1, linear=T, extrap=F, xo.length=40, yo.length=xo.length, countSurface=F) {
	# check types
	if (!require(akima, quietly=TRUE)) { stop("Require package 'akima'") }
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	if (length(blob.list) < 4) { stop("Need at least 4 items") }
	if (is.null(timelim)) {
		timelim <- c(start.timeblobs(blob.list), end.timeblobs(blob.list))
	} else {
		timelim <- as.POSIXct(timelim)
		if (any(is.na(timelim))) { stop("'timelim' must be a pair of valid times (POSIXt)") }
		blob.list <- lapply(blob.list, window, min(timelim), max(timelim))
	}
	type <- match.arg(type)
	if (!is.numeric(start.month)) { stop("'start.month' must be a number 1-12") }
	endYear.month <- if (start.month == 1) { 12 } else { start.month - 1 }
	monthQuarters <- rep(1:4, each=3)
	shiftWrap <- function(x, shift) { x[(seq_along(x)-shift-1)\%\%length(x)+1] }
	monthQuarters <- shiftWrap(monthQuarters, start.month-1)
	waterYearMonthNames <- shiftWrap(monthNames, 12-(start.month-1))
	quarterNames <- sapply(c(3,6,9,12), function(i) {
		paste(waterYearMonthNames[seq(i-2,i)], collapse=" ")
	})
	# get locations
	loc <- lapply(blob.list, attr, "location.xy")
	ok <- (sapply(loc, length) == 2)
	if (any(!ok)) {
		stop(paste("These items do not have a valid 'location.xy' attribute:",
			paste(names(blob.list)[!ok], collapse=", ")))
	}
	loc <- sapply(blob.list, attr, "location.xy")
	loc <- data.frame(t(loc))
	names(loc) <- c("X", "Y")
	# construct marginal dimensions of grid
	xo <- seq(min(loc$X), max(loc$X), length=xo.length)
	yo <- seq(min(loc$Y), max(loc$Y), length=yo.length)
	# construct synchronised monthly-scale time series
	tmp.data <- lapply(blob.list, aggregate, by="months", fun.qual="omit")
	tmp.data <- sync.timeblobs(tmp.data)
	# initialise spatial grids
	values <- list()
	counts <- list()
	values$accum <- matrix(as.numeric(0), nrow=yo.length, ncol=xo.length)
	counts$accum <- matrix(as.integer(0), nrow=yo.length, ncol=xo.length)
	if (type == "annual") {
		thisYear <- NULL
	}
	if (type == "quarters") {
		for (blah in quarterNames) {
			values[[blah]] <- values$accum
			counts[[blah]] <- counts$accum
		}
	}
	if (type == "months") {
		for (blah in monthNames) {
			values[[blah]] <- values$accum
			counts[[blah]] <- counts$accum
		}
	}
	# generate surfaces for each month, etc
	for (i in seq_along(tmp.data$Time)) {
		myTime <- tmp.data$Time[i]
		myMonth <- as.POSIXlt(myTime)$mon+1
		myYear <- as.POSIXlt(myTime)$year+1900
		waterYear <- if (myMonth < start.month) { myYear - 1 } else { myYear }
		if (type == "annual") {
			if (is.null(thisYear)) { thisYear <- waterYear }
			if (waterYear > thisYear) {
				# a new year
				values$accum[] <- 0
				counts$accum[] <- 0
				thisYear <- waterYear
			}
		}
		# find set of stations with valid data
		ok <- !is.na(tmp.data[i,-1])
		# need at least 4 points for spatial interpolation
		if (sum(ok) < 4) { next }
		# get this month's mean value for each station
		dataPoints <- as.numeric(tmp.data[i, which(ok)+1])
		# compute the spatial field (interpolation)
		monthSurf <- interp(x=loc$X[ok], y=loc$Y[ok], z=dataPoints, 
			xo=xo, yo=yo, linear=linear, extrap=extrap)$z
		gridOK <- !is.na(monthSurf)
		# store accumulated surface for this month
		# increment 'count' for each cell that has data for this month
		accumName <- "accum"
		if (type == "months") {
			accumName <- monthNames[myMonth]
		}
		if (type == "quarters") {
			accumName <- quarterNames[monthQuarters[myMonth]]
		}
		counts[[accumName]] <- counts[[accumName]] + gridOK
		monthSurf[!gridOK] <- 0
		values[[accumName]] <- values[[accumName]] + monthSurf
		# store accumulated surface for this water year
		if ((type == "annual") && (myMonth == endYear.month)) {
			values$accum[ (counts$accum < 12) ] <- NA
			yearLabel <- as.character(waterYear)
			if (any(!is.na(values$accum))) {
				values[[yearLabel]] <- values$accum / 12
				counts[[yearLabel]] <- counts$accum
			}
		}
	}
	# remove temporary (accumulation) grid
	if (type != "overall") {
		values$accum <- NULL
		counts$accum <- NULL
	}
	if (type == "annual") {
		if (length(values)==0) {
			stop("No years with valid data (no locations with all 12 months non-missing)")
		}
	} else {
		# divide by counts to get mean at each cell
		for (i in seq_along(values)) {
			values[[i]][ (counts[[i]]==0) ] <- NA
			values[[i]] <- values[[i]] / counts[[i]]
		}
	}
	# construct vectors x, y and z (as surface value)
	# as well as other factors specifying any different grids
	tmp.grid <- switch(type,
		overall=expand.grid(x=xo, y=yo),
		annual=expand.grid(x=xo, y=yo, year=names(values)),
		quarters=expand.grid(x=xo, y=yo, quarter=names(values)),
		months=expand.grid(x=xo, y=yo, month=names(values))
	)
	if (countSurface) {
		tmp.grid$z <- c(unlist(counts, use.names=F))
		return(tmp.grid)
	}
	tmp.grid$z <- c(unlist(values, use.names=F))
	return(tmp.grid)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
