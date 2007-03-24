\name{grid.xaxis.POSIXt}
\alias{grid.xaxis.POSIXt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
grid.xaxis.POSIXt(at = NULL, label = NULL, name = "timeaxis", lim = as.numeric(convertX(unit(c(0, 1), "npc"), "native")), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{at}{ ~~Describe \code{at} here~~ }
  \item{label}{ ~~Describe \code{label} here~~ }
  \item{name}{ ~~Describe \code{name} here~~ }
  \item{lim}{ ~~Describe \code{lim} here~~ }
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
function(at=NULL, label=NULL, name="timeaxis", lim=as.numeric(convertX(unit(c(0,1), "npc"), "native")), ...) {
	timelim <- as.POSIXct.raw(lim)
	if (is.null(at)) {
		myBy <- "1 hour"
		myStart <- trunc(min(timelim), units="hours")
		myFormat <- "\%m-\%d \%H:\%M"
		if (diff(lim) > 12 * 60*60) {
			myBy <- "6 hours"
			myStart <- trunc(myStart, units="days")
		}
		if (diff(lim) > 1.5 * 24*60*60) {
			myBy <- "1 day"
			myFormat <- "\%Y-\%m-\%d"
		}
		if (diff(lim) > 7 * 24*60*60) {
			myBy <- "4 days"
		}
		if (diff(lim) > 30 * 24*60*60) {
			myBy <- "1 month"
			myStart <- trunc.month(myStart)
			myFormat <- "\%Y-\%m"
		}
		if (diff(lim) > 6 * 30*24*60*60) {
			myBy <- "3 months"
			myStart <- trunc.year(myStart)
			myFormat <- "\%b"
		}
		if (diff(lim) > 24 * 30*24*60*60) {
			myBy <- "1 year"
			myFormat <- "\%Y"
		}
		if (diff(lim) > 10 * 365.25*24*60*60) {
			myBy <- "5 years"
			myStart <- trunc.decade(myStart)
		}
		if (diff(lim) > 30 * 365.25*24*60*60) {
			myBy <- "10 years"
		}
		at <- seq.POSIXt(myStart, max(timelim), by=myBy)
		# only keep axis ticks within range
		inrange <- (min(timelim) <= at) & (at <= max(timelim))
		at <- at[inrange]
		if (is.null(label)) {
			label <- format(at, format=myFormat)
			atYears <- sapply(at, function(x) {
				# TODO: make this stand-alone
				x == trunc.year(as.POSIXct.raw(x))
			})
			label[atYears] <- format(at[atYears], format="\%Y")
		}
	}
	grid.xaxis(at=at, label=label, name=name, ...)
	return(list(at=at, label=label))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
