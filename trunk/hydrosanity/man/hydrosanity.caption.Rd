\name{hydrosanity.caption}
\alias{hydrosanity.caption}
\title{Metadata caption for plots}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hydrosanity.caption(timelim, by, n, series = NA, x = unit(1, "npc") - unit(1, "mm"), y = unit(1, "mm"), just = c("right", "bottom"), gp = gpar(cex = 0.6, col = grey(0.3)))
}
\arguments{
  \item{timelim}{ ~~Describe \code{timelim} here~~ }
  \item{by}{ ~~Describe \code{by} here~~ }
  \item{n}{ ~~Describe \code{n} here~~ }
  \item{series}{ ~~Describe \code{series} here~~ }
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{y}{ ~~Describe \code{y} here~~ }
  \item{just}{ ~~Describe \code{just} here~~ }
  \item{gp}{ ~~Describe \code{gp} here~~ }
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
function(timelim, by, n, series=NA, x=unit(1,"npc")-unit(1,"mm"), y=unit(1,"mm"), just=c("right","bottom"), gp=gpar(cex=0.6, col=grey(0.3))) {
	timelim <- as.POSIXct(timelim)
	if (any(is.na(timelim))) { stop("'timelim' must be a pair of valid times (POSIXt)") }
	if (!identical(by, "irregular") && !is.na(series)) {
		allN <- (length(seq(timelim[1], timelim[2], by=by))-1) * series
		pctN <- round(100*(n/allN), digits=0)
		if (pctN < 100) {
			n <- sprintf('\%s=\%s\%\%', n, pctN)
		}
	}
	vRSimple <- paste(R.version$major, R.version$minor, sep='.')
	textGrob(sprintf(
		"Data: \%s to \%s by \%s (N=\%s). Hydrosanity \%s, R \%s",
		timelim[1], timelim[2], by, n, VERSION, vRSimple),
		x=x, y=y, just=just, gp=gp, name="hydrosanity.caption")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
