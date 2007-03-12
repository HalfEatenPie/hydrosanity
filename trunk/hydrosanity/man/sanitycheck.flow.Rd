\name{sanitycheck.flow}
\alias{sanitycheck.flow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sanitycheck.flow(timeblobList)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{timeblobList}{ ~~Describe \code{timeblobList} here~~ }
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
function(timeblobList) {
	if (is.data.frame(timeblobList)) { timeblobList <- list(timeblobList) }
	for (k in seq(along=timeblobList)) {
		cat("Sanity checking streamflow series", k, "(", names(timeblobList)[k], ") ...\n")
		rawdata <- timeblobList[[k]][,2]
		pctiles <- quantile(rawdata, na.rm=T)
		if (pctiles[["0\%"]] < 0) {
			cat("  INSANITY: minimum less than zero:", pctiles[["0\%"]], "\n")
		} else {
			if (pctiles[["100\%"]] < 1) {
				cat("  INSANITY: maximum less than 1:", pctiles[["100\%"]], "\n")
			}
			if (mean(rawdata, na.rm=T) < 2 * pctiles[["50\%"]]) {
				cat("  INSANITY: mean less than twice median (i.e. low skewness)", "\n")
			}
		}
	}
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
