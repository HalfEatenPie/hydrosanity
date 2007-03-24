\name{trunc.month}
\alias{trunc.month}
\alias{trunc.year}
\alias{trunc.decade}
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
trunc.month(thisPOSIXt)

trunc.year(thisPOSIXt)

trunc.decade(thisPOSIXt)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{thisPOSIXt}{ ~~Describe \code{thisPOSIXt} here~~ }
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
function(thisPOSIXt) {
	zz <- as.POSIXlt(thisPOSIXt)
	zz$mday <- 1
        zz$hour <- zz$min <- zz$sec <- 0
	zz$isdst <- -1
	return(zz)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
