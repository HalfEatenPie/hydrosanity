\name{panel.identify.qqmath}
\alias{panel.identify.qqmath}
\title{Identify data points in a qqmath plot}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
panel.identify.qqmath(panel.args = trellis.panelArgs(), ...)
}
\arguments{
  \item{panel.args}{ ~~Describe \code{panel.args} here~~ }
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
function(panel.args = trellis.panelArgs(), ...) {
	x <- panel.args$x
	y <- x
	groups <- panel.args$groups
	callArgs <- list(...)
	subscripts <- callArgs$subscripts
	if (is.null(subscripts)) { subscripts <- seq_along(x) }
	labels <- callArgs$labels
	if (is.null(labels)) { labels <- subscripts }
	# compute x locations (currently only for is.null(f))
	getxx <- function(x) { panel.args$distribution(ppoints(length(x))) }
	if (is.null(groups)) {
		x <- getxx(x)
		ii <- order(y)
		y <- y[ii]
		labels <- labels[ii]
		subscripts <- subscripts[ii]
	} else {
		for (g in levels(factor(groups))) {
			id <- (groups == g)
			x[id] <- getxx(x[id])
			ii <- order(y[id])
			y[id] <- y[id][ii]
			labels[id] <- labels[id][ii]
			subscripts[id] <- subscripts[id][ii]
		}
	}
	callArgs$x <- x
	callArgs$y <- y
	callArgs$labels <- labels
	callArgs$subscripts <- subscripts
	callArgs$panel.args <- NULL
	do.call(panel.identify, callArgs)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
