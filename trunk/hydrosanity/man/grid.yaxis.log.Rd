\name{grid.yaxis.log}
\alias{grid.yaxis.log}
\title{Log Scale Axis for Grid}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
grid.yaxis.log(at = NULL, label = NULL, name = "yaxis", lim = as.numeric(convertY(unit(c(0, 1), "npc"), "native")), ...)
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
function(at=NULL, label=NULL, name="yaxis", lim=as.numeric(convertY(unit(c(0,1), "npc"), "native")), ...) {
	if (is.null(at)) {
		# integer log powers
		at <- seq(ceiling(min(lim)), max(lim))
		if (diff(range(lim)) < 3.9) {
			# make it a linear sequence log-transformed
			mags <- seq(floor(min(lim)), max(lim))
			logAt <- c()
			for (mag in mags) {
				logAt <- c(logAt, log10(seq(10^mag, 10^(mag+1), 
					length.out=10)))
			}
			logAt <- logAt[(min(lim) <= logAt) & (logAt <= max(lim))]
			if ((diff(range(lim)) > 1.1) && !identical(label, F)) {
				# first draw ticks without labels
				# (can't just set these labels to ""; then main labels are not drawn)
				grid.yaxis(at=logAt, label=F,
					name=paste(name,".logticks",sep=''), ...)
			} else {
				at <- logAt
			}
		}
	}
	at <- unique(at)
	if (is.null(label)) {
		label <- sapply(10^at, format, digits=2, scientific=1)
		# large powers of 10 are better presented with an exponent
		bigIntPowers <- (at >= 5) & (at == round(at))
		label[bigIntPowers] <- paste("10^",at[bigIntPowers],sep='')
	}
	grid.yaxis(at=at, label=label, name=name, ...)
	return(list(at=at, label=label))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
