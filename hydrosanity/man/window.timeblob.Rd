\name{window.timeblob}
\alias{window.timeblob}
\alias{start.timeblob}
\alias{end.timeblob}
\alias{start.timeblobs}
\alias{end.timeblobs}
\alias{timelim.timeblobs}
\title{Time Windows of Time Series (Timeblobs)}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{window}{timeblob}(blob, start = NULL, end = NULL, inclusive = F, return.indices = F, extend = F)

\method{start}{timeblob}(blob)
\method{end}{timeblob}(blob)

start.timeblobs(blob.list)
end.timeblobs(blob.list)
timelim.timeblobs(blob.list)
}
\arguments{
  \item{blob}{ ~~Describe \code{blob} here~~ }
  \item{start}{ ~~Describe \code{start} here~~ }
  \item{end}{ ~~Describe \code{end} here~~ }
  \item{inclusive}{ ~~Describe \code{inclusive} here~~ }
  \item{return.indices}{ ~~Describe \code{return.indices} here~~ }
  \item{extend}{ ~~Describe \code{extend} here~~ }
  \item{blob.list}{ ~~Describe \code{blob.list} here~~ }
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
  returns length 0 if blob is empty
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
	
}
\keyword{ ts }

