\name{arealSubPolygons}
\alias{arealSubPolygons}
\title{ Compute sub-regions closest to each point }
\description{
  Compute the Voronoi mosaic of a set of points, and intersect it with a given
  polygon. This gives the sub-regions of the polygon for which each point is the
  closest.
}
\usage{
arealSubPolygons(x, y = NULL, IDs = row.names(x), boundary)
}
\arguments{
  \item{x}{ x coordinate of points, or a list containing \code{x} and \code{y}. }
  \item{y}{ y coordinate of points. }
  \item{IDs}{ identification strings for each point. }
  \item{boundary}{ polygon coordinates (coerced to "gpc.poly" class). }
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
\author{ ~~who you are~~ }
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
\keyword{ spatial }

