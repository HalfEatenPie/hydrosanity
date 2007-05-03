\name{grid.timeline.plot}
\alias{grid.timeline.plot}
\alias{grid.timeline.bar}
\alias{timelineColMapDefault}
\title{Plot Timeline of Data and its Quality}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
grid.timeline.plot(blob.list, xscale = NULL, colMap = NULL, barThickness = unit(1.2,"lines"), auto.key = T, maxLabelChars = 20, pad = unit(1,"lines"), grill = T, main = NULL, sub = T, newpage = T)

grid.timeline.bar <- function(blob, colMap = NULL, name = "timeline.bar", vp = NULL)

timelineColMapDefault(colMap = list(good="black", suspect=trellis.par.get("superpose.polygon")$col[1], poor=trellis.par.get("superpose.polygon")$col[2], disaccumulated=trellis.par.get("superpose.polygon")$col[3], imputed=trellis.par.get("superpose.polygon")$col[4]))
}
\arguments{
  \item{blob.list}{ ~~Describe \code{blob.list} here~~ }
  \item{xscale}{ ~~Describe \code{xscale} here~~ }
  \item{colMap}{ ~~Describe \code{colMap} here~~ }
  \item{barThickness}{ ~~Describe \code{thickness} here~~ }
  \item{auto.key}{ ~~Describe \code{auto.key} here~~ }
  \item{maxLabelChars}{ ~~Describe \code{maxLabelChars} here~~ }
  \item{pad}{ ~~Describe \code{pad} here~~ }
  \item{grill}{ ~~Describe \code{grill} here~~ }
  \item{main}{ ~~Describe \code{main} here~~ }
  \item{sub}{ ~~Describe \code{sub} here~~ }
  \item{newpage}{ ~~Describe \code{newpage} here~~ }
  \item{blob}{ ~~Describe \code{blob} here~~ }
  \item{name}{ ~~Describe \code{name} here~~ }
  \item{vp}{ ~~Describe \code{vp} here~~ }
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
function(blob.list, xscale=NULL, colMap=list(good="black",maybe="orange",poor="red"), thickness=unit(1.5,"lines"), maxLabelChars=20, pad=unit(4,"mm"), grill=T, newpage=T) {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	if (is.null(xscale)) {
		xscale <- c(start.timeblobs(blob.list), end.timeblobs(blob.list))
	}
	xscale <- as.POSIXct(xscale)
	if (any(is.na(xscale))) { stop("'xscale' must be a pair of valid times (POSIXt)") }
	# setup
	n <- length(blob.list)
	ylabs <- sapply(names(blob.list), toString, width=maxLabelChars)
	maxlab <- ylabs[ which.max(nchar(ylabs, "width")) ]
	if (newpage) { grid.newpage() }
	# layout for plot
	pushViewport(viewport(name="timeline.plot.layout",
		layout=grid.layout(3, 3,
			widths=unit.c(stringWidth(maxlab)+pad, unit(1,"null"), pad),
			heights=unit.c(pad, unit(1,"null"), unit(3, "lines")))))
	# overall plot viewport, and layout for timeline bars
	pushViewport(viewport(name="time.vp", 
		layout.pos.col=2, layout.pos.row=2, xscale=xscale,
		layout=grid.layout(n*2+1, 1,
			heights=unit.c(unit(1,"null"), 
				rep(unit.c(thickness, unit(1,"null")), n)))))
	# draw axis and grill
	grid.lines(y=unit(0,"npc"))
	tickX <- grid.xaxis.POSIXt(name="timeline.xaxis")$at
	if (grill) {
		grid.segments(tickX, unit(0,"npc"), tickX, unit(1,"npc")-pad,
			default.units="native", gp=gpar(col="grey"))
	}
	for (k in 1:n) {
		# draw timeline bar number k
		pushViewport(viewport(name=paste("timeline.bar",k,".vp",sep=''),
			layout.pos.row=k*2, xscale=xscale))
		grid.timeline.bar(blob.list[[k]], colMap=colMap)
		# draw label number k
		grid.text(ylabs[k], x=0, just="right",
			name=paste("label",k,sep=''))
		upViewport()
	}
	# come back up
	upViewport(2)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
