\name{grid.timeseries.plot}
\alias{grid.timeseries.plot}
\alias{grid.timeseries.plot.superpose}
\alias{grid.timeseries.steps}
\title{Plot Time Series}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
grid.timeseries.plot(blob.list, xscale = NULL, yscale = NULL, sameScales = T, logScale = F, qualTimeline = F, colMap = NULL, barThickness = unit(0.5,"lines"), auto.key = T, maxLabelChars = 20, pad = unit(1,"lines"), between = unit(0,"lines"), superPos = 1, newScale = T, main = NULL, sub = T, newpage = (superPos==1), nSuperpose = 1, gp = gpar(col=trellis.par.get("superpose.line")$col[superPos], lty = trellis.par.get("superpose.line")$lty[superPos]))

grid.timeseries.plot.superpose(superpose.blob.list, allSameScales = F, xscale = NULL, yscale = NULL, logScale = F, sub = T, ...)

grid.timeseries.steps(blob, logScale = F, name = "timeseries", gp = NULL, vp = NULL)
}
\arguments{
  \item{blob.list}{ ~~Describe \code{blob.list} here~~ }
  \item{xscale}{ ~~Describe \code{xscale} here~~ }
  \item{yscale}{ ~~Describe \code{yscale} here~~ }
  \item{sameScales}{ ~~Describe \code{sameScales} here~~ }
  \item{logScale}{ ~~Describe \code{logScale} here~~ }
  \item{qualTimeline}{ ~~Describe \code{qualTimeline} here~~ }
  \item{colMap}{ ~~Describe \code{colMap} here~~ }
  \item{barThickness}{ ~~Describe \code{thickness} here~~ }
  \item{auto.key}{ ~~Describe \code{auto.key} here~~ }
  \item{maxLabelChars}{ ~~Describe \code{maxLabelChars} here~~ }
  \item{pad}{ ~~Describe \code{pad} here~~ }
  \item{between}{ ~~Describe \code{between} here~~ }
  \item{superPos}{ ~~Describe \code{superPos} here~~ }
  \item{newScale}{ ~~Describe \code{newScale} here~~ }
  \item{main}{ ~~Describe \code{main} here~~ }
  \item{sub}{ ~~Describe \code{sub} here~~ }
  \item{newpage}{ ~~Describe \code{newpage} here~~ }
  \item{nSuperpose}{ ~~Describe \code{nSuperpose} here~~ }
  \item{gp}{ ~~Describe \code{gp} here~~ }
  \item{superpose.blob.list}{ ~~Describe \code{superpose.blob.list} here~~ }
  \item{allSameScales}{ ~~Describe \code{allSameScales} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
  \item{blob}{ ~~Describe \code{blob} here~~ }
  \item{name}{ ~~Describe \code{blob} here~~ }
  \item{vp}{ ~~Describe \code{blob} here~~ }
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
function(blob.list, xscale=NULL, yscale=NULL, sameScales=T, logScale=F, zeroLevel=NULL, maxLabelChars=20, pad=unit(5,"mm"), superPos=1, newScale=T, newpage=(superPos==1), nSuperpose=1, gp=gpar(col=colSet[superPos]), colSet=c("#0080ff", "#ff00ff", "darkgreen", "#ff0000", "orange", "#00ff00")) {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	if (is.null(xscale)) {
		if (superPos == 1) {
			xscale <- c(start.timeblobs(blob.list), end.timeblobs(blob.list))
		} else {
			depth <- downViewport("time.vp")
			xscale <- as.POSIXct.raw(convertX(unit(c(0,1), "npc"), "native"))
			upViewport(depth)
		}
	}
	xscale <- as.POSIXct(xscale)
	if (any(is.na(xscale))) { stop("'xscale' must be a pair of valid times (POSIXt)") }
	if (!is.null(yscale) && !is.numeric(yscale)) { stop("'yscale' must be numeric") }
	# setup
	n <- length(blob.list)
	blob.list <- lapply(blob.list, window.timeblob, xscale[1], xscale[2], inclusive=T)
	ylabs <- sapply(names(blob.list), toString, width=maxLabelChars)
	if (newpage) { grid.newpage() }
	# layout for plot
	yLabSpace <- unit(1.5, "lines")
	yAxisWidth <- unit(3, "lines")
	yAxesSpace <- yAxisWidth
	if (newScale) { yAxesSpace <- yAxisWidth * nSuperpose }
	if (superPos == 1) {
		pushViewport(viewport(name="timeseries.plot.layout",
			layout=grid.layout(3, 3,
			widths=unit.c(yLabSpace+yAxesSpace, unit(1,"null"), pad),
			heights=unit.c(pad, unit(1,"null"), unit(3, "lines")))))
		# overall plot viewport, and layout for timeseries plots
		pushViewport(viewport(name="time.vp", 
			layout.pos.col=2, layout.pos.row=2, xscale=xscale,
			layout=grid.layout(n*2-1, 1,
			heights=unit.c(rep(unit.c(unit(1,"null"), pad), n), unit(1,"null")) )))
		# draw time axis with labels at bottom of plot
		grid.xaxis.POSIXt(name="timeseries.xaxis")
	}
	# calculate common yscale
	if (is.null(yscale) && sameScales) {
		allRanges <- sapply(blob.list, range.timeblob, na.rm=T)
		yscale <- range(allRanges[is.finite(allRanges)])
		if (logScale && (yscale[1] <= 0)) {
			# limit by minimum non-zero value (for log scale)
			allMins <- sapply(blob.list, function(x){
				min(x[,2][x[,2]>0], na.rm=T)})
			yscale[1] <- min(allMins[is.finite(allMins)])
		}
	}
	# plot each timeblob in the list
	for (k in 1:n) {
		# allow skipping for superposed series
		if (is.null(blob.list[[k]])) { next }
		# set up vertical scale for timeseries number k
		myYScale <- yscale
		if (is.null(myYScale) && nrow(blob.list[[k]])==0) {
			# no data for this series
			myYScale <- c(0, 1)
		}
		if (is.null(myYScale)) {
			myYScale <- range.timeblob(blob.list[[k]], na.rm=T)
			# need to ensure yscale > 0 for log scale
			if (logScale) {
				if (is.null(zeroLevel)) {
					if (myYScale[1] <= 0) {
						myYScale[1] <- nonzeromin(blob.list[[k]][,2], na.rm=T)
					}
				} else {
					myYScale[1] <- max(myYScale[1], zeroLevel)
				}
			}
		}
		if (logScale) {
			myYScale <- log10(myYScale)
		}
		# extend yscale for clarity
		myYScale[2] <- myYScale[2] + 0.05*diff(myYScale)
		if (!logScale) { myYScale[1] <- myYScale[1] - 0.05*diff(myYScale) }
		# do plot
		if (superPos == 1) {
			# create viewport for timeseries number k
			pushViewport(viewport(
				name=paste("timeseries",k,".vp",sep=''),
				layout.pos.row=k*2-1, 
				xscale=xscale, yscale=myYScale, clip="on"))
		} else {
			# navigate down to where timeseries number k was plotted
			downViewport(paste("timeseries",k,".vp",sep=''))
			if (newScale==T) {
				# push a new viewport to change scales
				pushViewport(viewport(
					name=paste("timeseries",k,"/",superPos,".vp",sep=''),
					xscale=xscale, yscale=myYScale, clip="on"))
			}
		}
		grid.timeseries.steps(blob.list[[k]], logScale=logScale,
			gp=gp, name=paste("timeseries",k,sep=''))
		# draw frame and axes
		if (superPos == 1) {
			pushViewport(viewport(xscale=xscale, yscale=myYScale, clip="off"))
			grid.rect()
			grid.xaxis.POSIXt(label=F)
			if (logScale) {
				grid.yaxis.log()
				grid.yaxis.log(main=F, label=F)
			} else {
				grid.yaxis()
				grid.yaxis(main=F, label=F)
			}
			# draw label number k
			grid.text(ylabs[k], x=-1*yAxesSpace-unit(1,"lines"), rot=90,
				name=paste("label",k,sep=''))
		}
		if ((superPos != 1) && newScale) {
			pushViewport(viewport(x=-1*yAxisWidth*(superPos-1), 
				just="left", yscale=myYScale, clip="off", gp=gp))
			if (logScale) { grid.yaxis.log() }
			else { grid.yaxis() }
		}
		# come back up
		if (superPos == 1) { upViewport() } # axes
		if ((superPos != 1) && newScale) { upViewport(2) } # axes and scale
		upViewport() # from timeseriesk.vp
	}
	# come back up
	upViewport(2)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
