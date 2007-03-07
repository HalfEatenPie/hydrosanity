\name{timeseriesplot}
\alias{timeseriesplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
timeseriesplot(timeblobList, timeStart = start.timeblob(timeblobList), timeEnd = end.timeblob(timeblobList), focusTime = NA, windowDays = 365 * 2, interactiveMode = F, showTimeline = interactiveMode, scrollSteps = 3, dataType = c("mean", "inst"), type = "l", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{timeblobList}{ ~~Describe \code{timeblobList} here~~ }
  \item{timeStart}{ ~~Describe \code{timeStart} here~~ }
  \item{timeEnd}{ ~~Describe \code{timeEnd} here~~ }
  \item{focusTime}{ ~~Describe \code{focusTime} here~~ }
  \item{windowDays}{ ~~Describe \code{windowDays} here~~ }
  \item{interactiveMode}{ ~~Describe \code{interactiveMode} here~~ }
  \item{showTimeline}{ ~~Describe \code{showTimeline} here~~ }
  \item{scrollSteps}{ ~~Describe \code{scrollSteps} here~~ }
  \item{dataType}{ ~~Describe \code{dataType} here~~ }
  \item{type}{ ~~Describe \code{type} here~~ }
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
function(timeblobList, timeStart=start.timeblob(timeblobList), timeEnd=end.timeblob(timeblobList), focusTime=NA, windowDays=365*2, interactiveMode=F, showTimeline=interactiveMode, scrollSteps=3, dataType=c("mean","inst"), type="l", ...) {
	if (is.data.frame(timeblobList)) { timeblobList <- list(timeblobList) }
	n_ts <- length(timeblobList)
	plotRanges <- lapply(timeblobList, range.timeblob, na.rm=T)
	timeStart <- as.POSIXct(timeStart)
	timeEnd <- as.POSIXct(timeEnd)
	timeRange <- as.numeric(timeEnd) - as.numeric(timeStart)
	halfWin <- round(timeRange/2)
	currFocus <- as.numeric(timeStart) + halfWin
	halfWin <- halfWin * 1.04 # pretty padding
	if (!is.na(focusTime)) { halfWin <- (windowDays * 24 * 60 * 60) / 2 }
	if (!is.na(focusTime)) { currFocus <- as.numeric(as.POSIXct(focusTime)) }
	prevFocus <- currFocus
	old_par <- par(mar=c(3,4,0,1),oma=c(0,0,1,0),xpd=F)
	on.exit(par(old_par))
	old_opt <- options(locatorBell=F)
	on.exit(options(old_opt),add=T)
	doplot <- T
	while (doplot) {
		plotFocus <- currFocus
		if (interactiveMode && (currFocus != prevFocus)) {
			scrollUnit <- halfWin / scrollSteps
			displaceSteps <- (currFocus - prevFocus) / scrollUnit
			plotFocus <- prevFocus + (currFocus - prevFocus) / abs(round(displaceSteps))
			if (!is.finite(plotFocus)) { plotFocus <- currFocus }
			prevFocus <- plotFocus
		}
		if (showTimeline) {
			# place the timeline below the other plots, but draw it before them
			layout(matrix(c(seq(2,n_ts+1),1)),heights=c(rep(1,n_ts),0.2))
			tmp_par <- par(mar=c(1,4,0,4),xpd=NA)
			plot(0, type="n", axes=F, xlim=c(timeStart,timeEnd), xaxs="i", xlab=" ", ylim=c(0,1), ylab=" ")
			rect(timeStart, 0, timeEnd, 1, col=grey(0.9))
			rect(max(as.numeric(timeStart),plotFocus-halfWin), 0, min(as.numeric(timeEnd),plotFocus+halfWin), 1, col="grey")
			text(timeStart, 0.5, get.year(timeStart), pos=4)
			text(timeEnd, 0.5, get.year(timeEnd), pos=2)
			if (interactiveMode) {
				text(as.numeric(timeStart) + timeRange/2, 0.5, "Click on the plot (or here) to scroll through time.")
				text(timeStart, 0.5, "zoom\nout", pos=2, offset=1, cex=1, font=2, family="mono", col=grey(0.25))
				text(timeEnd, 0.5, "zoom\n in", pos=4, offset=1, cex=1, font=2, family="mono", col=grey(0.25))
			}
			par(tmp_par)
		} else {
			layout(matrix(seq(1,n_ts)))
		}
		for (k in 1:n_ts) {
			plotStart <- as.POSIXct.raw(plotFocus-halfWin)
			plotEnd <- as.POSIXct.raw(plotFocus+halfWin)
			plotData <- window.timeblob(timeblobList[[k]], plotStart, plotEnd, inclusive=T)
			iDates <- seq(1,nrow(plotData))
			iVals <- iDates
			if (dataType[1] == "mean") {
				# draw as steps (note: plot type="s" fails to draw horiz line preceding NA points)
				iDates <- c(rep(iDates,each=2)[-1],NA)
				iVals <- rep(iVals,each=2)
				type <- "l"
			}
			plot.default(plotData$Time[iDates], plotData[iVals,2], xlim=c(plotFocus-halfWin,plotFocus+halfWin), xaxs="i", xaxt="n", xlab=" ", ylim=plotRanges[[k]], ylab=names(timeblobList)[k], type=type, ...)
			axis.POSIXct(1, as.POSIXct.raw(plotFocus))
		}
		if (interactiveMode && (currFocus == prevFocus)) {
			# wait for user to click
			newFocus <- locator(n=1)
			# if the user cancelled then exit
			if (is.null(newFocus)) {
				return(c(as.POSIXct.raw(currFocus-halfWin), as.POSIXct.raw(currFocus+halfWin)))
			}
			# if the click occurred on the timeline area
			# timeline area starts 3 lines from bottom of plot area (i.e. mar=3)
			timelineAreaY <- par("usr")[3] - mar.to.xy(yMar=3)
			if (showTimeline && (newFocus$y < timelineAreaY)) {
				# convert main plot coordinates into timeline coordinates (scaled to [0,1])
				# adjust for different margins: right side is an extra 3 lines in (mar=4)
				relX <- (newFocus$x - (currFocus-halfWin)) / (halfWin*2 - mar.to.xy(xMar=3))
				if (relX < 0) {
					# user clicked on (-) to zoom out (expand time window)
					halfWin <- halfWin * 2
					if (halfWin > timeRange/2) {
						halfWin <- round(timeRange/2)
						currFocus <- as.numeric(timeStart) + halfWin
						prevFocus <- currFocus
					}
				} else if (relX > 1) {
					# user clicked on (+) to zoom in (contract time window)
					tmp_par <- par(xpd=NA)
					msgAreaHeightLines <- par("fig")[3] * par("din")[2] / par("csi")
					msgAreaHeight <- mar.to.xy(yMar=msgAreaHeightLines)
					leftEdgeX <- par("usr")[1] - mar.to.xy(xMar=4)
					rightEdgeX <- par("usr")[2] + mar.to.xy(xMar=1)
					rect(leftEdgeX, timelineAreaY - msgAreaHeight, rightEdgeX, timelineAreaY, col="yellow")
					text(currFocus, timelineAreaY - msgAreaHeight/2, "Click at the start of the window (to zoom in to)...", cex=1.5)
					newWindowStart <- locator(n=1)
					if (is.null(newWindowStart)) { next }
					newWindowStart <- round(newWindowStart$x)
					abline(v=newWindowStart, lty=2, lwd=2, col="red")
					rect(leftEdgeX, timelineAreaY - msgAreaHeight, rightEdgeX, timelineAreaY, col="yellow")
					text(currFocus, timelineAreaY - msgAreaHeight/2, "OK, now click at the end of the window.", cex=1.5)
					newWindowEnd <- locator(n=1)
					if (is.null(newWindowEnd)) { next }
					newWindowEnd <- round(newWindowEnd$x)
					abline(v=newWindowEnd, lty=2, lwd=2, col="red")
					if (newWindowEnd < newWindowStart) {
						tmp <- newWindowEnd
						newWindowEnd <- newWindowStart
						newWindowStart <- tmp
					}
					halfWin <- (newWindowEnd - newWindowStart) / 2
					currFocus <- newWindowStart + halfWin
					prevFocus <- currFocus
					par(tmp_par)
				} else {
					# user clicked timeline to scroll
					if (halfWin == round(timeRange/2)) { next }
					# convert into timeline coordinates
					currFocus <- as.numeric(timeStart) + round(relX * timeRange)
					prevFocus <- currFocus
				}
			} else {
				# click occurred on plot area
				if (halfWin == round(timeRange/2)) { next }				
				currFocus <- round(newFocus$x)
			}
			if (is.na(scrollSteps) || scrollSteps == 0) { prevFocus <- currFocus }
		}
		if (!interactiveMode) { doplot <- F }
	}
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
