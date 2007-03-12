\name{timelineplot}
\alias{timelineplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
timelineplot(timeblobList, timeStart = start.timeblob(timeblobList), timeEnd = end.timeblob(timeblobList), plotQualCodes = F, colMap = list(good = "black", maybe = "orange", poor = "red"), verticalGrid = T, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{timeblobList}{ ~~Describe \code{timeblobList} here~~ }
  \item{timeStart}{ ~~Describe \code{timeStart} here~~ }
  \item{timeEnd}{ ~~Describe \code{timeEnd} here~~ }
  \item{plotQualCodes}{ ~~Describe \code{plotQualCodes} here~~ }
  \item{colMap}{ ~~Describe \code{colMap} here~~ }
  \item{verticalGrid}{ ~~Describe \code{verticalGrid} here~~ }
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
function(timeblobList, timeStart=start.timeblob(timeblobList), timeEnd=end.timeblob(timeblobList), plotQualCodes=F, colMap=list(good="black",maybe="orange",poor="red"), verticalGrid=T, ...) {
	if (is.data.frame(timeblobList)) { timeblobList <- list(timeblobList) }
	timeStart <- as.POSIXct(timeStart)
	timeEnd <- as.POSIXct(timeEnd)
	opar <- par(mar=c(3,4,1,1))
	on.exit(par(opar))
	plot(0, type="n", xlim=c(timeStart, timeEnd), ylim=c(0.5, length(timeblobList)+0.5), axes=F, ann=F, ...)
	axisTicks <- axis.POSIXct(1, timeStart)
	if (verticalGrid) {
		abline(v=axisTicks, col="grey", lty=2)
	}
	# draw series labels
	tmp_par <- par(xpd=T)
	text(timeStart, y=seq(1,length(timeblobList)), labels=names(timeblobList), adj=c(1,0.5))
	par(tmp_par)
	# calculate the data time lines
	for (k in seq(along=timeblobList)) {
		subBlob <- window.timeblob(timeblobList[[k]], timeStart, timeEnd, inclusive=T)
		if (nrow(subBlob)==0) { continue }
		
		thisNA <- is.na(subBlob[,2])
		
		# default, if plotQualCodes == FALSE
		thisCol <- rep(as.factor("black"), nrow(subBlob))
		
		if (plotQualCodes && !is.null(subBlob$Qual)) {
			thisCol <- applyColourMap(subBlob$Qual, colMap)
		}
		
		# vector of integer codes for each colour as well as NA
		colIntCodes <- as.integer(thisCol)
		colIntCodes[thisNA] <- -1 # unique code for NA
		
		# find indices (time steps) where colour (or is.na) changes
		# i.e. divide up into blocks of equal colour
		blockStart <- which(c(1,diff(colIntCodes))!=0)
		blockEnd <- c(blockStart[-1], nrow(subBlob)+1)
		
		# get subset of colours and convert to character strings
		thisCol.block <- levels(thisCol)[ thisCol[blockStart] ]
		# set colour to NA where data is NA
		thisCol.block[thisNA[blockStart]] <- NA
		
		# extrapolate last interval to same length as second-last
		thisTime <- c(subBlob$Time, end.timeblob(subBlob) + diff(as.numeric(subBlob$Time[nrow(subBlob)-c(1,0)])) )
		
		# plot this timeline
		rect(thisTime[blockStart], k-0.15, thisTime[blockEnd], k+0.15, col=thisCol.block, border=NA)
	}
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
