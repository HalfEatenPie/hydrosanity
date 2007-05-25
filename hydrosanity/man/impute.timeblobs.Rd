\name{impute.timeblobs}
\alias{impute.timeblobs}
\alias{imputeGaps.timeblobs}
\alias{disaccumulate.timeblobs}
\alias{quick.disaccumulate.timeblob}
\alias{unimpute.timeblobs}
\title{Fill Gaps in Time Series}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
impute.timeblobs(blob.list, which.impute = names(blob.list), timelim = NULL, withinTimeframe = NA, method = c("distance", "constant"), constant = c("mean", "zero", "extend"), trim = 0)

imputeGaps.timeblobs(blob.list, which.impute = names(blob.list), maxGapLength = Inf, internalGapsOnly = F, ...)

disaccumulate.timeblobs(blob.list, which.impute = names(blob.list), ...)

quick.disaccumulate.timeblob(blob)

unimpute.timeblobs(blob.list, timelim = NULL, type = c("imputed", "disaccumulated"))
}
\arguments{
  \item{blob.list}{ ~~Describe \code{blob.list} here~~ }
  \item{which.impute}{ ~~Describe \code{which.impute} here~~ }
  \item{timelim}{ ~~Describe \code{timelim} here~~ }
  \item{withinTimeframe}{ ~~Describe \code{withinTimeframe} here~~ }
  \item{method}{ ~~Describe \code{method} here~~ }
  \item{constant}{ ~~Describe \code{constant} here~~ }
  \item{trim}{ ~~Describe \code{trim} here~~ }
  \item{maxGapLength}{ ~~Describe \code{maxGapLength} here~~ }
  \item{internalGapsOnly}{ ~~Describe \code{internalGapsOnly} here~~ }
  \item{blob}{ ~~Describe \code{blob} here~~ }
  \item{type}{ ~~Describe \code{type} here~~ }
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
function(blob.list, which.impute=names(blob.list), timelim=NULL, withinTimeframe=NA, maxGapLength=NA, internalGapsOnly=F, method=c("constant", "distance", "correlation", "regression"), constant=c("mean", "trim.mean", "zero", "extend"), trim=0.01, maxPredictors=3, disaccumulate=c("column", "gaps", "no"), accumStepsColumn="AccumSteps", maxGapLengthAccum="4 days") {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	if (!is.null(timelim)) {
		timelim <- as.POSIXct(timelim)
		if (any(is.na(timelim))) { stop("'timelim' must be a pair of valid times (POSIXt)") }
	}
	method <- match.arg(method)
	disaccumulate <- match.arg(disaccumulate)
	constant <- match.arg(constant)
	doAccum <- (disaccumulate != "no")
	
	# a list will be returned with the imputed blobs
	newBlobs <- blob.list[which.impute]
	
	# take just the time-period window of all timeblobs, 
	#   but keep its offset in the original data (so can update later)
	blobs <- NULL
	subLims <- NULL
	if (!is.null(timelim)) {
		blobs <- lapply(blob.list, window, timelim[1], timelim[2])
		subLims <- lapply(blob.list, window, timelim[1], timelim[2],
			return.indices=T)
	} else {
		blobs <- blob.list
	}
	
	# find all multiple accumulations (according to user definition)
	# and extract each accumulated value into a new column "AccumValue"
	# (so that raw data correctly indicates a missing value there)
	# (also make AccumSteps column if does not already exist)
	for (x in names(blobs)) {
		if (!doAccum) { break }
		spanEnd <- c()
		spanLength <- c()
		spanAccum <- c()
		# find all gaps less than maxGapLengthAccum
		if (disaccumulate == "gaps") {
			gapInfo <- gaps(blobs[[x]]$Data,
				internal.only=internalGapsOnly)
			maxGapStepsAccum <- if (is.na(maxGapLengthAccum))
				{ Inf } else { round(
				as.numeric.byString(maxGapLengthAccum)
				/ as.numeric.byString(attr(blobs[[x]], "timestep"))) }
			ok <- (gapInfo$gap.length <= maxGapStepsAccum)
			spanEnd <- gapInfo$gap.end[ok] + 1 # end at accum step
			spanLength <- gapInfo$gap.length[ok] + 1 # includes accum
		}
		# find gaps according to specified column (accumStepsColumn)
		if (disaccumulate == "column") {
			spanEnd <- which(blobs[[x]][[accumStepsColumn]] > 1)
			spanLength <- blobs[[x]][[accumStepsColumn]][spanEnd]
			# drop first gap if it is incomplete (cut off by 'window')
			if ((length(spanEnd) > 0) && (spanEnd[1] <= spanLength[1])) {
				spanEnd <- spanEnd[-1]
				spanLength <- spanLength[-1]
			}
		}
		# store length and sum of accumulations
		accumSteps <- rep(integer(1), nrow(blobs[[x]]))
		accumSteps[spanEnd] <- spanLength
		blobs[[x]][["AccumSteps"]] <- accumSteps
		accumVal <- rep(0, nrow(blobs[[x]]))
		accumVal[spanEnd] <- blobs[[x]]$Data[spanEnd]
		blobs[[x]][["AccumVal"]] <- accumVal
		# set accumulated observation to NA (it is part of the gap)
		blobs[[x]]$Data[spanEnd] <- NA
	}
	
	# exclude any "imputed" values: do not use imputed values to impute
	rawBlobs <- blobs
	for (x in names(rawBlobs)) {
		rawBlobs[[x]]$Data[ (blobs[[x]]$Qual == "imputed")
				| (blobs[[x]]$Qual == "disaccumulated") ] <- NA
	}
	
	for (blobName in which.impute) {
		print(paste("going to impute site", blobName))
		
		blob <- blobs[[blobName]]
		blobIndex <- match(blobName, names(blobs))
		levels(blob$Qual) <- union(levels(blob$Qual), c("imputed", "disaccumulated"))
		# synchronise all other data to this blob
		rawSync <- syncTo.timeblobs(rawBlobs, blob)
		
		print("original gaps:")
		str(gaps(blob$Data))
		
		repeat {
			
			# find all gaps up to maxGapLength
			maxGapSteps <- if (is.na(maxGapLength)) { Inf } else
				{ round(as.numeric.byString(maxGapLength)
				/ as.numeric.byString(attr(blob, "timestep"))) }
			gapInfo <- gaps(blob$Data, internal.only=internalGapsOnly)
			ok <- (gapInfo$gap.length <= maxGapSteps)
			gapEnd <- gapInfo$gap.end[ok]
			gapLength <- gapInfo$gap.length[ok]
			gapStart <- gapEnd - gapLength + 1
			print(paste(length(gapEnd), "gaps"))
			# if there are not any more gaps, then stop
			if (length(gapEnd)==0) { break }
			# concatenated indices of all the time steps in gaps (whee!)
			allGaps <- sequence(gapLength) + rep(gapStart, times=gapLength) - 1
			
			if (method == "regression") {
				# Linear Model imputation:
				# start by predicting with as many sites as possible,
				# but some gaps will need to use fewer sites, so repeat
				
				# include this blob in the data frame in current form
				# (may have gaps already filled in, unlike rawBlobs)
				rawSync[[blobName]] <- blob$Data
				
				# find which other sites have data to fill in these gaps
				hasGapData <- !is.na(rawSync[allGaps, -c(1,blobIndex)])
				# find biggest set of sites which have simultaneous data
				# (so can use them all to predict)
				maxSet <- hasGapData[which.max(apply(hasGapData, 1, sum)),]
				print(paste("predicting with", sum(maxSet), "sites"))
				# if there are is more available data for imputing, then stop
				if (sum(maxSet) == 0) { break }
				predictors <- colnames(hasGapData)[maxSet]
				# construct model
				newFormula <- as.formula(paste(blobName, "~ 0 +", 
					paste(predictors, collapse=" + ")))
				print(newFormula)
				dataOK <- complete.cases(rawSync[c(blobName, predictors)])
				if (sum(dataOK)==0) {
					print("gah -- suddenly no data here")
					next
				}
				newModel <- lm(newFormula, rawSync[dataOK,])#, na.action=na.omit)
				#print(summary(newModel))
				#print(summary(step(newModel, k=log(sum(dataOK)), trace=0)))
				gapPred <- predict(newModel, newdata=rawSync[allGaps,])
				# limit below by zero; but should not have negative coef! (TODO)
				gapPred <- pmax(0, gapPred)
				gapPredOK <- !is.na(gapPred)
				print(paste("predicted", sum(gapPredOK), "out of", length(allGaps), "timesteps"))
				# TODO: store upper and lower confidence bounds in extra columns
				blob$Data[ allGaps[gapPredOK] ] <- gapPred[gapPredOK]
				blob$Qual[ allGaps[gapPredOK] ] <- "imputed"
			}
			
			if (method == "correlation") {
				# each gap: find predictor sites (complete coverage...)
				
				# calculate scaling matrix:
				# ratio of some function (eg mean) between each pair of sites
				# (using only pairwise-complete observations)
				# (be sure to account for AccumSteps in calculating mean)
				
				
			}
			
			if (method == "constant") {
				val <- NA
				if (constant \%in\% c("mean", "trim.mean")) {
					if (constant == "mean") { trim <- 0 }
					if (doAccum) {
						accums <- (blob$AccumSteps > 1)
						allVals <- with(blob, c(
							Data[!is.na(Data)],
							rep(AccumVal[accums] / AccumSteps[accums],
								times=AccumSteps[accums])
						))
						val <- mean(allVals, trim=trim)
					} else {
						val <- mean(blob$Data, trim=trim, na.rm=T)
					}
				}
				if (constant == "zero") {
					val <- 0
				}
				if (constant == "extend") {
					# find value preceding each gap
					# if data begins with gap, just use 0
					gapAnte <- blob$Data[ifelse(gapStart==1,1,gapStart-1)]
					gapAnte[is.na(gapAnte)] <- 0
					val <- rep(gapAnte, times=gapLength)
				}
				blob$Data[allGaps] <- val
				blob$Qual[allGaps] <- "imputed"
			}
		}
		
		if (doAccum) {
			accumSteps <- blob$AccumSteps
			accumVal <- blob$AccumVal
			spanEnd <- which(blob$AccumSteps > 1)
			spanLength <- blob$AccumSteps[spanEnd]
			spanStart <- spanEnd - spanLength + 1
			spanAccum <- blob$AccumVal[spanEnd]
			cumSum <- cumsum(ifelse(is.na(blob$Data), 0, blob$Data))
			spanSum <- cumSum[spanEnd] - 
				ifelse(spanStart==0, 0, cumSum[spanStart-1])
			
			# TODO: check whether the whole gap was filled in;
			#       otherwise cancel it
			
			# concatenated indices of all the time steps in accums (whee!)
			allSpans <- sequence(spanLength) + rep(spanStart, times=spanLength) - 1
			
			spanReScale <- rep(spanAccum / spanSum, times=spanLength)
			blob$Data[allSpans] <- blob$Data[allSpans] * spanReScale
			blob$Qual[allSpans] <- "disaccumulated"
			blob$AccumSteps[allSpans] <- 1
		}
		
		# update the copy of original data ('newBlobs')
		levels(newBlobs[[blobName]]$Qual) <- union(
			levels(newBlobs[[blobName]]$Qual), levels(blob$Qual[,drop=T]))
		# find subscripts in original data for specified 'timelim'
		blobWindow <- if (is.null(subLims)) {
			seq(along=newBlobs[[blobName]]$Data)
		} else {
			seq(subLims[[blobName]][1], subLims[[blobName]][2])
		}
		newBlobs[[blobName]]$Data[blobWindow] <- blob$Data
		newBlobs[[blobName]]$Qual[blobWindow] <- blob$Qual
		if (disaccumulate == "column") {
			newBlobs[[blobName]][[accumStepsColumn]][blobWindow] <- blob$AccumSteps
		}
	}
	
	return(newBlobs)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
