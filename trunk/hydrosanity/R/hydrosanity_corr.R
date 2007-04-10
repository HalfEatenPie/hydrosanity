## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

updateCorrPage <- function() {
	setupIconView(theWidget("corr_iconview"))
	.hydrosanity$update$corr <<- F
	theWidget("hs_window")$present()
}

.hs_on_corr_corrplot_button_clicked <- function(button) {
	theWidget("hs_window")$setSensitive(F)
	on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	selNames <- iconViewGetSelectedNames(theWidget("corr_iconview"))
	if (length(selNames) != 2) {
		errorDialog("Select two items to calculate their cross-correlation.")
		return()
	}
	nBlobs <- length(selNames)
	doCorrPlot <- theWidget("corr_corrplot_radiobutton")$getActive()
	doRelationPlot <- theWidget("corr_relationplot_radiobutton")$getActive()
	doRawData <- theWidget("corr_rawdata_radiobutton")$getActive()
	doSmoothed <- theWidget("corr_smoothed_radiobutton")$getActive()
	smoothedBy <- theWidget("corr_smoothed_by_comboboxentry")$getActiveText()
	
	addLogComment("Generate cross-correlation plot")
	
	rawdata.cmd <- if (nBlobs == length(hsp$data)) {
		quote(hsp$data)
	} else {
		bquote(hsp$data[.(selNames)])
	}
	
	tmpObjs <- c('tmp.data')
	
	guiDo(isExpression=T, bquote(
		tmp.data <- sync.timeblobs(.(rawdata.cmd), timelim=hsp$timePeriod)
	))
	
	if (any(sapply(tmp.data[-1], is.na))) {
		errorDialog("Selected items contain missing values. ",
			"Adjust the time period, or impute missing values.")
		return()
	}
	
	if (doSmoothed) {
		delta <- as.numeric.byString(attr(tmp.data, "timestep"))
		smoothDelta <- as.numeric.byString(smoothedBy)
		winSize <- round(smoothDelta / delta)
		
		for (x in 2:3) {
			guiDo(isExpression=T, bquote(
				tmp.data[[.(x)]] <- filter(tmp.data[[.(x)]], 
					rep(1/.(winSize), .(winSize)))
			))
		}
	}
	
	tmpObjs <- c(tmpObjs, 'tmp.ccf')
	guiDo(tmp.ccf <- ccf(tmp.data[[2]], tmp.data[[3]], plot=F, 
		na.action=na.contiguous))
	
	# hydrosanity caption
	addToLog("## Make hydrosanity caption")
	tmpObjs <- c(tmpObjs, 'tmp.n', 'tmp.caption')
	guiDo(isExpression=T, bquote({
		tmp.n <- sum(!is.na(tmp.data[[2]]) & !is.na(tmp.data[[3]]))
		tmp.caption <- hydrosanity.caption(
			range(tmp.data$Time), # TODO: should add timestep to this
			by=.(attr(tmp.data, "timestep")), n=tmp.n, series=1)
	}))
	
	plot.cmd <- quote(
		xyplot(CCF ~ Lag, data=data.frame(CCF=tmp.ccf$acf, Lag=tmp.ccf$lag),
			type="h", panel=function(...) {
				panel.abline(h=0)
				panel.xyplot(...)
			})
	)
	# ci <- 0.95; clim <- qnorm((1 + ci)/2)/sqrt(x$n.used)
	
	if (doRelationPlot) {
		addToLog("## Shift indices by the lag with highest correlation")
		tmpObjs <- c(tmpObjs, 'tmp.bestLag', 'tmp.z')
		guiDo({
			tmp.bestLag <- tmp.ccf$lag[which.max(tmp.ccf$acf)]
			tmp.z <- 1:nrow(tmp.data) - tmp.bestLag
			tmp.z[(tmp.z < 1) | (tmp.z > nrow(tmp.data))] <- NA
			tmp.data[[3]] <- tmp.data[[3]][tmp.z]
		})
		plot.cmd <- call('xyplot')
		plot.cmd[[2]] <- bquote(
			.(as.symbol(names(tmp.data)[2])) ~ .(as.symbol(names(tmp.data)[3])))
		plot.cmd[[3]] <- quote(tmp.data)
		plot.cmd$scales$log <- T
		#plot.cmd$type <- c("p", "smooth")
		#plot.cmd$na.action <- quote(na.omit)
		plot.cmd$panel <- function(x, y, ...) {
			panel.xyplot(x, y, ...)
			ok <- is.finite(x) & is.finite(y)
			panel.loess(x[ok], y[ok], span=0.9,
			col.line=trellis.par.get("superpose.line")$col[2],
			lty=trellis.par.get("superpose.line")$lty[2], ...)
		}
		plot.cmd$xscale.components <- quote(lattice.x.prettylog)
		plot.cmd$yscale.components <- quote(lattice.y.prettylog)
	}
	
	plot.cmd$sub <- quote(tmp.caption)
	
	setPlotDevice("correlation")
	setCairoWindowButtons("correlation", identify=T, zoomin=T, log=T)
	
	result <- guiDo(plot.cmd, isExpression=T)
	# plot trellis object
	guiDo(bquote(print(.(result))), isExpression=T, doLog=F)
	
	.hydrosanity$call[["correlation"]] <<- evalCallArgs(plot.cmd, pattern="^tmp")
	
	if (doRelationPlot) {
		# construct call to panel.identify() for later use
		id.cmd <- call('panel.identify')
		id.cmd$labels <- format(tmp.data$Time, 
			timestepTimeFormat(attr(tmp.data, "timestep")))
		.hydrosanity$id.call[["correlation"]] <<- id.cmd
	}
	
	if (length(tmpObjs) > 0) {
		addToLog(paste('rm(', paste(tmpObjs, collapse=', '), ')', sep=''))
		rm(list=tmpObjs, envir=.GlobalEnv)
	}
	setStatusBar("Generated cross-correlation plot")
}

