## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

updateCorrPage <- function() {
	setupIconView(theWidget("corr_iconview"))
	.hydrosanity$update$corr <<- F
	APPWIN$present()
}

.hs_on_corr_ccfplot_button_clicked <- function(button) {
	APPWIN$setSensitive(F)
	on.exit(APPWIN$setSensitive(T))
	setStatusBar("")
	
	selNames <- iconViewGetSelectedNames(theWidget("corr_iconview"))
	if (length(selNames) != 2) {
		errorDialog("Select two items to calculate their cross-correlation.")
		return()
	}
	nBlobs <- length(selNames)
	doRises <- theWidget("corr_ccfplot_flowrises_checkbutton")$getActive()
	doRank <- theWidget("corr_ccfplot_rank_checkbutton")$getActive()
	doRawData <- theWidget("corr_rawdata_radiobutton")$getActive()
	doSmoothed <- theWidget("corr_smoothed_radiobutton")$getActive()
	smoothedBy <- theWidget("corr_smoothed_by_comboboxentry")$getActiveText()
	
	addLogComment("Generate cross-correlation plot")
	
	rawdata.cmd <- bquote(hsp$data[.(selNames)])
	
	tmpObjs <- c('tmp.data')
	
	guiDo(isExpression=T, bquote(
		tmp.data <- sync.timeblobs(.(rawdata.cmd), timelim=hsp$timePeriod)
	))
	
	#if (any(sapply(tmp.data[-1], is.na))) {
	#	errorDialog("Selected items contain missing values. ",
	#		"Adjust the time period, or impute/redistribute missing values.")
	#	return()
	#}
	
	if (doRises) {
		if (identical(attr(hsp$data[[selNames[1]]], "role"), "FLOW")) {
			guiDo(tmp.data[[2]] <- rises(tmp.data[[2]]))
		}
		if (identical(attr(hsp$data[[selNames[2]]], "role"), "FLOW")) {
			guiDo(tmp.data[[3]] <- rises(tmp.data[[3]]))
		}
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
	
	if (doRank) { # Spearman's rho
		guiDo(tmp.data[[2]] <- rank(tmp.data[[2]], na.last="keep"))
		guiDo(tmp.data[[3]] <- rank(tmp.data[[3]], na.last="keep"))
	}
	
	tmpObjs <- c(tmpObjs, 'tmp.ccf')
	guiDo(tmp.ccf <- ccf(tmp.data[[2]], tmp.data[[3]], plot=F, 
		na.action=na.contiguous))
	
	plot.cmd <- quote(
		xyplot(CCF ~ Lag, data=data.frame(CCF=tmp.ccf$acf, Lag=tmp.ccf$lag),
			type="h", panel=function(...) {
				panel.abline(h=0)
				panel.xyplot(...)
			})
	)
	# ci <- 0.95; clim <- qnorm((1 + ci)/2)/sqrt(x$n.used)
	
	# hydrosanity caption
	addToLog("## Make hydrosanity caption")
	tmpObjs <- c(tmpObjs, 'tmp.n', 'tmp.caption')
	guiDo(isExpression=T, bquote({
		tmp.n <- sum(!is.na(tmp.data[[2]]) & !is.na(tmp.data[[3]]))
		tmp.caption <- hydrosanity.caption(
			range(tmp.data$Time), # TODO: should add timestep to this
			by=.(attr(tmp.data, "timestep")), n=tmp.n, series=1)
	}))
	plot.cmd$sub <- quote(tmp.caption)
	
	setPlotDevice("cross-correlation")
	setCairoWindowButtons("cross-correlation", zoomin=T)
	
	result <- guiDo(plot.cmd, isExpression=T)
	# plot trellis object
	guiDo(bquote(print(.(result))), isExpression=T, doLog=F)
	
	.hydrosanity$call[["cross-correlation"]] <<- evalCallArgs(plot.cmd, pattern="^tmp")
	
	if (length(tmpObjs) > 0) {
		addToLog(paste('rm(', paste(tmpObjs, collapse=', '), ')', sep=''))
		rm(list=tmpObjs, envir=.GlobalEnv)
	}
	setStatusBar("Generated cross-correlation plot")
}

.hs_on_corr_relationplot_button_clicked <- function(button) {
	APPWIN$setSensitive(F)
	on.exit(APPWIN$setSensitive(T))
	setStatusBar("")
	
	selNames <- iconViewGetSelectedNames(theWidget("corr_iconview"))
	if (length(selNames) != 2) {
		errorDialog("Select two items to calculate their cross-correlation.")
		return()
	}
	nBlobs <- length(selNames)
	lagSpec <- theWidget("corr_relationplot_lag_comboboxentry")$getActiveText()
	doRises <- theWidget("corr_relationplot_flowrises_checkbutton")$getActive()
	
	addLogComment("Generate rainfall-runoff relationship plot")
	
	rawdata.cmd <- bquote(hsp$data[.(selNames)])
	
	tmpObjs <- c('tmp.data')
	
	guiDo(isExpression=T, bquote(
		tmp.data <- sync.timeblobs(.(rawdata.cmd), timelim=hsp$timePeriod)
	))
	
	if (doRises) {
		if (identical(attr(hsp$data[[selNames[1]]], "role"), "FLOW")) {
			guiDo(tmp.data[[2]] <- rises(tmp.data[[2]]))
		}
		if (identical(attr(hsp$data[[selNames[2]]], "role"), "FLOW")) {
			guiDo(tmp.data[[3]] <- rises(tmp.data[[3]]))
		}
	}
	
	tmpObjs <- c(tmpObjs, 'tmp.lag')
	if (identical(lagSpec, "best correlation")) {
		addToLog("## Shift indices by the lag with highest correlation")
		tmpObjs <- c(tmpObjs, 'tmp.ccf')
		guiDo(tmp.ccf <- ccf(tmp.data[[2]], tmp.data[[3]], plot=F, 
			na.action=na.contiguous))
		guiDo(tmp.lag <- tmp.ccf$lag[which.max(tmp.ccf$acf)])
	} else {
		addToLog("## Shift indices by the specified lag")
		lagSteps <- round(as.numeric.byString(lagSpec) / 
			as.numeric.byString(attr(tmp.data, "timestep")))
		guiDo(isExpression=T, bquote(tmp.lag <- .(lagSteps)))
	}
	# lag.timeblob?
	guiDo({
		tmp.z <- 1:nrow(tmp.data) - tmp.lag
		tmp.z[(tmp.z < 1) | (tmp.z > nrow(tmp.data))] <- NA
		tmp.data[[3]] <- tmp.data[[3]][tmp.z]
	})
	
	plot.cmd <- call('xyplot')
	plot.cmd[[2]] <- bquote(
		.(as.symbol(names(tmp.data)[2])) ~ .(as.symbol(names(tmp.data)[3])))
	plot.cmd[[3]] <- quote(tmp.data)
	plot.cmd$scales$log <- T
	#plot.cmd$type <- c("p", "smooth") breaks with missing values, so:
	plot.cmd$panel <- function(x, y, ...) {
		panel.xyplot(x, y, ...)
		ok <- is.finite(x) & is.finite(y)
		panel.loess(x[ok], y[ok], span=1,
		col.line=trellis.par.get("superpose.line")$col[2],
		lty=trellis.par.get("superpose.line")$lty[2], ...)
	}
	plot.cmd$xscale.components <- quote(lattice.x.prettylog)
	plot.cmd$yscale.components <- quote(lattice.y.prettylog)
	
	# hydrosanity caption
	addToLog("## Make hydrosanity caption")
	tmpObjs <- c(tmpObjs, 'tmp.n', 'tmp.caption')
	guiDo(isExpression=T, bquote({
		tmp.n <- sum(!is.na(tmp.data[[2]]) & !is.na(tmp.data[[3]]))
		tmp.caption <- hydrosanity.caption(
			range(tmp.data$Time), # TODO: should add timestep to this
			by=.(attr(tmp.data, "timestep")), n=tmp.n, series=1)
	}))
	plot.cmd$sub <- quote(tmp.caption)
	
	setPlotDevice("rainfall-runoff")
	setCairoWindowButtons("rainfall-runoff", identify=T, zoomin=T, log=T)
	
	result <- guiDo(plot.cmd, isExpression=T)
	# plot trellis object
	guiDo(bquote(print(.(result))), isExpression=T, doLog=F)
	
	.hydrosanity$call[["rainfall-runoff"]] <<- evalCallArgs(plot.cmd, pattern="^tmp")
	
	# construct call to panel.identify() for later use
	id.cmd <- call('panel.identify')
	id.cmd$labels <- format(tmp.data$Time, 
		timestepTimeFormat(attr(tmp.data, "timestep")))
	.hydrosanity$id.call[["rainfall-runoff"]] <<- id.cmd
	
	if (length(tmpObjs) > 0) {
		addToLog(paste('rm(', paste(tmpObjs, collapse=', '), ')', sep=''))
		rm(list=tmpObjs, envir=.GlobalEnv)
	}
	setStatusBar("Generated rainfall-runoff relationship plot")
}

