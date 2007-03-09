## Hydrosanity: an interface for exploring hydrological time series in R
##
## Time-stamp: <2007-03-05 00:00:00 Felix>
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

updateExplorePage <- function() {
	
	rainPixbuf <- gdkPixbufNewFromFile(getpackagefile("icon_RAIN.png"))$retval
	flowPixbuf <- gdkPixbufNewFromFile(getpackagefile("icon_FLOW.png"))$retval
	otherPixbuf <- gdkPixbufNewFromFile(getpackagefile("icon_OTHER.png"))$retval
	# (or NULL)
	
	#dfModel <- rGtkDataFrame(as.data.frame(names(hsp$data)))
	#dfModel$appendColumns(myPixbuf)
	
	list_store <- gtkListStore("character", "GdkPixbuf")
	
	for (i in seq(along=hsp$data)) {
		iter <- list_store$append()$iter
		list_store$set(iter, 0, names(hsp$data)[i])
		list_store$set(iter, 1, switch(attr(hsp$data[[i]], "role"),
			"RAIN"=rainPixbuf,
			"FLOW"=flowPixbuf,
			otherPixbuf)
		)
	}
	theWidget("explore_iconview")$setModel(list_store)
	theWidget("explore_iconview")$setTextColumn(0)
	theWidget("explore_iconview")$setPixbufColumn(1)
	theWidget("explore_iconview")$setItemWidth(50)	
	
	theWidget("hs_window")$present()
}

on_explore_timeseries_button_clicked <- function(button) {
	#theWidget("hs_window")$setSensitive(F)
	#on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	selNames <- iconViewGetSelectedNames(theWidget("explore_iconview"))
	
	setPlotDevice("explore")
	timeseriesplot(hsp$data[selNames], timeStart=hsp$timePeriod[1], timeEnd=hsp$timePeriod[2]) #, interactive=T)
}

on_explore_cdf_button_clicked <- function(button) {
	#theWidget("hs_window")$setSensitive(F)
	#on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	selNames <- iconViewGetSelectedNames(theWidget("explore_iconview"))
	
	setPlotDevice("explore")
	fdcplot(hsp$data[selNames], timeStart=hsp$timePeriod[1], timeEnd=hsp$timePeriod[2], plotQualCodes=T)
}

on_explore_seasonal_button_clicked <- function(button) {
	#theWidget("hs_window")$setSensitive(F)
	#on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	selNames <- iconViewGetSelectedNames(theWidget("explore_iconview"))
	
	setPlotDevice("explore")
	monthlyboxplot(hsp$data[[selNames[1]]])
}

