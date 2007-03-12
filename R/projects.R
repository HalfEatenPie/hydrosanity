## Hydrosanity: an interface for exploring hydrological time series in R
##
## Time-stamp: <2007-03-05 00:00:00 Felix>
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

on_menu_new_activate <- function(action, window) {hydrosanity()}
on_menu_open_activate <- function(action, window) {openProject()}
on_menu_save_activate <- function(action, window) {saveProject()}
on_menu_saveas_activate <- function(action, window) {saveProject(saveAs=T)}

openProject <- function() {
	theWidget("hs_window")$setSensitive(F)
	on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	ff <- c("Hydrosanity projects (.hydrosanity)", "*.hydrosanity")
	filename <- choose.files(caption="Open project", filters=ff, multi=F)
	theWidget("hs_window")$present()
	if (filename=="") { return() }
	
	load(filename, .GlobalEnv)
	#if (hsp$version < x) stop...
	hsp$projectFile <<- filename
	
	setTextview("log_textview", hsp$log)
	addLogSeparator()
	
	theWidget("notebook")$setCurrentPage(0)
	on_notebook_switch_page(theWidget("notebook"), theWidget("hs_window"), 0)
	
	setStatusBar("Loaded project from", filename)
}

saveProject <- function(saveAs=F) {
	theWidget("hs_window")$setSensitive(F)
	on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	filename <- hsp$projectFile
	if (is.null(filename)) { filename <- "" }
	if (saveAs==T || filename=="") {
		filename <- choose.file.save(filename, caption="Save project")
		theWidget("hs_window")$present()
		if (is.na(filename)) { return() }
	}
	
	if (get.extension(filename) != "hydrosanity")
	filename <- sprintf("%s.hydrosanity", filename)
	
	hsp$log <<- getTextviewText("log_textview")
	hsp$version <<- VERSION
	save(hsp, file=filename, compress=TRUE)
	hsp$projectFile <<- filename
	
	setStatusBar("Project saved to", filename)
}


