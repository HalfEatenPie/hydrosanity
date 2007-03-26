## Hydrosanity: an interface for exploring hydrological time series in R
##
## Time-stamp: <2007-03-05 00:00:00 Felix>
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

.hs_on_menu_new_activate <- function(action, window) {hydrosanity()}
.hs_on_menu_open_activate <- function(action, window) {openProject()}
.hs_on_menu_save_activate <- function(action, window) {saveProject()}
.hs_on_menu_saveas_activate <- function(action, window) {saveProject(saveAs=T)}

openProject <- function() {
	theWidget("hs_window")$setSensitive(F)
	on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	ff <- c("Hydrosanity projects (.hydrosanity)", "*.hydrosanity")
	filename <- choose.files(caption="Open project", filters=ff, multi=F)
	theWidget("hs_window")$present()
	if (filename=="") { return() }
	
	hydrosanity()
	load(filename, .GlobalEnv)
	#if (hsp$version < x) stop...
	hsp$projectFile <<- filename
	.hydrosanity$modified <<- F
	
	setTextview("log_textview", hsp$log)
	addLogSeparator()
	
	# switch to first page and trigger update
	theWidget("notebook")$setCurrentPage(0)
	.hs_on_notebook_switch_page(theWidget("notebook"), theWidget("hs_window"), 0)
	
	theWidget("import_options_expander")$setExpanded(FALSE)
	theWidget("import_makechanges_expander")$setExpanded(TRUE)
	
	setStatusBar("Loaded project from", filename)
}

saveProject <- function(saveAs=F) {
	theWidget("hs_window")$setSensitive(F)
	on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	ff <- c("Hydrosanity projects (.hydrosanity)", "*.hydrosanity")
	filename <- hsp$projectFile
	if (is.null(filename)) { filename <- "" }
	if (saveAs==T || filename=="") {
		filename <- choose.file.save(filename, caption="Save project",
			filters=ff)
		theWidget("hs_window")$present()
		if (is.na(filename)) { return() }
	}
	
	if (get.extension(filename) != "hydrosanity")
	filename <- sprintf("%s.hydrosanity", filename)
	
	hsp$log <<- getTextviewText("log_textview")
	hsp$version <<- VERSION
	save(hsp, file=filename, compress=TRUE)
	hsp$projectFile <<- filename
	.hydrosanity$modified <<- F
	
	setStatusBar("Project saved to", filename)
}


