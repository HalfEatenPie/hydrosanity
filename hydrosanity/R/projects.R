## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

.hs_on_menu_new_activate <- function(action, window) {hydrosanity()}
.hs_on_menu_open_activate <- function(action, window) {openProject()}
.hs_on_menu_save_activate <- function(action, window) {saveProject()}
.hs_on_menu_saveas_activate <- function(action, window) {saveProject(saveAs=T)}

openProject <- function() {
	APPWIN$setSensitive(F)
	on.exit(APPWIN$setSensitive(T))
	setStatusBar("")
	
	ff <- c("Hydrosanity projects (.hydrosanity)", "*.hydrosanity")
	filename <- choose.files(caption="Open project", filters=ff, multi=F)
	APPWIN$present()
	if (filename=="") { return() }
	
	hydrosanity()
	load(filename, .GlobalEnv)
	hsp$projectFile <<- filename
	.hydrosanity$modified <<- F
	
	setTextview(theWidget("log_textview"), hsp$log)
	addLogSeparator()
	hsp$log <<- NULL
	
	if (is.null(hsp$version) ||
		package_version(hsp$version) < package_version("0.5")) {
		errorDialog("The version of Hydrosanity used to save this project used a different data structure. Maybe you can manually fix hsp$data?")
		stop("Project file version not supported")
	} else
	if (package_version(hsp$version) < package_version("0.6")) {
		# rename a Qual factor level
		for (i in seq(along=hsp$data)) {
			oldLevels <- levels(hsp$data[[i]]$Qual)
			oldLevels[oldLevels == "maybe"] <- "suspect"
			levels(hsp$data[[i]]$Qual) <<- oldLevels
		}
	}
	hsp$version <<- NULL
	
	# switch to first page and trigger update
	datasetModificationUpdate()
	theWidget("notebook")$setCurrentPage(1)
	
	theWidget("import_options_expander")$setExpanded(FALSE)
	theWidget("import_makechanges_expander")$setExpanded(TRUE)
	
	setStatusBar("Loaded project from", filename)
}

saveProject <- function(saveAs=F) {
	APPWIN$setSensitive(F)
	on.exit(APPWIN$setSensitive(T))
	setStatusBar("")
	
	ff <- c("Hydrosanity projects (.hydrosanity)", "*.hydrosanity")
	filename <- hsp$projectFile
	if (is.null(filename)) { filename <- "" }
	if (saveAs==T || filename=="") {
		filename <- choose.file.save(filename, caption="Save project",
			filters=ff)
		APPWIN$present()
		if (is.na(filename)) { return() }
	}
	
	if (get.extension(filename) != "hydrosanity")
	filename <- sprintf("%s.hydrosanity", filename)
	
	hsp$log <<- getTextviewText(theWidget("log_textview"))
	hsp$version <<- VERSION
	save(hsp, file=filename, compress=TRUE)
	hsp$log <<- NULL
	hsp$version <<- NULL
	
	hsp$projectFile <<- filename
	.hydrosanity$modified <<- F
	
	setStatusBar("Project saved to", filename)
}


