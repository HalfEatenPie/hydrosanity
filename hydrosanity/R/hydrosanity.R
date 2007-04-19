## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL
## based on Rattle, (c) 2006 Graham.Williams@togaware.com
##

MAJOR <- "0"
MINOR <- "5"
REVISION <- unlist(strsplit("$Revision: 0 $", split=" "))[2]
VERSION <- paste(MAJOR, MINOR, REVISION, sep=".")
COPYRIGHT <- "(c) 2007 Felix Andrews <felix@nfrac.org>, GPL\n based on Rattle, (c) 2006 Graham.Williams@togaware.com"
WEBSITE <- "http://code.google.com/p/hydrosanity/"


## LICENSE
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version. See the file gpl-license.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

.KNOWN_FORMATS <- list(
	".au BoM daily rainfall"=
		c('read.timeblob',
		  'skip=1, sep=",", dataname="Rain (mm)", dataCol=6, qualCol=7, extraCols=c(9), extraNames=c("AccumSteps"), readTimesFromFile=F, startTime=list(year=3,month=4,day=5), timeSeqBy="DSTday"'),
	".au NSW Pinneena v8 streamflow (ML/day, default time format)"=
		c('read.timeblob',
		  'skip=3, sep=",", dataname="Flow (ML/day)", timeFormat="%H:%M_%d/%m/%Y", na.strings=c(\'""\')')
)
if (!exists("APPWIN")) { APPWIN <- NA }

hydrosanity <- function() {
	require(grid, quietly=TRUE)
	require(lattice, quietly=TRUE)
	require(RGtk2, quietly=TRUE) # From http://www.ggobi.org/rgtk2/
	require(cairoDevice, quietly=TRUE)
	
	if (exists('.hydrosanity') && !is.null(.hydrosanity$GUI)) {
		.hs_on_menu_quit_activate()
	}
	
	# create empty project variable
	hsp <<- list(data=list())
	
	# this global variable stores non-project state information
	.hydrosanity <<- list(
		dev=list(),
		win=list(),
		win.gui=list(),
		call=list(),
		id.call=list(),
		modified=F,
		update=list(
			import=T,
			timeperiod=T,
			explore=T,
			impute=T,
			corr=T
		)
	)
	
	.hydrosanity$GUI <<- gladeXMLNew(getpackagefile("hydrosanity.glade"),
		root="hs_window")
	APPWIN <<- theWidget("hs_window")
	
	# connect the callbacks (event handlers)
	gladeXMLSignalAutoconnect(.hydrosanity$GUI)
	gSignalConnect(APPWIN, "delete-event", .hs_on_menu_quit_activate)
	
	# set up log page
	addInitialLogMessage()
	
	theWidget("welcome_label")$setMarkup(paste(sep='',
		'<span foreground="#660066"><big><b>Welcome to Hydrosanity</b></big></span>', 
		' version ', VERSION, '\n', 
		gsub('[<>]','',COPYRIGHT), '\n', 
		'<tt>', WEBSITE, '</tt>'))
	
	# set up initial GUI state
	theWidget("notebook")$setCurrentPage(0)
	theWidget("import_file_radio_options_notebook")$setShowTabs(FALSE)
	theWidget("import_options_expander")$setExpanded(FALSE)
	theWidget("import_makechanges_expander")$setExpanded(FALSE)
	
	known_format_combo <- theWidget("import_known_format_combobox")
	known_format_combo$getModel()$clear()
	for (x in names(.KNOWN_FORMATS)) {
		known_format_combo$appendText(x)
	}
	theWidget("import_known_format_combobox")$setActive(0)
	theWidget("import_time_format_comboboxentry")$setActive(0)
	theWidget("import_time_format_codes_combobox")$setActive(0)
	theWidget("import_time_step_comboboxentry")$setActive(4)
	theWidget("import_makefactor_comboboxentry")$setActive(0)
	theWidget("explore_timeseries_aggr1_comboboxentry")$setActive(2)
	theWidget("explore_timeseries_aggr2_comboboxentry")$setActive(4)
	theWidget("explore_timeseries_yearstart_combobox")$setActive(0)
	theWidget("explore_cdf_aggr1_comboboxentry")$setActive(2)
	theWidget("explore_cdf_aggr2_comboboxentry")$setActive(4)
	theWidget("corr_smoothed_by_comboboxentry")$setActive(1)
	theWidget("corr_relationplot_lag_comboboxentry")$setActive(0)
	
	setTextviewMonospace(theWidget("log_textview"))
	setTextviewMonospace(theWidget("impute_textview"))
	
	# set up table format on import page
	importTreeView <- theWidget("import_summary_treeview")
	insertTreeViewTextColumns(importTreeView, 
		colNames=c("Name", "Data", "Start", "End", "Length", "Timestep", "Qual", "Extra_data", "Role"),
		editors=list(Name=.hs_on_import_summary_treeview_name_edited,
			Data=.hs_on_import_summary_treeview_dataname_edited,
			Role=.hs_on_import_summary_treeview_role_edited),
		combo=list(Role=data.frame(c("RAIN","FLOW","OTHER"))) )
	importTreeView$getSelection()$setMode("multiple")
	
	# set up table format on timeperiod page
	timeperiodTreeView <- theWidget("timeperiod_summary_treeview")
	insertTreeViewTextColumns(timeperiodTreeView, 
		colNames=c("Name", "Min", "Q25", "Median", "Q75", "Max", "Missing", ""))
	
	APPWIN$present()
}


addInitialLogMessage <- function() {
	addToLog(sprintf("## Hydrosanity version %s", VERSION), "\n",
sprintf("## Run by %s on %s", Sys.info()["user"], R.version.string), "\n\n",
"## Save the contents of this log to a file using the export button. As well as 
## keeping a record of the analysis procedure, it can be used to repeat the 
## actions by sending the same commands directly to R. For example, if the log
## has been exported to the file \"veryclever.R\" then in the R Console 
## 'source(\"veryclever.R\")' will run the commands in that file. Of course the 
## log can be edited and annotated, either by editing the exported file, or by 
## editing the text in this frame before exporting. Saving the Hydrosanity 
## project also retains this log.

library(hydrosanity)

## The variable hsp is used to store the current Hydrosanity Project. At any 
## time, type \"str(hsp)\" in the R Console to see what is stored there!

hsp <- list(data=list())")
	addLogSeparator()
}

datasetModificationUpdate <- function() {
	.hydrosanity$modified <<- T
	
	.hydrosanity$update$import <<- T
	.hydrosanity$update$timeperiod <<- T
	.hydrosanity$update$explore <<- T
	.hydrosanity$update$impute <<- T
	
	.hs_on_notebook_switch_page(
		page.num=theWidget("notebook")$getCurrentPage())
}

timeperiodModificationUpdate <- function() {
	.hydrosanity$modified <<- T
	
	.hydrosanity$update$timeperiod <<- T
	.hydrosanity$update$impute <<- T
	
	.hs_on_notebook_switch_page(
		page.num=theWidget("notebook")$getCurrentPage())
}


.hs_on_notebook_switch_page <- function(widget, page, page.num, ...) {
	APPWIN$setSensitive(F)
	on.exit(APPWIN$setSensitive(T))
	setStatusBar("")
	
	if (page.num == 1) {
		if (.hydrosanity$update$import) { updateImportPage() }
	}
	if (page.num == 2) {
		if (.hydrosanity$update$timeperiod) { updateTimePeriodPage() }
	}
	if (page.num == 3) {
		if (.hydrosanity$update$explore) { updateExplorePage() }
	}
	if (page.num == 4) {
		if (.hydrosanity$update$impute) { updateImputePage() }
	}
	if (page.num == 5) {
		if (.hydrosanity$update$corr) { updateCorrPage() }
	}
	APPWIN$present()
}


.hs_on_menu_quit_activate <- function(action, window) {
	if (.hydrosanity$modified && (length(hsp$data) > 0)) {
		if (!is.null(questionDialog("Save project?"))) {
			saveProject()
		}
	}
	for (i in dev.list()) { dev.off(i) }
	for (x in .hydrosanity$win) {
		try(x$destroy(), silent=TRUE)
	}
	APPWIN$destroy()
	.hydrosanity$GUI <<- NULL
}

.hs_on_menu_about_activate <-  function(action, window) {
	setStatusBar("")
	about <- gladeXMLNew(getpackagefile("hydrosanity.glade"), 
		root="aboutdialog")
	about$getWidget("aboutdialog")$setVersion(VERSION)
	about$getWidget("aboutdialog")$setCopyright(COPYRIGHT)
	about$getWidget("aboutdialog")$setWebsite(WEBSITE)
}

.hs_on_export_log_button_clicked <- function(button) {
	APPWIN$setSensitive(F)
	on.exit(APPWIN$setSensitive(T))
	setStatusBar("")
	
	filename <- choose.file.save("log.R", caption="Export Log", 
		filters=Filters[c("R","txt","All"),])
	APPWIN$present()
	if (is.na(filename)) { return() }
	
	if (get.extension(filename) == "") {
		filename <- sprintf("%s.R", filename)
	}
	
	write(getTextviewText(theWidget("log_textview")), filename)
	
	setStatusBar("The log has been exported to", filename)
}


sanitycheck.rain <- function(timeblobList) {
	if (is.data.frame(timeblobList)) { timeblobList <- list(timeblobList) }
	for (k in seq(along=timeblobList)) {
		cat("Sanity checking rainfall series", k, "(", names(timeblobList)[k], ") ...\n")
		rawdata <- timeblobList[[k]]$Data
		pctiles <- quantile(rawdata, na.rm=T)
		if (pctiles[["0%"]] != 0) {
			cat("  INSANITY: minimum not zero:", pctiles[["0%"]], "\n")
		} else {
			if (pctiles[["100%"]] < 1) {
				cat("  INSANITY: maximum less than 1:", pctiles[["100%"]], "\n")
			}
			if (pctiles[["50%"]] > 0) {
				cat("  INSANITY: median greater than zero:", pctiles[["50%"]], "\n")
			}
		}
	}
}


sanitycheck.flow <- function(timeblobList) {
	if (is.data.frame(timeblobList)) { timeblobList <- list(timeblobList) }
	for (k in seq(along=timeblobList)) {
		cat("Sanity checking streamflow series", k, "(", names(timeblobList)[k], ") ...\n")
		rawdata <- timeblobList[[k]]$Data
		pctiles <- quantile(rawdata, na.rm=T)
		if (pctiles[["0%"]] < 0) {
			cat("  INSANITY: minimum less than zero:", pctiles[["0%"]], "\n")
		} else {
			if (pctiles[["100%"]] < 1) {
				cat("  INSANITY: maximum less than 1:", pctiles[["100%"]], "\n")
			}
			if (mean(rawdata, na.rm=T) < 2 * pctiles[["50%"]]) {
				cat("  INSANITY: mean less than twice median (i.e. low skewness)", "\n")
			}
		}
	}
}


getpackagefile <- function(filename) {
	## Try firstly to load from the installed hydrosanity package
	## Otherwise, look locally.
	myPath <- ""
	result <- try(etc <- file.path(.path.package(package="hydrosanity")[1], "etc"), silent=TRUE)
	if (inherits(result, "try-error")) {
		myPath <- file.path("hydrosanity", "hydrosanity", "inst", "etc", filename)
	} else {
		myPath <- file.path(etc, filename)
	}
	if (!file.exists(myPath)) {
		stop("could not find file ", filename)
	} else {
		return(myPath)
	}
}

evalCallArgs <- function(myCall, pattern=".*") {
	for (i in seq(along=myCall)) {
		if ((mode(myCall) %in% "call") && (i == 1)) {
			next # don't eval function itself
		}
		if (length(grep(pattern, deparse(myCall[[i]])))>0) {
			myCall[[i]] <- eval(myCall[[i]], parent.frame(2))
		} else if (mode(myCall[[i]]) %in% c("call","list")) {
			myCall[[i]] <- evalCallArgs(myCall[[i]], pattern)
		}
	}
	return(myCall)
}

timestepTimeFormat <- function(timestep) {
	if (length(grep("month", timestep))>0) { return("%Y-%b") }
	if (length(grep("year", timestep))>0) { return("%Y") }
	return("")
}

getStem <- function(path) {
	path <- basename(path)
	hits <- gregexpr("\\.", path)[[1]]
	if (hits == -1) {
		path
	} else {
		substr(path, start=1, stop=hits[length(hits)]-1)
	}
}



