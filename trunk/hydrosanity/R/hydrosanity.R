## Hydrosanity: an interface for exploring hydrological time series in R
##
## Time-stamp: <2007-03-05 00:00:00 Felix>
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL
## based on Rattle, (c) 2006 Graham.Williams@togaware.com
##

MAJOR <- "0"
MINOR <- "3"
REVIS <- "2"
VERSION <- paste(MAJOR, MINOR, REVIS, sep=".")
COPYRIGHT <- "(c) 2007 Felix Andrews <felix@nfrac.org>, GPL\n based on Rattle, (c) 2006 Graham.Williams@togaware.com"

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
		  'skip=1, sep=",", dataName="Rain (mm)", dataCol=6, qualCol=7, extraCols=c(9), extraNames=c("AccumSteps"), readTimesFromFile=F, startTime=list(year=3,month=4,day=5), timeSeqBy="DSTday"'),
	".au NSW Pinneena v8 streamflow (ML/day, default time format)"=
		c('read.timeblob',
		  'skip=3, sep=",", dataName="Flow (ML/day)", timeFormat="%H:%M_%d/%m/%Y", na.strings=c(\'""\')')
)

hydrosanity <- function() {
	require(RGtk2, quietly=TRUE) # From http://www.ggobi.org/rgtk2/
	require(cairoDevice, quietly=TRUE)
	
	if (exists('.hydrosanity') && !is.null(.hydrosanity$GUI)) {
		on_menu_quit_activate()
	}
	
	hsp <<- blankStateHSP()
	.hydrosanity <<- list()
	.hydrosanity$dev <<- list()
	.hydrosanity$win <<- list()
	
	.hydrosanity$GUI <<- gladeXMLNew(getpackagefile("hydrosanity.glade"),
		root="hs_window")
	
	# connect the callbacks (event handlers)
	gladeXMLSignalAutoconnect(.hydrosanity$GUI)
	gSignalConnect(theWidget("hs_window"), "delete-event", on_menu_quit_activate)
	
	# set up initial GUI state
	theWidget("notebook")$setCurrentPage(0)
	
	theWidget("import_file_radio_options_notebook")$setShowTabs(FALSE)
	#theWidget("import_options_expander")$setExpanded(FALSE)
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
	
	# set up table format on import page
	importTreeView <- theWidget("import_summary_treeview")
	insertTreeViewTextColumns(importTreeView, 
		colNames=c("Name", "Start", "End", "Length", "Avg_Freq", "Data", "Qual", "Extra_columns", "Role"),
		editors=list(Name=on_import_summary_treeview_name_edited,
			Role=on_import_summary_treeview_role_edited),
		combo=list(Role=data.frame(c("RAIN","FLOW","OTHER"))) )
	importTreeView$getSelection()$setMode("multiple")
	#myTreeView$setHeadersClickable(TRUE)
	
	# set up table format on timeperiod page
	timeperiodTreeView <- theWidget("timeperiod_summary_treeview")
	insertTreeViewTextColumns(timeperiodTreeView, 
		colNames=c("Name", "Min", "Q25", "Median", "Q75", "Max", "Missing", ""))
	
	# set up log page
	setTextviewMonospace("log_textview")
	addInitialLogMessage()
	
	while (gtkEventsPending()) gtkMainIterationDo(blocking=F) # redraw
}


blankStateHSP <- function() {
	## PROJECT STATE VARIABLE
	list(
		data=list(),
		timePeriod=NULL,
		timeStep="1 DSTday",
		cwd=getwd(),
		defaultImportOptions=
		'sep=",", skip=1, dataName="Data", dataCol=2, qualCol=3'
	)
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

## The variable hsp is used to store the current Hydrosanity Project. It is 
## initialised here to be empty, but will hold data and settings when needed.
## Type \"str(hsp)\" in the R Console to see what is stored there!

hsp <- blankStateHSP()
")
	addLogSeparator()
}


on_notebook_switch_page <- function(notebook, window, page) {
	theWidget("hs_window")$setSensitive(F)
	on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	# page is the index of the page switched to.
	if (page == 0) {
		updateImportPage()
	}
	if (page == 1) {
		updateTimePeriodPage()
	}
	if (page == 2) {
		updateExplorePage()
	}
	#pageLabel <- nb$getTabLabelText(nb$getNthPage(page))
}


on_menu_quit_activate <- function(action, window) {
	if (length(hsp$data) > 0) {
		if (!is.null(questionDialog("Save project?"))) {
			saveProject()
		}
	}
	for (i in dev.list()) { dev.off(i) }
	for (x in .hydrosanity$win) {
		try(x$destroy(), silent=TRUE)
	}
	theWidget("hs_window")$destroy()
	.hydrosanity$GUI <<- NULL
}

on_menu_about_activate <-  function(action, window) {
	setStatusBar("")
	about <- gladeXMLNew(getpackagefile("hydrosanity.glade"), 
		root="aboutdialog")
	about$getWidget("aboutdialog")$setVersion(VERSION)
	about$getWidget("aboutdialog")$setCopyright(COPYRIGHT)
}

on_export_log_button_clicked <- function(button) {
	theWidget("hs_window")$setSensitive(F)
	on.exit(theWidget("hs_window")$setSensitive(T))
	setStatusBar("")
	
	filename <- choose.file.save(caption="Export Log")
	theWidget("hs_window")$present()
	if (is.na(filename)) { return() }
	
	if (get.extension(filename) != "R")
	filename <- sprintf("%s.R", filename)
	
	write(getTextviewText("log_textview"), filename)
	
	setStatusBar("The log has been exported to", filename)
}


sanitycheck.rain <- function(timeblobList) {
	if (is.data.frame(timeblobList)) { timeblobList <- list(timeblobList) }
	for (k in seq(along=timeblobList)) {
		cat("Sanity checking rainfall series", k, "(", names(timeblobList)[k], ") ...\n")
		rawdata <- timeblobList[[k]][,2]
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
		rawdata <- timeblobList[[k]][,2]
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
		myPath <- file.path("hydrosanity", "inst", "etc", filename)
	} else {
		myPath <- file.path(etc, filename)
	}
	if (!file.exists(myPath)) {
		stop("could not find file ", filename)
	} else {
		return(myPath)
	}
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



