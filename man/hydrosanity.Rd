\name{hydrosanity}
\alias{hydrosanity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hydrosanity()
}
%- maybe also 'usage' for other objects documented here.
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
function() {
	require(RGtk2, quietly=TRUE) # From http://www.ggobi.org/rgtk2/
	require(cairoDevice, quietly=TRUE)
	
	hsp <<- blankStateHSP()
	.hydrosanity <<- list()
	.hydrosanity$dev <<- list()
	.hydrosanity$win <<- list()
	
	## Try firstly to load the glade file from the installed hydrosanity
	## package, if it exists. Otherwise, look locally.
	result <- try(etc <- file.path(.path.package(package="hydrosanity")[1], "etc"), silent=TRUE)
	if (inherits(result, "try-error"))
		.hydrosanity$GUI <<- gladeXMLNew("hydrosanity.glade",root="hs_window")
	else
		.hydrosanity$GUI <<- gladeXMLNew(file.path(etc,"hydrosanity.glade"),root="hs_window")
	
	## Now connect the callbacks
	gladeXMLSignalAutoconnect(.hydrosanity$GUI)
	gSignalConnect(theWidget("hs_window"), "delete-event", on_menu_quit_activate)
	
	myTreeView <- theWidget("import_summary_treeview")
	insertTreeViewTextColumns(myTreeView, 
		colNames=c("Name", "Start", "End", "Length", "Avg_Freq", "Data", "Role", "Qual", "Extra_columns"),
		editors=list(Name=on_import_summary_treeview_name_edited,
			Role=on_import_summary_treeview_role_edited),
		combo=list(Role=data.frame(c("RAIN","FLOW","OTHER"))) )
	myTreeView$getSelection()$setMode("multiple")
	#myTreeView$setHeadersClickable(TRUE)
	
	hsp$defaultImportOptions <- theWidget("import_options_entry")$getText()
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
	
	setTextviewMonospace("log_textview")
	addInitialLogMessage()
	
	while (gtkEventsPending()) gtkMainIterationDo(blocking=F) # redraw
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
