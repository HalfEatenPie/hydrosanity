## Hydrosanity: an interface for exploring hydrological time series in R
##
## These functions were copied from Rattle v2.1
## Copyright (c) 2006 Graham Williams, Togaware.com, GPL Version 2
## Graham.Williams@togaware.com
##
## (with minor adjustments for Hydrosanity)


theWidget <- function(widget)
{
  return(.hydrosanity$GUI$getWidget(widget))
}

## Log support

addLogSeparator <- function() {
	addToLog("\n",
		"## ", paste(rep("=", 60), collapse=""), "\n",
		"## Timestamp: ", format(Sys.time()))
	#date()
}

addLogComment <- function(...) {
	addToLog("\n## ", ...)
}

addToLog <- function(..., sep="", end.with="\n", localise_assignment=T) {
	msg <- paste(sep=sep, ..., end.with)
	if (length(msg) == 0) { msg <- "" }
	if (localise_assignment) { msg <- gsub("<<-", "<-", msg) }

	## Always place text at the end, irrespective of where the cursor is.
	
	log.buf <- theWidget("log_textview")$getBuffer()
	location <- log.buf$getEndIter()$iter
	log.buf$insert(location, msg)
}


## Textview widget support

setTextview <- function(tv, ..., sep="")
{
  msg <- paste(sep=sep, ...)
  if (length(msg) == 0) msg <-""
  theWidget(tv)$getBuffer()$setText(msg)
}

addTextview <- function(tv, ..., sep="")
{
  msg <- paste(sep=sep, ...)
  if (length(msg) == 0) msg <-""
  tv.buf <- theWidget(tv)$getBuffer()
  loc <- tv.buf$getEndIter()$iter
  tv.buf$insert(loc, msg)
}

setTextviewMonospace <- function(tv)
{
  theWidget(tv)$modifyFont(pangoFontDescriptionFromString("monospace 10"))
}

getTextviewText <- function(TV)
{
  ## Extract text content of specified textview
  
  log.buf <- theWidget(TV)$getBuffer()
  start <- log.buf$getStartIter()$iter
  end <- log.buf$getEndIter()$iter
  return(log.buf$getText(start, end))
}


## Misc

setStatusBar <- function(..., sep=" ")
{
  msg <- paste(sep=sep, ...)
  if (length(msg) == 0) msg <-""
  theWidget("statusbar")$push(1, msg)
  while (gtkEventsPending()) gtkMainIteration() # Refresh status and windows
  invisible(NULL)
}

setCursor <- function(cursor=NULL) {
	if (!is.null(cursor)) { cursor <- gdkCursorNew(cursor) }
	theWidget("hs_window")$getWindow()$setCursor(cursor)
}

set.cursor <- function(cursor="left-ptr")
{
  theWidget("hs_window")$getWindow()$setCursor(gdkCursorNew(cursor))
}

get.extension <- function(path)
{
  ## Extract and return the extension part of a filename
  
  parts <- strsplit(path, "\\.")[[1]]
  if (length(parts) > 1)
    last <- parts[length(parts)]
  else
    last <- ""
  last
}

get.stem <- function(path)
{
  parts <- strsplit(basename(path), "\\.")[[1]]
  last <- paste(parts[1:length(parts)-1], collapse=".")
  last
}


## UTILITIES

"%notin%" <- function(x,y) ! x %in% y

not.null <- function(x) ! is.null(x)


