## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

updateMultivarPage <- function() {
	
	role <- sapply(hsp$data, attr, "role")
	
	flow_combo <- theWidget("multivar_flowblob_combobox")
	oldSel <- flow_combo$getActive()
	flow_combo$getModel()$clear()
	for (x in names(hsp$data)[role=="FLOW"]) {
		flow_combo$appendText(x)
	}
	if (oldSel == -1) { oldSel <- 0 }
	flow_combo$setActive(oldSel)
	
	setupIconView(theWidget("multivar_iconview"), 
		itemNames=names(hsp$data)[role=="RAIN"], selection="all")
	
	.hydrosanity$update$multivar <<- F
	theWidget(APPWIN)$present()
}


