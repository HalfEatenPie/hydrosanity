Hydrosanity is an add-on package for [R](http://www.R-project.org/). It provides a graphical user interface for exploring hydrological time series. It is designed to work with catchment surface hydrology data (mainly rainfall and streamflow time series at a set of locations). There are functions to import from a database or files; summarise and visualise the dataset in various ways; estimate areal rainfall; fill gaps in rainfall data; and estimate the rainfall-runoff relationship. Probably the most useful features are the interactive graphical displays of a spatial set of time series.

**WARNING:** this package has not been actively developed for several years.
An introductory paper is included, but there is not much detailed documentation.

Hydrosanity was developed by Felix Andrews, and the Graphical User Interface was based on [Rattle](http://rattle.googlecode.com/) by Graham Williams.

### Installation ###
  1. Make sure you have the GTK+ libraries, version >= 2.10.11:
    * **Windows**: download and install the _Gtk+/Win32 Runtime Environment_ from [here](http://gladewin32.sourceforge.net/) (~6MB).
    * **MacOS**: a GTK+ installer is available [here](http://r.research.att.com/gtk2-runtime.dmg) (~19MB).
    * **GNU/Linux**: GTK+ is usually included by default on these systems.
  1. Install R. Hydrosanity requires R version 2.5.0 or later. Here is a link to the [latest stable Windows release](http://cran.r-project.org/bin/windows/base/).
  1. Install the packages in R:
    * `install.packages(c("RGtk2", "playwith", "reshape", "latticeExtra", "sp", "rgdal", "tripack", "gpclib"))`
    * `install.packages(c("maptools", "akima", "maps"))` if you want some extra spatial functions.
    * `install.packages("mapdata")` (a 24MB download!) if you want higher resolution world coastlines and rivers on maps.
    * download the [windows binary package](http://hydrosanity.googlecode.com/files/hydrosanity_0.8-99.zip) or the [source package](http://hydrosanity.googlecode.com/files/hydrosanity_0.8-99.tar.gz) (e.g. for linux) and install it into your R library.
  1. `library(hydrosanity)` to load the package, and then `hydrosanity()` to start the graphical user interface.

### Troubleshooting ###
  * Try updating packages with `update.packages()` or the corresponding menu item in your R GUI.
  * If you experience crashes, try updating to the latest GTK+ libraries (2.10.x).
  * Things often don't get refreshed until you move the mouse around, so the program may appear to have stopped. This is a known limitation of RGtk2. Also, the main RGui sometimes flips to the front, hiding the Hydrosanity window. To avoid that, just minimise RGui.