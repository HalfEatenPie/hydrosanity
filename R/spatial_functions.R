## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

#panel.layers <- function(x, y, ..., layers=list()) {
#	lapply(layers, eval, list(...))
#}

panel.layers <- function(x, y, ..., layers=list()) {
	for (i in seq_along(layers)) {
		item <- layers[[i]]
		itemName <- names(layers)[i]
		if (is.null(itemName)) itemName <- F # dummy value
		# check if this item is turned off
		if (any(grep("\\.off$", itemName))) next
		if (!is.call(item) && !is.expression(item)) {
			warning("item ", i, " is ", mode(item), ", ignoring")
			next
		}
		eval(item, list(...))
	}
}

panel.levelplot.mosaic <- function(x, y, z, subscripts=T,
	at=seq(min(z, na.rm=T), max(z, na.rm=T), length=100),
	col.regions=regions$col, boundary=NULL) {
	# draw Voronoi mosaic
	#require(grid)
	#stopifnot(require(tripack))
	xlim <- convertX(unit(0:1,"npc"), "native", valueOnly=T)
	ylim <- convertY(unit(0:1,"npc"), "native", valueOnly=T)
	# only interpolate with sites in twice visible range
	xlim.use <- xlim + diff(xlim) * c(-0.5, 0.5)
	ylim.use <- ylim + diff(ylim) * c(-0.5, 0.5)
	# find subset of points to use
	ok <- ((min(xlim.use) < x) & (x < max(xlim.use)) &
		(min(ylim.use) < y) & (y < max(ylim.use)))
	ok <- ok & (subscripts %in% seq_along(x))
	ok <- ok & complete.cases(x, y, z)
	if (sum(ok) == 0) return() # warn?
	x <- x[ok]
	y <- y[ok]
	z <- z[ok]
	xy <- xy.coords(x, y)
	xy <- data.frame(x=xy$x, y=xy$y)


	subPolys <- arealSubPolygons(xy, boundary=boundary, min.area.pct=0)
	# draw it as one composite polygon
	coords_list <- lapply(subPolys@polygons, function(pp)
		lapply(pp@Polygons, coordinates))
	composite_n <- sapply(coords_list, length)
	poly_n <- rapply(coords_list, nrow)
	do.rbind <- function(x) do.call("rbind", x)
	polydata <- do.rbind( lapply(coords_list, do.rbind) )

	regions <- trellis.par.get("regions")
	zcol <- level.colors(z, at, col.regions, colors = TRUE)
	poly_col <- rep(zcol, times=composite_n)
	grid.polygon(polydata[,1], polydata[,2], id.lengths=poly_n,
		default.units="native", gp=gpar(fill=poly_col, col="transparent"))
	return()

	# add dummy points to ensure that voronoi polygons are finite
	dummies <- data.frame(x=c(-1,-1,1,1), y=c(-1,1,-1,1)) * 10 * max(abs(xy))
	xy <- rbind(xy, dummies)
	# calculate voronoi mosaic
	vpolys <- voronoi.polygons(voronoi.mosaic(xy, duplicate="remove"))
	# draw it as one composite polygon
	polydata <- do.call('rbind', vpolys)
	regions <- trellis.par.get("regions")
	zcol <- level.colors(z, at, col.regions, colors = TRUE)
	grid.polygon(polydata[,1], polydata[,2], id.lengths=sapply(vpolys, nrow),
		default.units="native", gp=gpar(fill=zcol, col="transparent"))
}

panel.contourplot.interp <- function(..., contour=T, region=F) {
	panel.levelplot.interp(..., contour=contour, region=region)
}

panel.levelplot.interp <- function(x, y, z, subscripts=T, xo.length=40, yo.length=xo.length,
	linear=T, extrap=F, contour=F, region=T, at, ...) {
	# draw interpolated grid
	#require(grid)
    stopifnot(require(akima))
	xlim <- convertX(unit(0:1,"npc"), "native", valueOnly=T)
	ylim <- convertY(unit(0:1,"npc"), "native", valueOnly=T)
	# only interpolate with sites in twice visible range
	xlim.use <- xlim + diff(xlim) * c(-0.5, 0.5)
	ylim.use <- ylim + diff(ylim) * c(-0.5, 0.5)
	# find subset of points to use
	ok <- ((min(xlim.use) < x) & (x < max(xlim.use)) &
		(min(ylim.use) < y) & (y < max(ylim.use)))
	ok <- ok & (subscripts %in% seq_along(x))
	ok <- ok & complete.cases(x, y, z)
	if (sum(ok) < 4) {
		warning("at least 4 locations are required for interpolation")
		return()
	}
	# construct marginal dimensions of grid
	xo <- seq(min(xlim), max(xlim), length=xo.length)
	yo <- seq(min(ylim), max(ylim), length=yo.length)
	tmp.grid <- expand.grid(x=xo, y=yo)
	# compute the spatial field (interpolation)
	tmp.grid$z <- as.vector(
		interp(x=x[ok], y=y[ok], z=z[ok],
		xo=xo, yo=yo, linear=linear,
		extrap=extrap, duplicate="mean")$z)
	# restrict surface within observed limits (for spline silliness)
	if (!linear) {
		tmp.grid$z <- pmax(min(z[ok]), pmin(max(z[ok]), tmp.grid$z))
	}
	myAt <- if (contour) pretty(z) else
		seq(min(z, na.rm=T), max(z, na.rm=T), length=100)
	if (diff(range(myAt)) == 0) myAt <- c(myAt[1], myAt[1] + 1)
	if (missing(at)) at <- myAt
	with(tmp.grid,
		panel.levelplot(x, y, z, subscripts=subscripts,
			contour=contour, region=region, at=at, ...))
}

panel.worldmap <- function(col="black", ...) {
	# draw map lines for national borders and coastlines
	#require(grid)
	xlim <- convertX(unit(0:1,"npc"), "native", valueOnly=T)
	ylim <- convertY(unit(0:1,"npc"), "native", valueOnly=T)
	if (require(mapdata, quietly=T)) {
		try({
			mapdb <- map("worldHires", plot=F, xlim=xlim, ylim=ylim)
			panel.lines(mapdb$x, mapdb$y, col=col, ...)
		}, silent=T)
	} else
	# Australia: 'oz' package has better resolution than 'maps'
	if ((113 <= max(xlim)) && (min(xlim) <= 154) &&
		(-44 <= max(ylim)) && (min(ylim) <= -10) &&
		require(oz, quietly=T)) {
		for (i in ozRegion(xlim=xlim, ylim=ylim)$lines) {
			panel.lines(i$x, i$y, col=col, ...)
		}
	} else
	if (require(maps)) {
		try({
			mapdb <- map("world", plot=F, xlim=xlim, ylim=ylim)
			panel.lines(mapdb$x, mapdb$y, col=col, ...)
		}, silent=T)
	}
}

panel.rivers <- function(col="blue", lty="longdash", ...) {
	# draw lines for major rivers
	#require(grid)
	xlim <- convertX(unit(0:1,"npc"), "native", valueOnly=T)
	ylim <- convertY(unit(0:1,"npc"), "native", valueOnly=T)
	if (require(maps, quietly=T)) {
		try({
			mapdb <- map("rivers", plot=F, xlim=xlim, ylim=ylim)
			panel.lines(mapdb$x, mapdb$y, col=col, lty=lty, ...)
		}, silent=T)
	}
}

panel.cities <- function(pch=15, col="black", ...) {
	# draw cities
	#require(grid)
	stopifnot(require(maps))
	xlim <- convertX(unit(0:1,"npc"), "native", valueOnly=T)
	ylim <- convertY(unit(0:1,"npc"), "native", valueOnly=T)
	data(world.cities)
	with(world.cities, {
		ok <- ((min(xlim) < long) & (long < max(xlim)) &
			(min(ylim) < lat) & (lat < max(ylim)))
		if (any(ok)) {
			if (sum(ok) < 25) panel.points(long[ok], lat[ok],
				pch=pch, col=col, ...)
			ux <- unit(long[ok], "native")
			uy <- unit(lat[ok], "native")
			grid.text(name[ok], ux, uy + unit(0.5, "char"),
				just="top", check.overlap=T,
				gp=gpar(col=col, ...))
			#foo <- grid.grabExpr(panel.text(long[ok], lat[ok],
			#	name[ok], pos=1, col=col, ...))
			#grid.draw(editGrob(foo, ".", grep=T, check.overlap=T))
		}
	})
}

# extend default limits by 10% on each side
prepanel.extend.10 <- function(...) {
	tmp <- lattice:::prepanel.default.xyplot(...)
	if (is.numeric(tmp$xlim)) {
		tmp$xlim <- extendrange(r=tmp$xlim, f=0.1)
	}
	if (is.numeric(tmp$ylim)) {
		tmp$ylim <- extendrange(r=tmp$ylim, f=0.1)
	}
	tmp
}

# ColorBrewer 9-class sequential Blues
Blues <- c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6",  "#2171B5", "#08519C", "#08306B")

sqrtSeq <- function(from, to, length.out)
	seq(sqrt(from), sqrt(to), length=length.out)^2

sqrtPalette <- function(cols=Blues, n=100)
	colorRampPalette(cols, bias=2)(n)

sqrtPalette1 <- function() hsv(h=sqrt(seq(1/6^2, 1, length=100)))

sqrtPalette2 <- function(x=rainbow(100, start=1/6))
	colorRampPalette(x, bias=2)(length(x))

sqrtPretty <- function(x, ...) pretty(sqrt(x), ...)^2

# assumes full 2D grid; 'xlim' and 'ylim' refer to dimensions 1 and 2
subGrid <- function(grid, xlim=NULL, ylim=NULL, inclusive=F) {
	stopifnot(fullgrid(grid))
	coords <- coordinatevalues(getGridTopology(grid))
	lim <- list(xlim, ylim)
	win <- list()
	for (i in 1:2) {
		if (is.null(lim[[i]])) {
			# this dimension not specified; select all values
			win[[i]] <- seq_along(coords[[i]])
			next
		}
		# coords need to be sorted increasing
		flipped <- is.unsorted(coords[[i]])
		if (flipped) coords[[i]] <- rev(coords[[i]])
		# find indices corresponding to given dimension limits
		lim.idx <- findIntervalRange(min(lim[[i]]), max(lim[[i]]),
			coords[[i]], inclusive=inclusive)
		# SpatialGrid can not handle a zero length dimension
		if (all(lim.idx == 0)) stop("dimension ", i, " is empty")
		lim.idx <- pmax(1, lim.idx)
		if (flipped) lim.idx <- length(coords[[i]]) - rev(lim.idx) + 1
		# fill in index sequence
		win[[i]] <- seq(lim.idx[1], lim.idx[2])
	}
	# extract values (dimensions in reverse order?)
	grid[win[[2]], win[[1]]]
}

notInterp <- function(x, y, z, xo=seq(min(x), max(x), length = 40),
	yo=seq(min(y), max(y), length = 40)) {
	#require(tripack)
	xy <- xy.coords(x, y)
	stopifnot(length(z) == length(xy$x))
	xy <- data.frame(x=xy$x, y=xy$y)
	# add dummy points to ensure that voronoi polygons are finite
	dummies <- data.frame(x=c(-1,-1,1,1), y=c(-1,1,-1,1)) * 10 * max(abs(xy))
	xy <- rbind(xy, dummies)
	# calculate voronoi mosaic
	vpolys <- voronoi.polygons(voronoi.mosaic(xy))
	sps <- as.SpatialPolygons.voronoi.polygons(vpolys, IDs=seq_along(z))
	# find polygon (hence z value) for each point in xo * yo grid
	hits <- overlay(SpatialPoints(expand.grid(x=xo,y=yo)), sps)
	zz <- matrix(z[hits], nrow=length(xo), ncol=length(yo))
	list(x=xo, y=yo, z=zz)
}

arealSubPolygons <- function(x, y=NULL, IDs=NULL, boundary=NULL, min.area.pct=0.5) {
	#stopifnot(require(tripack))
	#stopifnot(require(gpclib))
	xy <- xy.coords(x, y)
	if (is.null(IDs)) {
		IDs <- labels(x)
		if (is.list(IDs)) IDs <- IDs[[1]]
	}
	stopifnot(length(IDs) == length(xy$x))
	xy <- data.frame(x=xy$x, y=xy$y)
	# add dummy points to ensure that voronoi polygons are finite
	dummies <- data.frame(x=c(-1,-1,1,1), y=c(-1,1,-1,1)) * 10 * max(abs(xy))
	xy <- rbind(xy, dummies)
	# calculate voronoi mosaic
	vpolys <- voronoi.polygons(voronoi.mosaic(xy, duplicate="error"))
	subpolys <- lapply(vpolys, as, "gpc.poly")
	if (!is.null(boundary)) {
		boundary <- as(boundary, "gpc.poly")
		# clip it
		totalArea <- area.poly(boundary)
		stopifnot(totalArea > 0)
		subpolys <- lapply(subpolys, gpclib::intersect, boundary)
		# remove any sites with less than min.area.frac fraction of the area
		if (min.area.pct > 0) {
			min.area.frac <- min.area.pct / 100
			ok <- rep(TRUE, length(IDs))
			for (i in seq(along=IDs)) {
				if (area.poly(subpolys[[i]]) / totalArea < min.area.frac)
					ok[i] <- FALSE
			}
			# remove too-small entries and call recursively
			if (any(!ok)) {
				ok <- c(ok, rep(FALSE, 4)) # exclude dummy points
				return(arealSubPolygons(xy$x[ok], xy$y[ok], IDs[ok],
					boundary=boundary, min.area.pct=min.area.pct))
			}
		}
	}
	# convert back to SpatialPolygons
	thisSPs <- list()
	for (i in seq(along=IDs)) {
		if (length(get.pts(subpolys[[i]])) == 0) next
		ii <- length(thisSPs) + 1
		thisSPs[[ii]] <- as.Polygons.gpc.poly(subpolys[[i]], IDs[i])
	}
	SpatialPolygons(thisSPs)
}

as.Polygons.gpc.poly <- function(x, ID) {
	thisPolys <- lapply(get.pts(x), function(p) {
		Polygon(rbind(cbind(p$x,p$y),c(p$x[1],p$y[1])), hole=p$hole)
	})
	Polygons(thisPolys, ID)
}

as.SpatialPolygons.voronoi.polygons <- function(x, IDs) {
	thisSP <- list()
	for (i in seq_along(x)) {
		p <- x[[i]]
		tmpPoly <- Polygon(rbind(p, p[1,]), hole=F)
		thisSP[[i]] <- Polygons(list(tmpPoly), IDs[i])
	}
	SpatialPolygons(thisSP)
}

readGDAL_fixed <- function(fname, ...) {
	if (get.extension(tolower(fname)) == "flt") {
		foo <- readGDAL(fname, ...)
		foo.dim <- gridparameters(foo)$cells.dim
		binDat <- readBin(fname, "double", n=prod(foo.dim), size=4)
		binDat[binDat == -9999] <- NA
		foo@data[[1]] <- binDat
		return(foo)
	}
	readGDAL(fname, ...)
}


