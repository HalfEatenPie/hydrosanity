## Dataset tab ##
  * import from text files in arbitrary format, or in one of the pre-defined formats;
  * view and edit metadata (and raw data);
  * transform time series: temporal aggregation, and ratio of time series;
  * export time series.

## Scope tab ##
  * import from a database of rainfall stations (currently Bureau of Meteorology), with iterative refinement of space and time bounds;
  * timeline plot showing missing values and quality codes;
  * site map showing locations of sites, catchment boundary, coastlines, etc.

## Summary tab ##
  * text summary of missing values in dataset, and ranges of observed values.

## Basic plots tab ##
  * flexible time series plots, can include aggregated or smoothed data;
  * plot distributions of values compared between sites;
  * plot distributions of values compared between seasons.

## Impute tab ##
  * impute data values at one site based on values at other sites:
    * Correlation method: use sites with best correlation;
    * Distance method: inverse distance weighting;
    * Constant method: carry last observation forward, or use mean etc;
  * disaccumulate accumulated rainfall values, based on results of imputing;
  * plot predicted (imputed) vs actual data values;
  * plot distribution of imputed values when data is missing vs when data is not missing (check Missing At Random).

## Areal rainfall tab ##
  * plot time-series of multiple rain gauges on a map. You can step through the maps day-by-day to see how each storm evolves. This is useful for finding errors;
  * plot average rainfall map for the chosen time period;
  * plot average rainfall map compared between seasons;
  * plot average rainfall map for each year of record;
  * plot Thiessen polygons (aka "Voronoi mosaic");
  * estimate catchment areal rainfall by Thiessen polygon method;
  * plot a sequence of spatial grids (external files) with dates;
  * calculate average gridded value inside catchment for a sequence of grids: to estimate a coarse areal rainfall series;
  * estimate catchment areal rainfall by Thiessen polygon method, with "downscaling" from gridded areal rainfall series: the original data is scaled to be consistent with the spatial grids.

## Multivariate tab ##
  * plot annual time series of cross-correlation/lag between pairs of sites (usually multiple rain series compared to one stream flow series);
  * plot rainfall-runoff relationship, showing many rainfall series against one flow series.

## Bivariate tab ##
  * plot cross-correlation function between pairs of sites;
  * plot rainfall-runoff relationship, conditioned on time, season and/or antecedent flow.

## Log tab ##
  * keeps a record of all data modifications.