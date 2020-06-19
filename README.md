
<!-- README.md is generated from README.Rmd. Please edit that file -->

# WeStCOMSExploreR

<!-- badges: start -->

<!-- badges: end -->

`WeStCOMSExploreR` is an R package which provides tools for the
exploration of unstructured, prism-based hydrodynamic model outputs
(i.e. from the Finite Coastal Ocean Volume Model, FVCOM) in `R`. The
package has been designed specifically for the West Coast of Scotland
Coastal Modelling System (WeStCOMS), which implements FVCOM. Package
development has been motivated by the requirements of ecological
research, and the need to link hydrodynamic model outputs with
ecological analyses implemented in `R`. To this end, the package
includes functions which facilitate the following operations:

  - Processing WeStCOMS outputs, including the definition of new fields;
  - Building the WeStCOMS unstructured mesh(es) as spatial objects;
  - Working with WeStCOMS outputs, including extracting predictions,
    locating cells and coordinates, computing layer depths and
    interpolating predictions;
  - Exploring environmental conditions through space and time, with
    statistical summaries and maps;
  - Validating WeStCOMS predictions with observations, including from
    diverse animal movement datasets;

## Installation

You can install the released version of WeStCOMSExploreR from
[CRAN](https://CRAN.R-project.org) with:

``` r
#### install from CRAN
install.packages("WeStCOMSExploreR")
```

``` r
#### Or, install the development version from github
devtools::install_github("edwardlavender/WeStCOMSExploreR")
```

## Prerequisites

WeStCOMS files are MATLAB® files. To use these in R, some pre-processing
outside of R is required. This is described in the vignette.

## Processing WeStCOMS outputs

Some functions are designed to facilitate processing WeStCOMS outputs.
These include:

  - `date_name()` - flick between dates and WeStCOMS file names;
  - `create_wcdirs()` - create folders in which to store WeStCOMS
    outputs;
  - `define_dates2load()` - define a sequence of dates for which to load
    WeStCOMS files, accounting for corrupt files;
  - `compute_new2dfield()` - compute new 2 dimensional hydrodynamic
    fields from WeStCOMS outputs (namely, thermocline strength, wind
    speed, wind direction, current speed, current direction and sun
    angle);
  - `compute_sun_angle_field()` - compute sun angle as a WeStCOMS field;
  - `WeStCOMSarray2df()` - convert a WeStCOMS array to a dataframe for
    plotting (see later);

## Unstructured mesh(es)

`build_mesh()` builds an unstructured mesh (around nodes or elements)
from node coordinates and connections as a `SpatialPolygonsDataFrame` in
R.

## Work with WeStCOMS files

Some functions are designed to facilitate working with WeStCOMS files.
These include the following:

  - `find_cells()` - find the mesh cells (for nodes or elements) which
    enclose inputted coordinates;
  - `find_xy()` - find the coordinates of mesh cells (for nodes or
    elements);
  - `exclude_corrupt()` and `exclude_unavailable()` exclude corrupt and
    unavailable files from vectors of WeStCOMS file names
  - `extract()` - extract WeStCOMS predictions for multiple
    dates/hours/layers/mesh cells;
  - `calc_layer_depth()` - calculate the depths of Sigma layers when
    parameters are known;
  - `compute_layer_depth()` - compute the depths of Sigma layers,
    extracting parameters from WeStCOMS outputs as necessary;
  - `interp_layer()`, `interp_btw_hours()` and `interp_btw_depths()` -
    interpolate fractional layer numbers and predictions between hours
    or layers;

## Explore environmental conditions

Some functions are designed to facilitate exploration of environmental
conditions through space and/or time. These include the following:

  - `summarise2dfield()` - compute statistical summaries of
    environmental conditions across a WeStCOMS layer (through time for a
    given WeStCOMS file, if applicable);
  - `plot2dfield()` - visualise environmental conditions across a
    WeStCOMS layer through space at a specified point in time;
  - `explore()` - implement `summarise2dfield()` and `plot2dfield()`
    across multiple timepoints and/or WeStCOMS files;
  - `plot3dscape()` and `vis3dscape()` produce interactive,
    3-dimensional visualisations of landscapes/seascapes and/or
    environmental conditions; for large rasters, `crop_aggr_utm()` helps
    reduce raster dimensions for these functions;

## Model validation

`validate()` facilitates the comparison of observed conditions
(including those from animal movement datasets) with predicted
conditions to evaluate WeStCOMS skill.

## Future functionality

Future plans for `WeStCOMSExploreR` include developing the following
functionality:

  - Exploring temperature profiles through space and time;
  - Exploring spatiotemporal variation in environmental conditions in
    3d;
  - Defining bottom velocity from vertical profiles;
  - Improved flexibility to define new environmental fields;
