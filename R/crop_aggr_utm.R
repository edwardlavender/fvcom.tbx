#' @title Crop, aggregate and reproject rasters for 3d visualisation
#' @description The 3d, interactive visualisation of large rasters (e.g. with \code{\link[WeStCOMSExploreR]{plot3dscape}}) can be constrained by memory requirements. Therefore, often, it may be preferable to crop a raster to a smaller area and aggregate the raster (if necessary) to reduce raster size prior to plotting. For meaningful landscape visualisations, it is also helpful to convert rasters, if required, to the Universal Transverse Mercator system, so that x, y and z coordinates are all on the same scale. This function is a wrapper function for \code{\link[raster]{crop}}, \code{\link[raster]{aggregate}} and \code{\link[raster]{projectRaster}} which implements these operations. This facilitates the visualisation of large raster files in chunks via \code{\link[WeStCOMSExploreR]{plot3dscape}}.
#'
#' @param r The \code{\link[raster]{raster}} to be processed.
#' @param x1 A number which specifies the minimum x value for the raster. The raster will be cropped by this value.
#' @param x2 A number which specifies the maximum y value for the raster. The raster will be cropped by this value.
#' @param y1 A number which specifies the minimum x value for the raster. The raster will be cropped by this value.
#' @param y2 A number which specifies the maximum y value for the raster. The raster will be cropped by this value.
#' @param aggregate (optional) A named list of arguments that is passed to \code{\link[raster]{aggregate}} to aggregate the cropped raster.
#' @param proj_utm A Universal Transverse Mercator projection string of class \code{\link[sp]{CRS-class}}CRS-class. Zone 29 is the default.
#' @param verbose A logical input which defines whether or not to print messages to the console regarding function progress. This can be helpful because cropping, aggregating and re-projecting large rasters can take some time.
#'
#' @examples
#'
#' #### Example (1) Crop dat_gebco raster to defined area, aggregate and ensure UTM coordinates for
#' # ... sensible visualisation on the scale of the data:
#' proj_utm <-
#'   sp::CRS("+proj=utm +zone=29 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
#' dat_gebco_processed <-
#' crop_aggr_utm(dat_gebco, -5.5, -5.4, 56.36, 56.44,
#'               aggregate = list(fact = 2, fun = max),
#'               proj_utm = proj_utm)
#' plot3dscape(dat_gebco_processed, aspectmode = "data", stretch = 10)
#'
#' @author Edward Lavender
#' @export
#'

###################################
###################################
#### crop_aggr_utm()

crop_aggr_utm <-
  function(r, x1, x2, y1, y2,
    aggregate = NULL,
    proj_utm =
      sp::CRS(paste("+proj=utm +zone=29 +datum=WGS84",
                    "+units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")),
    verbose = TRUE){

  #### Crop raster to requested extent:
  if(verbose) cat("Defining raster extent...\n")
  eras <- raster::extent(r)
  if(x1 < eras[1]) x1 <- eras[1]
  if(x2 > eras[2]) x2 <- eras[2]
  if(y1 < eras[3]) y1 <- eras[3]
  if(y2 > eras[4]) y2 <- eras[4]
  if(verbose) cat("Cropping raster...\n")
  ecrop <- raster::extent(x1, x2, y1, y2)
  r <- raster::crop(r, ecrop)

  #### Aggregate raster using user-supplied list, if supplied:
  if(!is.null(aggregate)) {
    if(verbose) cat("Aggregating raster...\n")
    aggregate$x <- r
    r <- do.call(raster::aggregate, aggregate)
  }

  #### Re-project raster, if the CRS is not UTM
  if(raster::crs(r)@projargs != proj_utm@projargs){
    if(verbose) cat("Reprojecting raster to UTM...\n")
    r <- raster::projectRaster(r, crs = proj_utm)
  }

  #### Print raster dimensions and return raster
  if(verbose){
    cat("Printing raster dimensions...\n")
    cat(paste(paste(dim(r), collapse = ", "), "\n"))
    cat("Returning raster...\n")
  }
  return(r)
}

#### End of code.
###################################
###################################
