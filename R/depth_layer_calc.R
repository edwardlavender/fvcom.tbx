#' @title Calculate the depths of WeStCOMS layers with known inputs
#' @description The depth of WeStCOMS layers can be computed from the depth of the seabed below mean sea level (in a given location), the tidal elevation (at a given time and in a given location) and a vector of constants which adjusts for the relative position of each layer. If these values are known, \code{\link[WeStCOMSExploreR]{depth_layer_calc}} can be implemented to calculate the depth of Sigma layers. If these values are unknown, the function \code{\link[WeStCOMSExploreR]{compute_field_depth}} should be used instead: this function first computes the appropriate values for specified times/locations and then uses these values to calculate depths.
#'
#' @param siglev A number or numeric vectors of siglev values.
#' @param h A number or numeric vectors of depth below mean sea level.
#' @param el A number of numeric vector of tidal elevation.
#'
#' @return The function returns a number/numeric vector of Sigma layer depths calculated from user inputs.
#'
#' @examples
#'
#' #### Examples
#' depth_layer_calc(dat_siglev$siglev[1:10], dat_h$h[1], el = 0)
#' depth_layer_calc(dat_siglev$siglev[10], dat_h$h[1:10], el = 0)
#' depth_layer_calc(dat_siglev$siglev[1:10], dat_h$h[1:10], el = 1:10)
#' # Be careful with how arguments are recyled: the function may need to be implemented using apply()
#' # ... in more complex cases, like here:
#' \dontrun{
#'  depth_layer_calc(dat_siglev$siglev[1:10], dat_h$h[1:3], el = 1:3)
#' }
#'
#' @author Edward Lavender
#' @export
#'

depth_layer_calc <-
  function(siglev, h, el){
    siglev * (h + el)
  }
