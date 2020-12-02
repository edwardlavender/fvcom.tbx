##############################################
##############################################
#### interp_layer()

#' @title Interpolate a fractional layer number for an observation between Sigma layers
#' @description This function linearly interpolates the layer number for an observation of a specified depth, depending on the depths of the nearest layers (see Examples). This approach is used in \code{\link[fvcom.tbx]{depth_from_unknown}} to define fractional layer numbers.
#'
#' @param x A number which specifies the depth of the observation.
#' @param l1 A number which specifies the layer ID of the nearest layer.
#' @param l2 A number which specifies the layer ID of the other surrounding layer.
#' @param b1 A number which specifies the depth of the first layer.
#' @param b2 A number which specifies the depth of the second layer.
#'
#' @return The function returns a (fractional) layer number for an observation that lies between two layers.
#'
#' @examples
#' #### Example (1): Interpolate fractional layer numbers
#' # Imagine we have made an observation at 22 m deep. To compare this observation to
#' # ... FVCOM predictions we might want to define the layer number of that observation.
#' # Imagine we know that the observation lies between layers 2 and 3, which are 21 and 33 m deep
#' # ... respectively. We can interpolate the layer number of our observation between layers 2 and
#' # ... 3:
#' interp_layer(22, 2, 3, 21, 32)
#'
#' @author Edward Lavender
#' @export
#'

interp_layer <- function(x, l1, l2, b1, b2){
  stopifnot(x >= b1 & x <= b2)
  if(x == b1) return(b1)
  if(x == b2) return(b2)
  d1 <- abs(x - b1)
  d2 <- abs(x - b2)
  delta <- abs(b1 - b2)
  y <- l1*((delta-d1)/delta) + l2*((delta-d2)/delta)
  return(y)
}


##############################################
##############################################
#### interp_btw_hours()

#' @title Interpolate an FVCOM prediction between hours
#' @description This function linearly interpolates an FVCOM prediction between hours.
#'
#' @param x A number which specifies the fractional hour for which a prediction is desired.
#' @param h1 An integer which specifies the hour of the first FVCOM prediction.
#' @param h2 An integer which specifies the hour of the surrounding FVCOM prediction.
#' @param p1 A number which specifies the FVCOM prediction at \code{h1}.
#' @param p2 A number which specifies the FVCOM prediction at \code{h2}.
#'
#' @return The function returns an interpolated FVCOM prediction for a time between two hours at which FVCOM predictions are available.
#'
#' @examples
#' #### Example (1): Interpolate environmental conditions between hours
#' # Imagine we want to know the predicted temperature at 10:10 am on a given day.
#' # FVCOM outputs the predictions on every hour, so we have predictions for
#' # ... 10 am and 11 am of 12 and 13 dg C respectively. Then, the interpolated
#' # ... temperature at 10:10 am is
#' interp_btw_hours(10+10/60, 10, 11, 12, 13)
#'
#' @author Edward Lavender
#' @export
#'

interp_btw_hours <- function(x, h1, h2, p1, p2){
  stopifnot(x >= h1 & x <= h2)
  if(x == h1) return(p1)
  if(x == h2) return(p2)
  d1 <- abs(x - h1)
  d2 <- abs(x - h2)
  delta <- abs(h2 - h1)
  w1 <- (delta - d1)/delta
  w2 <- (delta - d2)/delta
  y <- p1*w1 + p2*w2
  return(y)
}


##############################################
##############################################
#### interp_btw_depths()

#' @title Interpolate an FVCOM prediction between depths
#' @description This function linearly interpolates an FVCOM prediction between the depths of two layers.
#'
#' @param x A number which specifies the depth for which a prediction is desired.
#' @param d1 An integer which specifies the depth of the first FVCOM prediction.
#' @param d2 An integer which specifies the depth of the second FVCOM prediction.
#' @param p1 A number which specifies the FVCOM prediction at \code{d1}.
#' @param p2 A number which specifies the FVCOM prediction at \code{d2}.
#'
#' @details This function implements linear interpolation between the depths of layers, rather than layer numbers, because the spacing between layers is non linear.
#'
#' @return The function returns an interpolated FVCOM prediction for a depth between two depths (the depths of two layers at which FVCOM predictions are available).
#'
#' @examples
#' #### Example (1): Interpolate environmental conditions between depths
#' # Imagine we want to know the predicted temperature at depth 25 m on a given day.
#' # For nodes, FVCOM outputs the predictions for each layer. Imagine that the nearest
#' # ... predictions are for layers 2 and 3 which have depths 20 and 30 m respectively.
#' # Imagine these predictions are 8 and 9 dg C. Then the interpolated temperature at
#' # ... depth 25 m is:
#' interp_btw_depths(25, 20, 30, 8, 9)
#'
#' @author Edward Lavender
#' @export
#'

interp_btw_depths <- function(x, d1, d2, p1, p2){
  stopifnot(x >= d1 & x <= d2)
  if(x == d1) return(p1)
  if(x == d2) return(p2)
  delta <- abs(d1 - d2)
  n1 <- abs(delta - abs(d1 - x))
  n2 <- abs(delta - abs(d2 - x))
  y <- p1*(n1/delta) + p2*(n2/delta)
  return(y)
}


#### End of code.
##############################################
##############################################
