#' @title quiver
#' @description Add a vector field to a plot
#'
#' @param x A numeric value specifying the x coordinate(s) of the start point of the arrow(s).
#' @param y A numeric value specifying the x coordinate(s) of the start point of the arrow(s).
#' @param u The u (easting) component vector.
#' @param v The v (northing) component vector.
#' @param scale A numeric value which scales the length of the arrow shaft.
#' @param length A numeric value which adjusts the length of the arrow head.
#' @param angle A numeric value which defines the angle (degrees) between the two sides of the arrow head.
#' @param ... Other graphical parameters that can be passed to \code{\link[graphics]{arrows}}.
#'
#' @source This function is copied from pracma::quiver but corrects a small bug in the function which stops the angle of arrow heads being adjustable.
#'
#' @note This function is not exported.
#' @keywords internal



################################################
#### quiver

# This function is copied from pracma::quiver but I've adjusted angle = 10
# ... to angle = angle in the body of the function.

quiver <-
  function (x, y, u, v, scale = 0.05, angle = 10, length = 0.1,
            ...)
  {
    stopifnot(is.numeric(x), is.numeric(y), is.numeric(u), is.numeric(v))
    graphics::arrows(x, y, x + scale * u, y + scale * v, angle = angle, length = length,
           ...)
  }


#### End of code.
################################################
################################################
