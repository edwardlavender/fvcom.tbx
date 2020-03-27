#####################################
#####################################
#### hour_dbl()

#' @title Compute the time of day, in hours, as a double
#' @description Compute the time of day, in hours, from a \code{\link[base]{DateTimeClasses}} object, accounting for the hour of day, the number of minutes passed the hour and the number of seconds passed the minute.
#'
#' @param time A vector of times, of the class \code{\link[base]{DateTimeClasses}}.
#'
#' @return The function returns a double or vector of doubles which defines/define the time of day, in hours.
#'
#' @examples
#'
#' hour_dbl(as.POSIXct("2016-01-01 13:01:40"))
#'
#' @author Edward Lavender
#' @export
#'

hour_dbl <- function(time){
  lubridate::hour(time) + lubridate::minute(time)/60 + lubridate::second(time)/3600
}


#####################################
#####################################
#### hour_nearest()

#' @title Compute the nearest integer hour of day at supplied times
#' @description Compute the the nearest integer hour of day from a \code{\link[base]{DateTimeClasses}} object (i.e., nearest neighbour interpolation).
#'
#' @param time A vector of times, of the class \code{\link[base]{DateTimeClasses}}.
#'
#' @return The function returns an integer for each inputted time.
#'
#' @examples
#'
#' hour_nearest(as.POSIXct("2016-01-01 13:01:40"))
#' hour_nearest(as.POSIXct(c("2016-01-01 13:01:40", NA)))
#'
#' @author Edward Lavender
#' @export
#'

hour_nearest <- function(time){
  round.POSIXt(time, units = "hours")
}

