#' @title Calculate thermocline strength
#' @description This function calculates thermocline strength from FVCOM temperature predictions.
#' @param l A list composed of a single environmental array (comprising temperature predictions for hours x layers x nodes) for a particular day.
#' @details Thermocline strength is approximated as the difference in temperature between the shallowest and the deepest Sigma layers. This is computationally faster than calculating other metrics (e.g. the standard deviation in temperature across layers). Positive numbers indicate the temperature at the surface is higher than at the bottom. This function cannot be applied properly to the data supplied with this package, which is a small subset of the temperature predictions including only the top two layers.
#' @return The function returns a 2-dimensional array of the thermocline strength for each hour and each node in the original array.
#' @examples
#' #### Step 1: Read example temperature file into a list
#' path <- system.file("WeStCOMS_files/temp/",
#'                     package = "WeStCOMSExploreR", mustWork = TRUE)
#' file <- list.files(path, full.names = TRUE)[1]
#' l <- list(R.matlab::readMat(file)$data)
#' str(l)
#'
#' #### Step 2: Compute thermocline strength
#' # (Note this is only for demonstration purposes:
#' # ... example package temperature predictions only include layers 1 and 2!)
#' thermocline_strength <- calc_thermocline(l)
#' hist(thermocline_strength)
#'
#' @seealso This function can be implemented within \code{\link[WeStCOMSExploreR]{compute_field_from_fvcom}}.
#' @author Edward Lavender
#' @export
#'

calc_thermocline <- function(l){
  stopifnot(is.list(l))
  stopifnot(length(l) == 1)
  fvcom_array <- l[[1]]
  n_layer <- dim(fvcom_array)[2]
  stopifnot(n_layer > 1)
  fvcom_field <- fvcom_array[, c(1, max(n_layer)), ]
  fvcom_field <- fvcom_field[, 1, ] - fvcom_field[, 2, ]
  return(fvcom_field)
}


#' @title Calculate wind or current speed from u and v component vectors
#' @description This function calculates wind or current speed from u and v component vector arrays.
#' @param l A list composed of two environmental arrays (u and v component arrays) for a particular day.
#' @details In each cell, speed (m/s) is given by Pythagoras' Theorem.
#' @return The function returns an array comprising wind/current speeds for each cell in the original arrays.
#' @examples
#' #### Step 1: Read example u and v files into a list
#' # Define the path to the u and v files:
#' path <- system.file("WeStCOMS_files/",
#'                     package = "WeStCOMSExploreR", mustWork = TRUE)
#' path_u <- paste0(path, "uwind_speed")
#' path_v <- paste0(path, "vwind_speed")
#' # Define the source files:
#' source_u <- list.files(path_u, full.names = TRUE)[1]
#' source_v <- list.files(path_v, full.names = TRUE)[1]
#' # Define a list comprising u and v arrays with one element for each day:
#' l <- lapply(list(source_u, source_v), function(source){
#'               R.matlab::readMat(source)$data
#'            })
#'
#' #### Step 2: Compute wind speed
#' wind_speed <- calc_speed(l)
#' hist(wind_speed)
#'
#' @seealso This function can be implemented within \code{\link[WeStCOMSExploreR]{compute_field_from_fvcom}}.
#' @author Edward Lavender
#' @export
#'
calc_speed <- function(l){
  stopifnot(is.list(l))
  stopifnot(length(l) == 2)
  udat <- l[[1]]
  vdat <- l[[2]]
  speed <- sqrt(udat^2 + vdat^2)
  return(speed)
}

#' @title Calculate wind or current direction from u and v component vectors
#' @description This function calculates wind or current direction from u and v component vector arrays.
#' @param l A list composed of two environmental arrays (u and v component arrays) for a particular day.
#' @details Wind or current direction are expressed as the direction (degrees) of mass flow.
#' @return The function returns an array comprising wind/current directions for each cell in the original array.
#' @seealso This function can be implemented within \code{\link[WeStCOMSExploreR]{compute_field_from_fvcom}}.
#' @examples
#' #### Step 1: Read example u and v files into a list
#' # Define the path to the u and v files:
#' path <- system.file("WeStCOMS_files/",
#'                     package = "WeStCOMSExploreR", mustWork = TRUE)
#' path_u <- paste0(path, "uwind_speed")
#' path_v <- paste0(path, "vwind_speed")
#' # Define the source files:
#' source_u <- list.files(path_u, full.names = TRUE)[1]
#' source_v <- list.files(path_v, full.names = TRUE)[1]
#' # Define a list comprising u and v arrays with one element for each day:
#' l <- lapply(list(source_u, source_v), function(source){
#'               R.matlab::readMat(source)$data
#'            })
#'
#' #### Step 2: Compute wind direction
#' wind_direction <- calc_direction(l)
#' graphics::hist(wind_direction)
#'
#' @author Edward Lavender
#' @export
#'

calc_direction <- function(l){
  stopifnot(is.list(l))
  stopifnot(length(l) == 2)
  udat <- l[[1]]
  vdat <- l[[2]]
  direction <- 90 - atan2(y = vdat, x = udat) * 180/pi
  posneg <- which(direction < 0)
  direction[posneg] <- direction[posneg] + 360
  return(direction)
}
