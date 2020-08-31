#' @title Compute sun angle across an FVCOM mesh
#' @description This function computes the sun angle across a spatial mesh on the dates/times specified. To calculate sun angle, the user must specify a dataframe containing node IDs and associated coordinates across which sun angle is evaluated. (Sun angle, a scalar variable, is computed at nodes for consistency with other FVCOM outputs.) Next, the user must specify the date and hours on that date for which to calculate sun angle. Finally, the user needs to specify whether or not sun angle should be returns in degrees or radians, the directory to save files (not required) and whether or not to print messages/progress to the console.
#'
#' @param nodexy A dataframe containing node ids and decimal coordinates (in latitude/longitude). The dataframe should have three columns: 'node_id', 'x' and 'y'. See \code{\link[WeStCOMSExploreR]{dat_nodexy}} for an example.
#' @param date A vector of dates (see \code{\link[base]{Date}}) for which sun angle is to be calculated.
#' @param tz A character vector specifying the time zone. The default is \code{"UTC"}.
#' @param sink_file (optional) A character specifying the name of sun angle fields, if saved as files (see \code{dir2save}, below). If \code{dir2save = TRUE} and \code{sink_file = NULL}, \code{\link[WeStCOMSExploreR]{date_name}} is used to define file names from inputted dates.
#' @param hours A integer vector specifying the hours at which you want to calculate sun angle.
#' @param units A character input defining the units (\code{"degrees"} or \code{"radians"} of sun angle.
#' @param dir2save (optional) A string specifying the directory in which to save sun angle files.
#' @param verbose A logical input specifying whether or not messages and a progress bar should be printed to the console. The default is TRUE.
#'
#' @return For each date, the function creates a matrix of hours x mesh cells containing sun angles. Matrices are either returned as a list or saved as .rds files, with one file per day (if \code{dir2save = TRUE}).
#'
#' @examples
#'
#' #### (1) Compute sun angle across a sample of WeStCOMS nodes
#' sun_angle <- WeStCOMSExploreR::compute_field_sun_angle(
#'   nodexy = WeStCOMSExploreR::dat_nodexy,
#'   date = as.character("2016-01-01"),
#'   tz = "UTC",
#'   hours = 0:23,
#'   units = "degrees",
#'   dir2save = NULL,
#'   verbose = TRUE
#'   )
#'
#' @author Edward Lavender
#' @source This function is a wrapper for \code{\link[suncalc]{getSunlightPosition}} function.
#' @export


################################################
################################################
#### compute_field_sun_angle

compute_field_sun_angle <-
  function(nodexy,
           date,
           tz = "UTC",
           hours = 0:23,
           units = "degrees",
           sink_file = NULL,
           dir2save = NULL,
           verbose = TRUE
  ){

    #### Checks
    if(!is.null(dir2save)) dir2save <- check_dir(input = dir2save, check_slash = TRUE)

    #### Define dataframe to calculate sun_angle:
    if(verbose) cat("WeStCOMSExploreR::compute_field_sun_angle() called...\n")
    nodexy$index <- 1:nrow(nodexy)
    date <- as.POSIXct(date, tz = tz)
    secs <- hours*60*60
    timestamp <- lapply(date, function(day) day + secs)
    timestamp <- sort(do.call(c, timestamp))
    lubridate::tz(timestamp) <- tz
    dat <- expand.grid(timestamp, nodexy$node_id)
    colnames(dat) <- c("date", "mesh_ID")
    dat$nodexy_index <- nodexy$index[match(dat$mesh_ID, nodexy$node_id)]
    dat$lat <- nodexy$y[match(dat$mesh_ID, nodexy$node_id)]
    dat$lon <- nodexy$x[match(dat$mesh_ID, nodexy$node_id)]
    dat$hour <- lubridate::hour(dat$date)
    dat <- dat[order(dat$date, dat$mesh_ID), ]

    #### Compute sun angle
    if(verbose) cat("Computing sun angle...\n")
    dat$altitude <- suncalc::getSunlightPosition(data = dat[, c("date", "lon", "lat")], keep = c("altitude"))$altitude
    check_value(arg = "units", input = units, supp = c("degrees", "radians"), warn = TRUE, default = "degrees")
    if(units == "degrees") dat$altitude <- dat$altitude * (180/pi)

    #### Define matrices
    if(verbose) cat("Defining sun angle arrays...\n")
    dat_by_date <- split(dat, f = as.Date(dat$date))
    nrw <- length(hours)
    ncl <- max(nodexy$index)
    sun_angle_mat_ls <- lapply(dat_by_date, function(d){
      mat <- matrix(d$altitude, nrow = nrw, ncol = ncl, byrow = TRUE)
      colnames(mat) <- nodexy$node_id
      rownames(mat) <- hours
      return(mat)
    })

    #### Save file for specified date in appropriate location, if specified:
    # If the user has supplied a dir2save...
    if(!is.null(dir2save)){
      if(verbose) cat("Saving sun angle arrays... \n")
      # Define file names, if not provided
      if(is.null(sink_file)) sink_file <- date_name(date, define = "date_name")
      # Save each file
      out <- mapply(sun_angle_mat_ls, sink_file, FUN = function(sun_angle_mat, file){
        saveRDS(sun_angle_mat, paste0(dir2save, file, ".rds"))
      })
    } else{
      return(sun_angle_mat_ls)
    }
  }


#### End of code.
################################################
################################################
