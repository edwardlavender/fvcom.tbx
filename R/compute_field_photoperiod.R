#' @title Compute photoperiod across an FVCOM mesh
#' @description This function computes photoperiod as an environmental field across an spatial mesh.
#' @param date A vector of dates (see \code{\link[base]{Date}}) for which photoperiod will be calculated.
#' @param nodexy A dataframe containing decimal coordinates (in latitude/longitude) at which photoperiod is calculated. The dataframe should have two columns: 'x' and 'y'. See \code{\link[WeStCOMSExploreR]{dat_nodexy}} for an example.
#' @param interval A character vector with two elements which define the specific sunrise and sunset phases (in that order) between which photoperiod is calculated. This is passed to the \code{keep} argument of \code{\link[suncalc]{getSunlightTimes}} where further details are provided. The default option is to calculate the duration between dawn and dusk in each location on each day.
#' @param units A character input which defines the units of the time difference. This is passed to \code{\link[base]{difftime}}. Options are: "auto", "secs", "mins", "hours", "days" or "weeks".
#' @details At specified locations (taken from \code{nodexy}) and on specified dates (taken from \code{date}), photoperiod is calculated as the number of time units (e.g. hours) between two discrete sunlight times (e.g. dawn and dusk), defined by the user and calculated by \code{\link[suncalc]{getSunlightTimes}}. For very long sequences of dates/locations, the function may take several minutes.
#' @return Unlike other \code{compute_field_*()} functions, photoperiod is only defined on each day (not for each hour of the day). Therefore, rather than returning an array, the function returns a dataframe with the date ('date'), the location ('lat' and 'lon'), the times of the user-specified interval (e.g., 'dawn' and 'dusk') and the photoperiod ('photoperiod') in user-specified units. The dataframe is ordered by date.
#' @examples
#' #### Example (1): Compute photoperiod as the number of hours between dawn and dusk:
#' field_photoperiod <-
#'   compute_field_photoperiod(date = as.Date(c("2016-03-01", "2016-03-02")),
#'                            nodexy = dat_nodexy,
#'                             interval = c("dawn", "dusk"),
#'                             units = "hours")
#' utils::head(field_photoperiod)
#'
#' #### Example (2): Compute photoperiod as the number of days between dawn and dusk
#' field_photoperiod <-
#'   compute_field_photoperiod(date = as.Date(c("2016-03-01", "2016-03-02")),
#'                             nodexy = dat_nodexy,
#'                             interval = c("dawn", "dusk"),
#'                             units = "days")
#' utils::head(field_photoperiod)
#'
#' #### Example (3): Compute photoperiod as the number of days between sunrise and sunset:
#' field_photoperiod <-
#'  compute_field_photoperiod(date = as.Date(c("2016-03-01", "2016-03-02")),
#'                             nodexy = dat_nodexy,
#'                             interval = c("sunrise", "sunset"),
#'                             units = "days")
#' utils::head(field_photoperiod)
#'
#' @author Edward Lavender
#' @export
#'
compute_field_photoperiod <-
  function(
    date,
    nodexy,
    interval = c("dawn", "dusk"),
    units = "hours"
  ){

    #### Checks
    ## Check nodexy contains required names
    check_names(arg = "nodexy",
                input = nodexy,
                req = c("x", "y"),
                extract_names = colnames,
                type = all)

    #### Define a dataframe with the photoperiod on each date
    nodexy_nrw <- nrow(nodexy)
    loc_index <- 1:nodexy_nrw
    dat <- expand.grid(date, loc_index)
    colnames(dat) <- c("date", "loc")
    dat <- dat[order(dat$date, dat$loc), ]
    dat$lon <- nodexy$x[match(dat$loc, loc_index)]
    dat$lat <- nodexy$y[match(dat$loc, loc_index)]
    dat$loc <- NULL
    photoperiod_df <- suncalc::getSunlightTimes(data = dat,
                                                keep = interval)

    #### Calculate photoperiod in user-specified units:
    photoperiod_df$photoperiod <-
      as.numeric(difftime(photoperiod_df[, interval[2]],
                          photoperiod_df[, interval[1]],
                          units = units)
                 )

    #### Return data.frame
    return(photoperiod_df)

  }

