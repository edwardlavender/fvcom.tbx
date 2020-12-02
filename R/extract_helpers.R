######################################
######################################
#### expand_by_hour()

#' @title Expand a dataframe with non-integer hours into one with surrounding integer hours
#' @description This function is designed to facilitate the extraction of FVCOM predictions at times that are not integer hours (the temporal resolution at which predictions are made). To this end, this function expands a dataframe with non-integer hours to include both surrounding integer hours. The resultant dataframe can be passed \code{\link[fvcom.tbx]{extract}} (outside of this function) to extract predictions for these integer hours. Predictions can then be interpolated to the non-integer hours of interest via \code{\link[fvcom.tbx]{shrink_by_hour}}.
#' @param dat A dataframe that defines the FVCOM array date names, hours, layers (if applicable) and mesh cell IDs for which FVCOM predictions are desired (see \code{\link[fvcom.tbx]{extract}}). However, unlike the direct implementation of \code{\link[fvcom.tbx]{extract}}, the 'hours' column should be named 'hour_dbl' to reflect the fact that predictions are desired at non-integer hours. Strictly speaking, only the 'hour_dbl' column is required for this function, but it is anticipated that this function will be implemented prior to \code{\link[fvcom.tbx]{extract}} and thus the dataframe will contain all other columns required for that function.
#' @return The function returns a dataframe, as inputted, but in which any rows with a non-integer 'hour_dbl' value have been expanded into two rows, with the hour before and the hour after the non-integer hour respectively defined in a new column 'hour' (as required for \code{\link[fvcom.tbx]{extract}}). A column 'order' is also added; this provides a unique identifier for each original row.
#'
#' @examples
#' #### Define example data for which FVCOM predictions are desired
#' # Times, including non integer hours
#' timestamp <- as.POSIXct(c("2016-03-01 00:00:00",
#'                           "2016-03-02 00:02:00",
#'                           "2016-03-01 00:00:00"))
#' dat <- data.frame(timestamp = timestamp)
#' # Define columns required to extract FVCOM predictions (see fvcom.tbx::extract())
#' dat$hour_dbl <- lubridate::hour(dat$timestamp) +
#'                 lubridate::minute(dat$timestamp)/60 +
#'                 lubridate::second(dat$timestamp)/3600
#' dat$date_name <- date_name(dat$timestamp)
#' dat$mesh_ID <- as.numeric(as.character(dat_mesh_around_nodes$ID))[1]
#'
#' #### Implement expand_by_hour()
#' dat <- expand_by_hour(dat)
#' # Now fvcom.tbx::extract() can be implemented to extract predictions
#' # And then predictions at integer hours can be interpolated to the original timestamps.
#' @author Edward Lavender
#' @export
#'

expand_by_hour <- function(dat){
  # Check dat contains necessary names
  check_names(input = dat,
              req = "hour_dbl",
              extract_names = colnames,
              type = all)
  # Expand date for all non integer hours
  if(any(c("order", "freq") %in% colnames(dat))){
    message("'order' and/or 'freq' colnames in 'dat' overwritten or deleted by 'fvcom.tbx::expand_by_hour()'.")
  }
  dat$order <- 1:nrow(dat)
  dat$freq <- (dat$hour_dbl != floor(dat$hour_dbl)) + 1
  if(all(dat$freq == 1)){
    message("No non-integer hours identified: 'dat' returned unchanged. \n")
    return(dat)
  }
  dat <- dat %>% dplyr::slice(rep(seq_len(dplyr::n()), .data$freq))
  # Assign integer hours
  second <- which(duplicated(dat$order))
  first <- second - 1
  dat$hour <- round(dat$hour_dbl)
  dat$hour[first] <- floor(dat$hour_dbl[first])
  dat$hour[second] <- ceiling(dat$hour_dbl[second])
  # Return dat
  dat$freq <- NULL
  return(dat)
}


######################################
######################################
#### shrink_by_hour()

#' @importFrom rlang .data
#' @title Shrink a dataframe with predictions at integer hours to one with interpolated predictions at non-integer hours
#' @description FVCOM predictions are resolved hourly. For predictions at non-integer hours, \code{\link[fvcom.tbx]{expand_by_hour}} expands a dataframe with non-integer hours to include both surrounding integer hours, the predictions of which can be obtained with \code{\link[fvcom.tbx]{extract}}. This function is designed to 'shrink' the expanded dataframe back down to its original size by interpolating predictions between hours to generate a single prediction for each original timestamp.
#' @param dat A dataframe that contains FVCOM predictions (see \code{\link[fvcom.tbx]{expand_by_hour}} and \code{\link[fvcom.tbx]{extract}}). At a minimum, this should contain the following columns: 'hour_dbl', a numeric vector that defines the time of day in hours; 'hour', an integer vector that defines the hour for which predictions have been extracted; 'index_hour', an integer vector that defines the index in the FVCOM arrays for each hour; order', an integer vector that defines each unique, original row (prior to dataframe expansion by \code{\link[fvcom.tbx]{expand_by_hour}}); and 'wc', an numeric vector that defines the prediction extracted from FVCOM array(s).
#' @param verbose A logical input that defines whether or not to print messages to the console to monitor function progress.
#' @return The function returns a dataframe, as inputted, but in which any predictions for non-integer hours have been interpolated from the predictions derived at the two surrounding hours. The 'hour' and 'index_hour' columns are dropped to avoid confusion.
#' @seealso \code{\link[fvcom.tbx]{interp_btw_hours}} linearly interpolates predictions between hours.
#'
#' @examples
#' #### Step (1): Define a dataframe that defines the FVCOM predictions required
#' # Times, including non integer hours
#' timestamp <- as.POSIXct(c("2016-03-01 00:00:00",
#'                           "2016-03-02 00:02:00",
#'                           "2016-03-01 00:00:00"))
#' dat <- data.frame(timestamp = timestamp)
#' # Define columns required to extract FVCOM predictions (see fvcom.tbx::extract())
#' dat$hour_dbl <- lubridate::hour(dat$timestamp) +
#'                 lubridate::minute(dat$timestamp)/60 +
#'                 lubridate::second(dat$timestamp)/3600
#' dat$date_name <- date_name(dat$timestamp)
#' dat$mesh_ID <- as.numeric(as.character(dat_mesh_around_nodes$ID))[1]
#' # Examine dat
#' dat
#'
#' #### Step (2): Implement expand_by_hour()
#' # ... to define integer hours at which to extract predictions
#' dat_exp <- expand_by_hour(dat)
#' # Examine dat_exp
#' dat_exp
#'
#' #### Step (3): Extract predictions
#' # Define matches (see ?fvcom.tbx::extract)
#' match_hour <- data.frame(hour = 0:1, index = 1:2)
#' match_mesh <- data.frame(mesh = dat_nodexy$node_id, index = 1:length(dat_nodexy$node_id))
#' # Define path
#' path <- system.file("WeStCOMS_files/tidal_elevation",
#'                      package = "fvcom.tbx", mustWork = TRUE)
#'                      path <- paste0(path, "/")
#' # Extract predictions
#' dat_with_wc <- extract(dat = dat_exp,
#'                        match_hour = match_hour,
#'                        match_mesh = match_mesh,
#'                        dir2load = path)
#' # Examine dat_with_wc
#' dat_with_wc
#'
#' #### Step (4): Implement shrink_by_hour() to interpolate predictions
#' dat_with_wc_interp <- shrink_by_hour(dat = dat_with_wc)
#' # Examine interpolated dataframe
#' dat_with_wc_interp
#' # Check interpolated value with interp_btw_hours()
#' interp_btw_hours(x = dat_with_wc$hour_dbl[2],
#'                  h1 = dat_with_wc$hour[2],
#'                  h2 = dat_with_wc$hour[3],
#'                  p1 = dat_with_wc$wc[2],
#'                  p2 = dat_with_wc$wc[3]
#'                  )
#'
#' @author Edward Lavender & Janneke Ransijn
#' @export
#'

shrink_by_hour <- function(dat, verbose = TRUE){
  check_names(input = dat,
              req = c("hour_dbl", "hour", "index_hour", "order", "wc"),
              extract_names = colnames,
              type = all)

  #### Isolate non-integer data
  if(verbose) cat("Isolating non-integer predictions...\n")
  cols <- colnames(dat)
  dat_non_int <-
    dat %>%
    dplyr::group_by(.data$order) %>%
    dplyr::filter(.data$hour_dbl >= .data$hour[1] & .data$hour_dbl <= .data$hour[2])

  #### Add required columns to interpolate between hours and interpolate
  if(verbose) cat("Interplating non-integer predictions...\n")
  dat_non_int <-
    dat_non_int %>%
    dplyr::mutate(d1 = abs(.data$hour_dbl - .data$hour[1]),
                  d2 = abs(.data$hour_dbl - .data$hour[2]),
                  delta = abs(.data$hour[2] - .data$hour[1]),
                  wc_interp = .data$wc[1] * ((.data$delta - .data$d1)/.data$delta) + .data$wc[2] * ((.data$delta - .data$d2)/.data$delta))

  #### Isolate the first row for the interpolated predictions
  # (only one row is required)
  dat_non_int <- dat_non_int %>% dplyr::slice(1)
  dat_non_int$wc <- dat_non_int$wc_interp
  dat_non_int <- dat_non_int[, cols]

  #### Isolate integer data and combine integer and non-integer data
  if(verbose) cat("Joining integer and non-integer predictions...\n")
  dat_int <- dat %>%
    dplyr::group_by(.data$order)%>%
    dplyr::filter(.data$hour_dbl <= .data$hour[1] | .data$hour_dbl >= .data$hour[2])
  dat_interp <-
    merge(dat_non_int, dat_int, all = TRUE)  %>%
    dplyr::arrange(.data$order)

  #### Drop 'hour' and 'index_hour' columns to avoid confusion
  dat_interp$hour <- NULL
  dat_interp$index_hour

  #### Return interpolated dataframe
  return(dat_interp)
}
