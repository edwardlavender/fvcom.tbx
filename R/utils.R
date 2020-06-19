#############################
#############################
#### pipe

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


###############################
###############################
#### match_ts_nearest() from Tools4ETS
# Source: https://github.com/edwardlavender/Tools4ETS
# 18/06/2020

#' @title Find the position in one vector that is nearest in time to a value in another dataframe
#' @import data.table
#'
#' @description This function is like \code{\link[base]{match}}, but the aim is, for a given sequence of times (\code{times}), to find the positions in another sequence of times (\code{lookup}) that are nearest in time to those in the first sequence. In other words, for every time inputted, \code{match_ts_nearest()} finds the position in another sequence of times which is nearest in time to that time. This is useful if, for example, you have an existing dataframe to which you want to add the observations, held in another dataframe, that are nearest in time to observations in the first dataframe (i.e., nearest neighbour interpolation). This function uses \code{\link[data.table]{data.table}} for fast matching, even with very large vectors.
#'
#' @param times A vector of timestamps for which you want to identify the position of the nearest timestamp in another vector (\code{lookup}).
#' @param lookup A vector of timestamps for which you will determine the position of the nearest timestamp to each time in \code{times}.
#'
#' @details If there are multiple matches, only the first is returned.
#'
#' @return For a sequence of times (\code{times}), the function returns a vector of the positions of the nearest times in another sequence (\code{lookup}).
#'
#' @author Edward Lavender
#' @keywords internal
#'

match_ts_nearest <- function(times, lookup){
  # Define data.table objects, adding indices to both tables;
  # We will use the index to ensure that observations are returned in the correct order.
  # This also means that 'times' input does not need to be arranged by time.
  dt1 <- data.table::data.table(t = times, index_times = 1:length(times))
  dt2 <- data.table::data.table(t = lookup, index_lookup = 1:length(lookup))
  # Set the key for both tables, arranging by time
  data.table::setkey(dt1, t)
  data.table::setkey(dt2, t)
  # Join the data tables by the observations that are nearest in time
  # This adds the index_lookup column to dt1, adding the values in index_lookup
  # ... that are nearest in time to the times in dt1
  djoin <- dt2[dt1, roll = "nearest", mult = "first"]
  # Reorder djoin by index_times so that the vector of positions in lookup
  # ... returned is in the appropriate order given order of inputted times
  # NB set index times to NULL to avoid 'no visible binding for global variable ‘index_times’'
  index_times <- NULL
  data.table::setkey(djoin, index_times)
  return(djoin$index_lookup)
}

#### End of code.
###############################
###############################
