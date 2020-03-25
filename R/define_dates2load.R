#' @title  Create a dataframe that specified which WeStCOMS files to load
#' @description This function creates a dataframe that can be used to load WeStCOMS files into R or MATLAB.
#'
#' @param start_date A date object specifying the earliest date for which you have a WeStCOMS file.
#' @param end_date A date object specifying the latest date for which you have a WeStCOMS file. Specifying \code{start_date} and \code{end_date} is appropriate if you want to load all dates between a start and end date. If not, see \code{custom_dates}.
#' @param custom_dates A vector of dates, for which to create the dataframe. This is an alternative to specifing \code{start_date} and \code{end_date} (see above).
#' @param corrupt_dates A vector of dates for which WeStCOMS files are corrupt and should not be included in the dataframe.
#'
#' @return A dataframe with three columns: date (Date object), year (2 digits, a numeric object) and date_name (6 digits, a numeric object).
#'
#' @author Edward Lavender
#'
#' @examples
#'
#' # 1) Define a dataframe for a regular sequence of dates between a start and end date:
#' define_dates2load(start_date = as.Date("2016-03-01"),
#'                   end_date = as.Date("2016-03-05"),
#'                   corrupt_dates = NULL)
#'
#' # 2) Define a dataframe as above, but removing some corrupt dates:
#' define_dates2load(start_date = as.Date("2016-03-01"),
#'                   end_date = as.Date("2016-03-05"),
#'                   corrupt_dates = as.Date(c("2016-03-02", "2016-03-03"))
#'                   )
#'
#' # 3) Define a dataframe for a custom series of dates:
#' define_dates2load(custom_dates = as.Date(c("2016-03-02", "2016-03-03", "2017-01-01")))
#'
#' @export
#'

################################################
#### Define function:

define_dates2load <-
  function(start_date,
           end_date,
           custom_dates = NULL,
           corrupt_dates = NULL){

    #### If custom dates have not specified:
    if(is.null(custom_dates)){
      # Check these are a date object:
      if(class(start_date) != "Date" | class(end_date) != "Date"){
        stop("start_date and end_date should be a Date object.")
        }
      # Define a sequence of dates between the start and end date:
      dates <- seq.Date(start_date, end_date, by = 1)

    #### Otherwise, use the custom dates provided:
    } else{
      # Check custom_dates are a date object:
      if(class(custom_dates) != "Date"){
        stop("custom_dates should be a Date object.")
      }
      # Define dates:
      dates <- custom_dates
    }

    #### define dataframe
    fvcom <- data.frame(date = dates)
    # add year
    fvcom$year <- as.numeric(substr(lubridate::year(fvcom$date), 3, 4))
    # define date_name
    fvcom$date_name <- date_name(x = fvcom$date, define = "date_name")

    #### Filter out corrupt dates:
    if(!is.null(corrupt_dates)){
      # Check corrupt dates are a Date object:
      if(class(corrupt_dates) != "Date"){
        stop("Corrupt dates should be a Date object.")
      }
      # Remove rows with corrupt dates:
      fvcom <- fvcom[!(fvcom$date %in% corrupt_dates), ]
    } # close if(!is.null(corrupt_dates)){

    #### return dataframe:
    return(fvcom)
  } # close function


################################################
################################################
