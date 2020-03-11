#' @title Flip between WeStCOMS file names and dates
#' @description This function converts a date to the 6 digit code that relates that date to a WeStCOMS file name (or vice versa).
#'
#' @param x A vector of dates or date names to be converted.
#' @param define A character that specifes whether 'x' is to be converted from a date to a date_name (define = "date_name") or from a date_name to a date ("date").
#'
#' @author Edward Lavender
#'
#' @examples
#' # 1) Extract the WeStCOMS file name as a numeric object from some dates:
#' date.name(x = as.Date(c("2016-03-01", "2016-03-02")), define = "date_name")
#'
#' # 2) Define a sequence of dates from WeStCOMS file names:
#' date.name(x = c(160101, 170101), define = "date")
#' @export

date.name <-
  function(x, define = "date_name"){

    #### Check that the input to define is allowed:
    if(!(define %in% c("date_name", "date"))){
      stop("You have supplied the 'define' argument with an unsupported string: 'date_name' or 'date' are supported.")
    }

    #### If the user has selected to define date_name from a date...
    if(define == "date_name"){
      #### Check they have inputted a Date or POSIXct object:
      if(!(class(x) %in% c("Date", "POSIXct"))){
        stop("Please enter a Date or POSIXct object")
      }

      #### Define date_name:
      # extract day and add a leading 0 if day is a single digit
      d <- lubridate::day(x)
      d <- sprintf("%02d", d)
      # extract month and add a leading 0 if month is a single digit
      m <- lubridate::month(x)
      m <- sprintf("%02d", m)
      # extract year and take the last two digits (e.g. 16 in 2016)
      y <- lubridate::year(x)
      y <- substr(y, 3, 4)
      # define date_name as a numeric object
      date_name <- as.numeric(paste0(y, m, d))
      # return date_name as a numeric object:
      return(date_name)
      # close function
    } # close if(define == "date_name"){

    #### if the user has selected to define a date from a date_name object...
    if(define == "date"){

      #### Make sure x is recognised as a character:
      x <- as.character(x)

      #### Check that all inputted date_names have the correct number of characters:
      if(!any((nchar(x) == 6))){
        stop("The inputted value for 'x' it not a recognised date name. These have 6 exactly 6 characters in the form YYMMDD.")
      }

      #### Define date:
      # Define year (4 digits):
      yyyy <- paste0("20", substr(x, 1, 2))
      # Define month:
      mm <- substr(x, 3, 4)
      # Define day:
      dd <- substr(x, 5, 6)
      # Define date:
      date <- paste0(yyyy, "-", mm, "-", dd)
      # Return date as a character object:
      return(date)

    } # close if(define == "date"){
  } # close function
