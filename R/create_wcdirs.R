#' @title Create folders in which to save environmental arrays
#' @description This function creates a set of folders in the specified directory in which environmental arrays (one file for each day) can be stored.
#'
#' @param dir A string specifying the directory in which to create a folder for each environmental variable.
#' @param vars A character vector specifying the names of the folders to create in \code{dir}. Folders that already exist will not be overwritten; in this case, the function simply returns a warning.
#'
#' @author Edward Lavender
#'
#' @examples
#'
#'\dontrun{
#' # Define folders for several variables in the working directory:
#' create_wcdirs(dir = getwd(), vars = c("temp", "salinity", "tidal_elevation", "short_wave"))
#'}
#'
#' @export


################################################
#### Define function:

create_wcdirs <-
  function(
    dir = getwd(),
    vars = "temp"){

  # Define the number of characters in the inputted directory
  l <- nchar(dir)
  # Add a "/" to the end of the directory if required:
  if(substr(dir, l-1, l) != "/"){
    dir <- paste0(dir, "/")
  }

  # Define an object 'ls' in which to store the output of the loop, below:
  ls <-
    # loop over every variable:
    lapply(vars, function(var){
      # Define the path to be created
      path <- paste0(dir, var, "/")
      # If the path does not exist...
      if(!file.exists(path)){
        # Create the path:
        dir.create(path)
        # Otherwise, return a helpful warning explaining that variable was skipped:
      } else{
        warning(paste("path", path, "(for variable", var, ") already exists and was skipped."))
      }
    }) # close function and lapply
  } # close function





################################################
################################################
