##############################################
##############################################
#### exclude_corrupt()

#' @title Exclude any rows in a dataframe associated with corrupt WeStCOMS files
#' @description This function screens vector of WeStCOMS file names in a dataframe and removes any rows which refer to the names of known, corrupt files (as defined by the user). This is an important check prior to loading multiple WeStCOMS files into R.
#'
#' @param dat A dataframe (e.g. containing information necessary to extract WeStCOMS outputs). The only requirement is an integer column named 'date_name' which contains the 6 digit code of WeStCOMS file names (see \code{\link[WeStCOMSExploreR]{date_name}}).
#' @param corrupt A vector of integers which define the 6 digit code of any corrupt WeStCOMS files.
#'
#' @return A dataframe, as inputted, but in which any rows associated with corrupt WeStCOMS files have been excluded. If rows have been excluded, the function returns a warning.
#'
#' @examples
#'
#' \dontrun{
#' exclude_corrupt(data.frame(date_name = c(160301, 160302, 160303)), 160301)
#' }
#'
#' @seealso This check is implemented by \code{\link[WeStCOMSExploreR]{extract}}.
#'
#' @author Edward Lavender
#' @export
#'

exclude_corrupt <- function(dat, corrupt = NULL){
  if(is.null(corrupt)){
    return(dat)
  } else{
    stopifnot("date_name" %in% colnames(dat))
    pos_corrupt <- which(dat$date_name %in% corrupt)
    if(length(pos_corrupt) > 0){
      warning(paste(length(pos_corrupt), "observation(s) associated with corrupt file(s) excluded. \n"))
      dat <- dat[-c(pos_corrupt), , drop = FALSE]
    }
    stopifnot(nrow(dat) > 0)
    return(dat)
  }
}


##############################################
##############################################
#### exclude_unavailable()

#' @title Exclude any rows in a dataframe associated with unavailable WeStCOMS files
#' @description  This function screens vector of WeStCOMS file names in a dataframe and removes any rows wich refer to WeStCOMS files that are unavailable in a specified directory. This is an important check prior to loading multiple WeStCOMS files into R.
#'
#' @param dat A dataframe (e.g. containing information necessary to extract WeStCOMS outputs). The only requirement is an integer column named 'date_name' which contains the 6 digit code of WeStCOMS file names (see \code{\link[WeStCOMSExploreR]{date_name}}).
#' @param dir2load A string which defines the directory from which WeStCOMS files are loaded. The function identifies all files in this directory (with necessary properties, see \code{...}) to determine whether any of the date names in \code{dat} are not found \code{dir2load}. If this is the case, these rows which refer to unavailable files in \code{dat} are removed.
#' @param ... Additional arguments passed to \code{\link[base]{list.files}}, such as \code{pattern}.
#'
#' @return A dataframe, as inputted, but in which any rows which refer to the names of unavailable WeStCOMS files have been excluded. If rows have been excluded, the function returns a warning.
#'
#' @examples
#'
#' \dontrun{
#' path <- system.file("WeStCOMS_files/tidal_elevation/",
#'                     package = "WeStCOMSExploreR", mustWork = TRUE)
#' exclude_unavailable(data.frame(date_name = c(160301, 160302, 160303)), path)
#' exclude_unavailable(data.frame(date_name = c(160301, 160302, 160303), depth = c(60,  65, 90)), path)
#' }
#'
#' @seealso This check is implemented by \code{\link[WeStCOMSExploreR]{extract}}.
#'
#' @author Edward Lavender
#' @export
#'

exclude_unavailable <- function(dat, dir2load,...){
  stopifnot("date_name" %in% colnames(dat))
  files <- list.files(dir2load,...)
  stopifnot(length(files) > 0)
  file_codes <- as.numeric(substr(files, 1, 6))
  pos_unavailable <- which(!(dat$date_name %in% file_codes))
  if(length(pos_unavailable) > 0){
    warning(paste(length(pos_unavailable), "observation(s) associated with unavailable predictions excluded. \n"))
    dat <- dat[-c(pos_unavailable), , drop = FALSE]
  }
  stopifnot(nrow(dat) > 0)
  return(dat)
}



#### End of code.
##############################################
##############################################
