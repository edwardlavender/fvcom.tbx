#' @title Compute sun angle across an unstructured mesh
#' @description This function computes sun angle on a given day across an unstructured mesh on the hours supplied. To calculate sun angle, the user must specify a dataframe containing node IDs and associated coordinates across which sun angle is evaluated. (Sun angle, a scalar variable, is computed at nodes for consistency with other WeStCOMS outputs.) Next, the user must specify the date and hours on that date for which to calculate sun angle. If this function is applied iteratively, the user can also specify a pre-computed blank matrix with a row for each hour specified and a column for each mesh cell, but this is not required. Finally, the user needs to specify whether or not sun angle should be returns in degrees, the directory to save files (not required) and whether or not to print messages/progress to the console.
#'
#' @param nodexy A dataframe containing node ids and coordinates (in latitude/longitude). The dataframe should have three columns: node_id, x and y. See \code{?WeStCOMSExploreR::dat_nodexy} for the dataset included in WeStCOMSExploreR as a guide.
#' @param date The date for which sun angle is to be calculated.
#' @param tz A character vector specifying the time zone. The default is "UTC".
#' @param date_name (optional) A character specifying the WeStCOMS date_name code that corresponds to the date supplied. This is only required if \code{dir2save = TRUE} (see below) because it is used to define the file name of the object saved. If \code{date_name} is not supplied, then it is calculated from the \code{date} supplied (with a small computational cost if the function is applied many times.)
#' @param hours A integer vector specifying the hours at which you want to calculate sun angle.
#' @param sun_angle_mat (optional) A blank matrix with a row for each hour and a column for each cell, into which sun angles will be added. This is is not required. However, if the function is applied iteratively, supplying a pre-computed matrix will improve computation time.
#' @param degrees A logical input defining whether or not sun angles should be returned in degrees (TRUE) or radians (FALSE).
#' @param dir2save (optional) A string specifying the directory in which to save sun angle files.
#' @param verbose A logical input specifying whether or not messages and a progress bar should be printed to the console. The default is TRUE.
#'
#' @examples
#'
#' #### (1) Compute sun angle across a sample of WeStCOMS nodes
#' sun_angle <- sun.angle.across.mesh(nodexy = WeStCOMSExploreR::dat_nodexy,
#' date = as.character("2016-01-01"),
#' date_name = 160101,
#' tz = "UTC",
#' hours = 1:24,
#' degrees = TRUE,
#' dir2save = NULL,
#' verbose = TRUE)
#' # Examine sun angle matrix produced:
#' str(sun_angle)
#'
#' @author Edward Lavender
#' @source This function is a wrapper for \code{\link[suncalc]{getSunlightPosition}} function.
#' @export


################################################
################################################
#### sun.angle.across.mesh

sun.angle.across.mesh <-
  function(nodexy,
           date,
           date_name = NULL,
           tz = "UTC",
           hours = 1:24,
           sun_angle_mat = NULL,
           degrees = TRUE,
           dir2save,
           verbose = TRUE
  ){

    #### Define IDxy dataframe
    ID <- nodexy$node_id
    IDxy <- data.frame(ID = ID, x = nodexy$x, y = nodexy$y)

    #### Define matrix in which to store sun angles, if not supplied:
    if(is.null(sun_angle_mat)){
      # calculate the number of cells for which we'll compute sun angle
      lID <- length(ID)
      # calculate the number of hours
      lhours <- length(hours)
      # create a blank matrix with rows for hours and columns for each cell
      sun_angle_mat <- matrix(NA, nrow = lhours, ncol = lID)
      # add rownames and column names to aid intepreation
      colnames(sun_angle_mat) <- sort(as.numeric(as.character(ID)))
      rownames(sun_angle_mat) <- hours
      }

    #### define timestamps at which to calculate sun_angle:
    suncalc_timestamp <- as.POSIXct(date, tz = "UTC") + hours*60*60

    ##### Display a helpful message...
    if(verbose){
      cat(paste("Calculating sun angle at all nodes on day:", date, "...\n"))
      pb_sun_angle <- utils::txtProgressBar(min = 0, max = lID, style = 3)
      } # close if(verbose)

    #### For every mesh cell, we'll calculate sun angle
    for(xy in 1:length(ID)){
      # obtain the coordinates of that cell from IDxy
      # note the importance of IDxy being in the same order as IDs.
      cellxy <- IDxy[xy, c("x", "y")]
      # calculate sun angle for that node:
      angles <- suncalc::getSunlightPosition(date = suncalc_timestamp,
                                             lat = cellxy$y,
                                             lon = cellxy$x,
                                             keep = "altitude")$altitude
      # Add to matrix in appropriate position:
      sun_angle_mat[, xy] <- angles
      # Update messages:
      if(verbose){
        # Update progress bar...
        utils::setTxtProgressBar(pb_sun_angle, xy)
      } # close if(verbose)
    } # close for(xy in ID){

    #### Convert angles to degrees, if requested
    if(degrees){
      sun_angle_mat <- sun_angle_mat * (180/pi)
      } # close if(degrees)

    #### save file for specified date in appropriate location, if specified:
    # If the user has supplied a dir2save...
    if(!is.null(dir2save)){
      # If the user hasn't provided a date name, then we'll define this:
      if(is.null(date_name)){
        date.name(date, define = "date_name")
      }
      # Define a path/filename to save based on datename:
      sun_angle_file_name <- paste0(dir2save, date_name, ".RData")
      # save the file:
      saveRDS(sun_angle_mat, file = sun_angle_file_name)
    }

    # Also return the computed matrix:
    return(sun_angle_mat)

  } # close function

#### End of code.
################################################
################################################
