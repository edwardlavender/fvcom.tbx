#' @title Compute sun angle across an FVCOM mesh
#' @description This function computes the sun angle across a spatial mesh on the dates/times specified. To calculate sun angle, the user must specify a dataframe containing node IDs and associated coordinates across which sun angle is evaluated. (Sun angle, a scalar variable, is computed at nodes for consistency with other FVCOM outputs.) Next, the user must specify the date and hours on that date for which to calculate sun angle. Finally, the user needs to specify whether or not sun angle should be returned in degrees or radians, the directory to save files (not required) and whether or not to print messages/progress to the console.
#'
#' @param nodexy A dataframe containing node ids and decimal coordinates (in latitude/longitude). The dataframe should have three columns: 'node_id', 'x' and 'y'. See \code{\link[fvcom.tbx]{dat_nodexy}} for an example.
#' @param date A vector of dates (see \code{\link[base]{Date}}) for which sun angle is to be calculated.
#' @param tz A character vector specifying the time zone. The default is \code{"UTC"}.
#' @param sink_file (optional) A character specifying the name of sun angle fields, if saved as files (see \code{dir2save}, below). If \code{dir2save = TRUE} and \code{sink_file = NULL}, \code{\link[fvcom.tbx]{date_name}} is used to define file names from inputted dates.
#' @param hours A integer vector specifying the hours at which you want to calculate sun angle.
#' @param units A character input defining the units (\code{"degrees"} or \code{"radians"}) of sun angle.
#' @param iterative A logical input that defines whether or not to compute sun angle matrices iteratively for each date. The default option is \code{FALSE} but, for large sequences of dates and many nodes, an iterative implementation is necessary to avoid exhausting vector memory.
#' @param cl (optional) A cluster objected created by the parallel package. This is only implemented if \code{iterative = TRUE}. If supplied, the algorithm (computing sun angle matrices and, if applicable, saving these to file) is implemented in parallel. Note that the connection with the cluster is stopped within the function.
#' @param pass2varlist A list containing the names of exported objects. This may be required if \code{cl} is supplied. This is passed to the \code{varlist} argument of \code{\link[parallel]{clusterExport}}. Exported objects must be located in the global environment.
#' @param dir2save (optional) A string specifying the directory in which to save sun angle files.
#' @param verbose A logical input specifying whether or not messages and a progress bar should be printed to the console. The default is TRUE.
#'
#' @return For each date, the function creates a matrix of hours x mesh cells containing sun angles. Matrices are either returned as a list or saved as .rds files, with one file per day (if \code{dir2save = TRUE}).
#'
#' @examples
#' #### Example (1): Calculate sun angles for one date and some sample nodes
#' sun_angle <-
#'   compute_field_sun_angle(
#'     nodexy = dat_nodexy,
#'     date = as.Date("2016-01-01"),
#'     tz = "UTC",
#'     hours = 0:23,
#'     units = "degrees",
#'     dir2save = NULL,
#'     verbose = TRUE
#'   )
#' # The function returns a list with one element (matrix) for each date
#' # ... with rows (hours) x columns (nodes)
#' utils::str(sun_angle)
#'
#' #### Example (2): Implement algorithm iteratively over each date
#' # We're only specifying one example date here, but for demonstration purposes:
#' sun_angle <-
#'   compute_field_sun_angle(
#'     nodexy = dat_nodexy,
#'     date = as.Date("2016-01-01"),
#'     tz = "UTC",
#'     hours = 0:23,
#'     units = "degrees",
#'     dir2save = NULL,
#'     iterative = TRUE,
#'     verbose = TRUE
#'   )
#' utils::str(sun_angle)
#'
#' #### Example (3): Implement iterations in parallel via cl argument
#' # Again, for demonstration purposes only:
#' sun_angle <-
#'   compute_field_sun_angle(
#'     nodexy = dat_nodexy,
#'     date = as.Date("2016-01-01"),
#'     tz = "UTC",
#'     hours = 0:23,
#'     units = "degrees",
#'     dir2save = NULL,
#'     iterative = TRUE,
#'     cl = parallel::makeCluster(2L),
#'     verbose = TRUE
#'   )
#' utils::str(sun_angle)
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
           iterative = FALSE,
           cl = NULL,
           pass2varlist = NULL,
           verbose = TRUE
  ){

    #### Checks
    t1 <- Sys.time()
    if(verbose) cat("fvcom.tbx::compute_field_sun_angle() called...\n")
    if(verbose) cat("Checking user inputs...\n")
    check_value(arg = "units", input = units, supp = c("degrees", "radians"), warn = TRUE, default = "degrees")
    if(!is.null(dir2save)) {
      dir2save <- check_dir(input = dir2save, check_slash = TRUE)
      if(!is.null(sink_file)) check_length(arg = "sink_file", input = sink_file, req_length = length(date), req_arg = "length(date)")
    }
    if(!iterative) if(!is.null(cl) | !is.null(pass2varlist)) message("Cluster (implemented via 'cl' and 'pass2varlist') ignored if iterative = FALSE.")
    if(is.null(cl)) if(!is.null(pass2varlist)) message("pass2varlist not implemented without cluster.")

    #### Process user inputs and define necessary variables
    if(verbose) cat("Processing mesh nodes and timestamps...\n")
    nodexy$index <- 1:nrow(nodexy)
    nrw <- length(hours)
    ncl <- max(nodexy$index)
    if(!inherits(date, "Date")) date <- as.Date(date)
    secs <- hours*60*60
    timestamp <- lapply(as.POSIXct(paste0(date, "00:00:00"), tz = tz), function(day) day + secs)
    timestamp <- sort(do.call(c, timestamp))
    lubridate::tz(timestamp) <- tz

    #### Non iterative algorithm implementation
    if(!iterative){
      if(verbose) cat("Setting up non-iterative algorithm...\n")

      #### Define dataframe to calculate sun_angle:
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
      if(units == "degrees") dat$altitude <- dat$altitude * (180/pi)

      #### Define matrices
      if(verbose) cat("Defining sun angle arrays...\n")
      dat_by_date <- split(dat, f = as.Date(dat$date))
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
      }

    } else{

      #### Iterative algorithm implementation
      if(verbose) cat("Setting up iterative algorithm...\n")
      tmp <- data.frame(date = as.Date(timestamp), timestamp = timestamp)
      if(!is.null(sink_file)){
        dsink <- data.frame(date = date, sink_file = sink_file)
        tmp$sink_file <- dsink$sink_file[match(tmp$date, dsink$date)]
      } else{
        tmp$sink_file <- date_name(tmp$date, define = "date_name")
      }
      dat_by_date <- split(tmp, tmp$date)

      #### Loop over each date and compute and return/save the files for that date
      if(verbose) {
        if(!is.null(dir2save)){
          cat("Implementing iterative algorithm: computing and saving sun angle matrices for each day...\n")
        } else{
          cat("Implementing iterative algorithm: computing sun angle matrices for each day...\n")
        }
      }
      if(!is.null(cl) & !is.null(pass2varlist)) parallel::clusterExport(cl = cl, varlist = pass2varlist)
      sun_angle_mat_ls <- pbapply::pblapply(dat_by_date, cl = cl, function(d){

        #### Set up dataframe with observations for each timestamp for each node
        dat <- expand.grid(d$timestamp, nodexy$node_id)
        colnames(dat) <- c("date", "mesh_ID")
        dat$nodexy_index <- nodexy$index[match(dat$mesh_ID, nodexy$node_id)]
        dat$lat <- nodexy$y[match(dat$mesh_ID, nodexy$node_id)]
        dat$lon <- nodexy$x[match(dat$mesh_ID, nodexy$node_id)]
        dat$hour <- lubridate::hour(dat$date)
        dat <- dat[order(dat$date, dat$mesh_ID), ]

        #### Compute sun angle
        dat$altitude <- suncalc::getSunlightPosition(data = dat[, c("date", "lon", "lat")], keep = c("altitude"))$altitude
        if(units == "degrees") dat$altitude <- dat$altitude * (180/pi)

        #### Define matrix of sun angles
        mat <- matrix(dat$altitude, nrow = nrw, ncol = ncl, byrow = TRUE)
        colnames(mat) <- nodexy$node_id
        rownames(mat) <- hours

        #### Save file for specified date in appropriate location, if specified:
        # If the user has supplied a dir2save...
        if(!is.null(dir2save)){
          saveRDS(mat, paste0(dir2save, d$sink_file[1], ".rds"))
          return(NULL)
        } else return(mat)
      })
      if(!is.null(cl)) parallel::stopCluster(cl)
    }

    #### Determine algorithm duration
    t2 <- Sys.time()
    tdiff <- round(difftime(t2, t1))
    if(verbose) cat(paste0("fvcom.tbx::compute_field_sun_angle() algorithm duration approximately ", round(tdiff), " ",  methods::slot(tdiff, "units"), ".\n"))

    #### Return sun angle matrices, if requested
    if(is.null(dir2save)) return(sun_angle_mat_ls)

  }


#### End of code.
################################################
################################################
