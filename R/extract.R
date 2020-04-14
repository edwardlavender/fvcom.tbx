#' @title Extract WeStCOMS predictions for a variable on specified dates/times/layers/mesh cells
#' @description This function automates the extraction of WeStCOMS predictions for a variable on specified dates/times/layers/mesh cells from WeStCOMS files. The function implements important checks, accounts for any corrupt files and can load WeStCOMS files in parallel. WeStCOMS predictions are returned as a dataframe because these form the backbone of data analysis in R and are easily converted to other objects (e.g. matrices, lists) where necessary.
#'
#' @param dat A dataframe which defines the WeStCOMS files date names, hours, layers (if applicable) and mesh_IDs for which WeStCOMS predictions should be extracted from WeStCOMS files. Columns should be named 'date_name', 'hour', 'layer' and 'mesh_ID' respectively.
#' @param match_hour A dataframe with two integer columns named 'hour' and 'index' which defines the index in WeStCOMS files (i.e. the row) which corresponds to each hour. The default dataframe is usually appropriate, if WeStCOMS files have not been subsetted. However, if WeStCOMS files have been subsetted (e.g. by selecting rows corresponding to hours 12 and 13), then rows 1 and 2 in WeStCOMS files now represent hours 12 and 13, not hours 0 and 1. \code{match_hour} provides the link which ensures that data for specified hours (e.g. hours 12 and 13) are correctly extracted from a WeStCOMS array (see Examples). All WeStCOMS files are assumed to have the same structure.
#' @param match_layer A dataframe with two integer columns named 'layer' and 'index' which defines the index in WeStCOMS files (i.e. the column) which corresponds to each layer. This is only necessary if you are working with 3d fields and if you are working with subsetted of WeStCOMS files: if WeStCOMS files have been subsetted (e.g. by selecting layers corresponding to columns 2 and 3), then columns 1 and 2 in WeStCOMS files now represent layers 2 and 3, not hours 1 and 2. \code{match_layer} provides this link which ensures that WeStCOMS predictions are extracted correctly (see Examples). All WeStCOMS files are assumed to have the same structure.
#' @param match_mesh (optional) A dataframe with two columns named 'mesh' and 'index' which defines the index in WeStCOMS files (columns or sheets for 2d and 3d arrays respectively) which corresponds to each mesh cell. This only needs to be provided if you are working with a subset of WeStCOMS files: in this situation, mesh IDs 5, 6, 7, for example, may not correspond to index 5, 6, 7 in WeStCOMS files. \code{match_mesh} provides the link which ensures that WeStCOMS predictions are extracted correctly (see Examples). All WeStCOMS files are assumed to have the same structure.
#' @param corrupt A vector of numbers, representing WeStCOMS date names, which define corrupt files. These will not be loaded.
#' @param read_fvcom A function which is used to load files. The function should take a single single argument, the file connection, as input, and load the file. The default is \code{\link{R.matlab}{readMat}} but other functions may be required depending on the format of FVCOM files (e.g. \code{\link[base]{readRDS}}).
#' @param dir2load A string which defines the directory from which to load WeStCOMS files. In this directory, WeStCOMS file names are assumed to follow the standard naming convention (i.e., yymmdd; see \code{\link[WeStCOMSExploreR]{date_name}}. All files with the pattern \code{*extension}, see \code{extension} are assumed to be WeStCOMS files.
#' @param extension A string which defines the extension of the WeStCOMS files. The default is \code{".mat"}.
#' @param cl (optional) A cluster objected created by the parallel package. If supplied, the algorithm is implemented in parallel. Note that the connection with the cluster is stopped within the function.
#' @param pass2varlist A list of character vector of names of objects to export to be passed to the \code{varlist} argument of \code{\link[parallel]{clusterExport}}.
#' @param verbose A logical input which defines whether or not to display messages to the console detailing function progress.
#'
#' @details The function is designed to extract WeStCOMS predictions for only one variable at a time. This is because WeStCOMS outputs different variables should be located in different files, may be different in dimension (2d versus 3d variables) and file type (usually files are .mat but WeStCOMSExploreR can compute and save outputs as .RData files). To implement the function for multiple variables, implement the function sequentially or in parallel for each variable. Non integer hours or layers are not currently implemented. If you require predictions for non integer hours or layers, include each the lower and upper hour or layer in \code{dat} and then interpolate predictions after these are provided. It is important to emphasise that WeStCOMS outputs are saved in multiple files (one for each day) by necessity: they contain a lot of data. Therefore, there are memory limitations which constrain the amount of data that can be extracted and returned by \code{extract}. If you need to extract many predictions to compute summary statistics or plots, use \code{\link[WeStCOMSExploreR]{explore}} which implements an iterative approach which avoids this issue by loading files iteratively and only saving outputs computed from each file, rather than all the predictions for each file. In other cases, \code{extract} can be implemented iteratively, with the outputs saved after every iteration in separate files.
#'
#' @return The function returns a dataframe which includes WeStCOMS predictions for specified dates/hours/layers and mesh cells.
#'
#' @examples
#'
#' #### Example (1) Implement extract() for a 2d variable
#' # Define dataframe specifying the dates/hours and locations for which we want predictions:
#' timestamp <- as.POSIXct(c("2016-03-01", "2016-03-02"), tz = "UTC")
#' dat <- data.frame(timestamp = timestamp)
#' dat$date_name <- date_name(dat$timestamp)
#' dat$hour <- lubridate::hour(dat$timestamp)
#' dat$mesh_ID <- dat_nodexy[1:nrow(dat), "node_id"]
#' # Define match dataframes to provide the link between dat and WeStCOMS arrays:
#' match_hour <- data.frame(hour = 0:1, index = 1:2)
#' match_mesh <- data.frame(mesh = dat_nodexy$node_id, index = 1:length(dat_nodexy$node_id))
#' # Define path
#' path <- system.file("WeStCOMS_files/tidal_elevation/",
#'                     package = "WeStCOMSExploreR", mustWork = TRUE)
#' # Implement extract() for a 2d variable
#' ext <- extract(dat = dat,
#'                match_hour = match_hour,
#'                match_mesh = match_mesh,
#'                dir2load = path,
#'                extension = ".mat",
#'                verbose = TRUE)
#'
#' #### Example (2) Implement extract for a 3d variable
#' # Define dat$layer
#' dat$layer <- 1
#' # Define match_layer if necessary
#' # Define path
#' path <- system.file("WeStCOMS_files/temp/",
#'                     package = "WeStCOMSExploreR", mustWork = TRUE)
#' # Implement extract()
#' ext <- extract(dat = dat,
#'                match_hour = match_hour,
#'                match_mesh = match_mesh,
#'                dir2load = path,
#'                extension = ".mat",
#'                verbose = TRUE)
#'
#' #### Example (3) Implement extract() in parallel
#' ext <- extract(dat = dat,
#'                match_hour = match_hour,
#'                match_mesh = match_mesh,
#'                dir2load = path,
#'                extension = ".mat",
#'                verbose = TRUE,
#'                cl = parallel::makeCluster(2L))
#'
#' @author Edward Lavender
#' @export
#'


###########################################
###########################################
#### extract()

extract <-
  function(dat,
           match_hour = data.frame(hour = 0:23, index = 1:24),
           match_layer = NULL,
           match_mesh = NULL,
           corrupt = NULL,
           read_fvcom = function(con){ R.matlab::readMat(con) },
           dir2load,
           extension = ".mat",
           cl = NULL,
           pass2varlist = NULL,
           verbose = TRUE){


    ########################################
    #### Check and process dat as required

    #### Algorithm start
    t1 <- Sys.time()
    if(verbose){
      cat("WeStCOMSExploreR::extract() called...\n")
      cat("Step 1: Initial checks/processing of dat...\n")
    }

    #### Check dat has been provided correctly
    stopifnot(all(c("date_name", "hour", "mesh_ID") %in% colnames(dat)))

    #### Exclude corrupt files
    dat <- exclude_corrupt(dat, corrupt)

    #### Exclude any dates without associated files
    dat <- exclude_unavailable(dat, dir2load, pattern = paste0("*", extension))


    ########################################
    #### Implements required to load FVCOM files

    if(verbose) cat("Step 2: Getting ready to load in FVCOM files...\n")

    #### Define indices to extract predictions
    # Define whether or not we're dealing with a 2d or 3d field:
    field3d <- ifelse(is.null(dat$layer), FALSE, TRUE)
    # Define indices
    if(!is.null(match_hour)){
      dat$index_hour <- match_hour$index[match(dat$hour, match_hour$hour)]
    } else{
      dat$index_hour <- dat$hour
    }
    if(!is.null(match_mesh)){
      dat$index_mesh <- match_mesh$index[match(dat$mesh_ID, match_mesh$mesh)]
    } else{
      dat$index_mesh <- dat$mesh_ID
    }
    if(field3d){
      if(!is.null(match_layer)){
        dat$index_layer <- match_layer$index[match(dat$layer, match_layer$layer)]
      } else{
        dat$index_layer <- dat$layer
      }
    }

    #### Define a list with one element for each unique date_name
    dat_ls <- split(dat, f = dat$date_name)

    #### Set up paralelisation if required
    if(!is.null(cl) & !is.null(pass2varlist)){
      varlist <- pass2varlist
      parallel::clusterExport(cl = cl, varlist = varlist)
    }


    ########################################
    #### Load FVCOM files

    #### Loop over every date_name and extract predictions for hours/layers/mesh IDs
    if(verbose) cat("Step 3: Loading FVCOM files and adding model predictions to dat...\n")
    pls <- pbapply::pblapply(dat_ls, cl = cl, FUN = function(df){

      #### Define connection and load file
      con <- paste0(dir2load, df$date_name[1], extension)
      if(verbose) cat(paste0("\n Loading file ", df$date_name[1], extension, "...\n"))
      wc <- read_fvcom(con)
      wc <- wc$data

      #### Extract values from fvcom depending on whether or not the inputted variable is 2d or 3d
      if(length(dim(wc)) == 3){
        stopifnot(!is.null(df$layer))
        df$wc <- wc[cbind(df$index_hour, df$index_layer, df$index_mesh)]
      } else if(length(dim(wc)) == 2){
        df$wc <- wc[cbind(df$index_hour, df$index_mesh)]
      } else{
        stop("2d or 3d matrix should be supplied")
      }

      #### Return predictions
      return(df$wc)

    })
    if(!is.null(cl)) parallel::stopCluster(cl)

    #### Add wc predictions to dataframe
    dat$wc <- as.numeric(unlist(pls))

    #### End time
    t2 <- Sys.time()
    tdiff <- round(difftime(t2, t1))
    if(verbose) cat(paste0("extract() algorithm duration approximately ", round(tdiff), " ",  methods::slot(tdiff, "units"), ".\n"))

    #### Return dataframe
    return(dat)

  }

#### End of code.
###########################################
###########################################
