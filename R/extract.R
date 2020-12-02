#' @title Extract predictions from FVCOM arrays
#' @description This function automates the extraction of FVCOM predictions for a variable on specified dates/times/layers/mesh cells from FVCOM arrays. The function implements important checks, accounts for any corrupt files and can load FVCOM arrays in parallel. FVCOM predictions are returned as a dataframe because these form the backbone of data analysis in R and are easily converted to other objects (e.g. matrices, lists) where necessary.
#'
#' @param dat A dataframe which defines the FVCOM array date names, hours, layers (if applicable) and mesh cell IDs for which FVCOM predictions should be extracted from FVCOM arrays. Columns should be named 'date_name', 'hour', 'layer' and 'mesh_ID' respectively.
#' @param match_hour A dataframe with two integer columns named 'hour' and 'index' which defines the index in FVCOM arrays (i.e. the row) which corresponds to each hour. The default dataframe is usually appropriate, if FVCOM arrays have not been subsetted. However, if FVCOM arrays have been subsetted (e.g. by selecting rows corresponding to hours 12 and 13), then rows 1 and 2 in FVCOM arrays now represent hours 12 and 13, not hours 0 and 1. \code{match_hour} provides the link which ensures that data for specified hours (e.g. hours 12 and 13) are correctly extracted from an FVCOM array (see Examples). All FVCOM arrays in \code{dir2load} are assumed to have the same structure.
#' @param match_layer A dataframe with two integer columns named 'layer' and 'index' which defines the index in FVCOM arrays (i.e. the column) which corresponds to each layer. This is only necessary if you are working with 3-dimensional fields and if you are working with subsetted of FVCOM arrays: if FVCOM arrays have been subsetted (e.g. by selecting layers corresponding to columns 2 and 3), then columns 1 and 2 in FVCOM arrays now represent layers 2 and 3, not layers 1 and 2. \code{match_layer} provides this link which ensures that FVCOM predictions are extracted correctly (see Examples). All FVCOM arrays in \code{dir2load} are assumed to have the same structure.
#' @param match_mesh (optional) A dataframe with two columns named 'mesh' and 'index' which defines the index in FVCOM arrays (columns or sheets for 2-dimensional and 3-dimensional arrays respectively) which corresponds to each mesh cell. This only needs to be provided if you are working with a subset of FVCOM arrays: in this situation, mesh IDs 5, 6, 7, for example, may not correspond to index 5, 6, 7 in FVCOM arrays. \code{match_mesh} provides the link which ensures that FVCOM predictions are extracted correctly (see Examples). All FVCOM arrays in \code{dir2load} are assumed to have the same structure.
#' @param corrupt A vector of numbers, representing FVCOM date names, which define corrupt files. These will not be loaded.
#' @param read_fvcom A function which is used to load files (and, if necessary, extract the FVCOM array from the loaded object). The function should take a single single argument, the file connection, as input, and load the file. The default is \code{function(con) = R.matlab::readMat(con)$data} but other functions may be required depending on the format of FVCOM files (e.g. \code{\link[base]{readRDS}}).
#' @param dir2load A string which defines the directory from which to load FVCOM arrays. In this directory, FVCOM file names are assumed to follow the standard naming convention (i.e., yymmdd; see \code{\link[fvcom.tbx]{date_name}}). All files with the pattern \code{*extension} (see \code{extension}) are assumed to be FVCOM arrays.
#' @param extension A string which defines the extension of the FVCOM arrays. The default is \code{".mat"}.
#' @param cl (optional) A cluster objected created by the parallel package. If supplied, the algorithm is implemented in parallel. Note that the connection with the cluster is stopped within the function.
#' @param pass2varlist A list containing the names of exported objects. This may be required if \code{cl} is supplied. This is passed to the \code{varlist} argument of \code{\link[parallel]{clusterExport}}. Exported objects must be located in the global environment.
#' @param verbose A logical input which defines whether or not to display messages to the console detailing function progress.
#'
#' @details The function is designed to extract FVCOM predictions for only one variable at a time. This is because FVCOM outputs different variables should be located in different folders and may differ in dimension (2-dimensional versus 3-dimensional variables) or file type (usually files are .mat but fvcom.tbx can compute and save outputs as .rds files). To implement the function for multiple variables, implement the function sequentially or in parallel for each variable. Non integer hours or layers are not currently implemented. If you require predictions for non integer hours or layers, include each the lower and upper hour or layer in \code{dat} and then interpolate predictions after these are provided. It is important to emphasise that FVCOM outputs are saved in multiple files (one for each day) by necessity: they contain a lot of data. Therefore, there are memory limitations which constrain the amount of data that can be extracted and returned by \code{\link[fvcom.tbx]{extract}}. If you need to extract large numbers of predictions to compute summary statistics or plots, use \code{\link[fvcom.tbx]{explore_field_2d}} which implements an iterative approach which avoids this issue by loading files iteratively and only saving outputs computed from each file, rather than all the predictions for each file. In other cases, \code{\link[fvcom.tbx]{extract}} can be implemented iteratively, with the outputs saved after every iteration in separate files.
#'
#' @return The function returns a dataframe which includes FVCOM predictions for specified dates/hours/layers and mesh cells.
#'
#' @examples
#' #### Example (1) Implement extract() for a 2-dimensional variable
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
#' path <- system.file("WeStCOMS_files/tidal_elevation",
#'                     package = "fvcom.tbx", mustWork = TRUE)
#' path <- paste0(path, "/")
#' # Implement extract() for a 2-dimensional variable
#' ext <- extract(dat = dat,
#'                match_hour = match_hour,
#'                match_mesh = match_mesh,
#'                dir2load = path,
#'                extension = ".mat",
#'                verbose = TRUE)
#'
#' #### Example (2) Implement extract for a 3-dimensional variable
#' # Define dat$layer
#' dat$layer <- 1
#' # Define match_layer if necessary
#' # Define path
#' path <- system.file("WeStCOMS_files/temp",
#'                     package = "fvcom.tbx", mustWork = TRUE)
#' path <- paste0(path, "/")
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
           read_fvcom = function(con) R.matlab::readMat(con)$data,
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
      cat("fvcom.tbx::extract() called...\n")
      cat("... extract() step 1: Initial checks/processing of dat...\n")
    }

    #### Check dat has been provided correctly
    check_names(input = dat,
                req = c("date_name", "hour", "mesh_ID"),
                extract_names = colnames,
                type = all)
    has_name_layer <- ifelse(rlang::has_name(dat, "layer"), TRUE, FALSE)

    #### Exclude corrupt files
    dat <- exclude_corrupt(dat, corrupt)

    #### Exclude any dates without associated files
    dat <- exclude_unavailable(dat, dir2load, pattern = paste0("*", extension))

    #### Check directory
    dir2load <- check_dir(arg = "dir2load",
                          input = dir2load,
                          check_slash = TRUE)


    ########################################
    #### Implements required to load FVCOM files

    if(verbose) cat("... extract() step 2: Getting ready to load in FVCOM files...\n")

    #### Define indices to extract predictions
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
    if(has_name_layer){
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
    if(verbose) cat("... extract() step 3: Loading FVCOM files and adding model predictions to dat...\n")
    pls <- pbapply::pblapply(dat_ls, cl = cl, FUN = function(df){

      #### Define connection and load file
      con <- paste0(dir2load, df$date_name[1], extension)
      if(verbose) cat(paste0("\n Loading file ", df$date_name[1], extension, "...\n"))
      wc <- read_fvcom(con)

      #### Extract values from fvcom depending on whether or not the inputted variable is 2-dimensional or 3-dimensional
      if(length(dim(wc)) == 3){
        stopifnot(!is.null(df$layer))
        df$wc <- wc[cbind(df$index_hour, df$index_layer, df$index_mesh)]
      } else if(length(dim(wc)) == 2){
        df$wc <- wc[cbind(df$index_hour, df$index_mesh)]
      } else{
        stop("2-dimensional or 3-dimensional matrix should be supplied")
      }

      #### Return predictions
      return(df)

    })
    if(!is.null(cl)) parallel::stopCluster(cl)

    #### Define dataframe of predictions
    pdat <- dplyr::bind_rows(pls)
    pdat$key <- paste0(pdat$date_name, "-", pdat$index_hour, "-", pdat$index_mesh, "-", pdat$index_layer)
    dat$key  <- paste0(dat$date_name, "-", dat$index_hour, "-", dat$index_mesh, "-", dat$index_layer)
    dat$wc   <- pdat$wc[match(dat$key, pdat$key)]
    dat$key  <- NULL

    #### End time
    t2 <- Sys.time()
    tdiff <- round(difftime(t2, t1))
    if(verbose) cat(paste0("fvcom.tbx::extract() algorithm duration approximately ", round(tdiff), " ",  methods::slot(tdiff, "units"), ".\n"))

    #### Return dataframe
    return(dat)

  }

#### End of code.
###########################################
###########################################
