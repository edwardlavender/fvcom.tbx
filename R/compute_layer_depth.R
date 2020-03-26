#' @title Compute the depth of WeStCOMS layers at specified location(s) and time(s)
#' @description In WeStCOMS, three-dimensional hydrodynamic conditions are predicted for each node (or element) at 11 Sigma layers (or between these layers) for every hour of every day. The approximate depth of each layer depends on the of the seabed below mean sea level (in a given location), the tidal elevation (in a given location and at a given time) and a constant which adjusts this depth for each layer, according to the equation \eqn{s_l \times (h_n + el_{n, t})} where \eqn{s} is a constant multiplier for layer \eqn{l}, \code{h} is the depth of node \code{n} below mean sea level and el us the tidal elevation{n, t} predicted at that node at hour \code{t}. If values for \eqn{h_n} and \eqn{el_{n, t}} are already known, \code{\link[WeStCOMSExploreR]{calc_layer_depth}} calculates the depths of the layers accordingly. Otherwise, \code{compute_depth_layer()} first extracts necessary parameters for these variables and then uses these to calculate the depths of all layers specified in \code{siglev} at specified times and locations. To implement \code{compute_depth_layer()}, the user must supply a dataframe which contains the times (dates and hours) for which depths should be calculated, as well as a vector of constants (\code{siglev}) which are used to calculate the depths of corresponding layers. If requested, the function can use the computed depths of each layer to assign user-supplied depths Sigma layers IDs using nearest neighbour interpolation or fractional layer IDs using linear interpolation.

#' @param dat 	A dataframe which defines the WeStCOMS file date names, hours, and mesh (node) IDs for which layer depths should be calculated. Columns should be named 'date_name', 'hour' and 'mesh_ID' respectively. The dataframe may contain column named 'depth', containing (absolute depths, m), if you want to assign layers IDs to depth observations (see Description). The dataframe should be arranged by 'date_name'.
#' @param h A dataframe which, for each mesh ID, defines the (absolute) depth of the seabed in that cell below mean sea level. Columns should be named 'ID' and 'h' respectively.
#' @param siglev A dataframe which, for each Sigma layer, defines the (absolute value of) siglev constant. Columns should be named 'layer' and 'siglev' respectively.
#' @param match_hour A dataframe with two integer columns named 'hour' and 'index' which defines the index in WeStCOMS files (i.e. the row) which corresponds to each hour. The default dataframe is usually appropriate, if WeStCOMS files have not been subsetted. However, if WeStCOMS files have been subsetted (e.g. by selecting rows corresponding to hours 12 and 13), then rows 1 and 2 in WeStCOMS files now represent hours 12 and 13, not hours 0 and 1. \code{match_hour} provides the link which ensures that data for specified hours (e.g. hours 12 and 13) are correctly extracted from a WeStCOMS array (see Examples). All WeStCOMS files are assumed to have the same structure.
#' @param match_mesh (optional) A dataframe with two columns named 'mesh' and 'index' which defines the index in WeStCOMS files (columns or sheets for 2d and 3d arrays respectively) which corresponds to each mesh cell. This only needs to be provided if you are working with a subset of WeStCOMS files: in this situation, mesh IDs 5, 6, 7, for example, may not correspond to index 5, 6, 7 in WeStCOMS files. \code{match_mesh} provides the link which ensures that WeStCOMS predictions are extracted correctly (see Examples). All WeStCOMS files are assumed to have the same structure.
#' @param dir2load A string which defines the directory from which to load WeStCOMS files containing tidal elevation predictions (required to calculate depth). In this directory, WeStCOMS file names are assumed to follow the standard naming convention (i.e., yymmdd; see \code{\link[WeStCOMSExploreR]{date_name}}. All files with the pattern \code{*extension}, see \code{extension} are assumed to be WeStCOMS files.
#' @param extension A string which defines the extension of the WeStCOMS files. The default is \code{".mat"}.
#' @param corrupt A vector of numbers, representing WeStCOMS date names, which define corrupt files. These will not be loaded.
#' @param cl (optional) A cluster objected created by the parallel package. If supplied, the algorithm is implemented in parallel. Note that the connection with the cluster is stopped within the function.
#' @param pass2varlist A list of character vector of names of objects to export to be passed to the \code{varlist} argument of \code{\link[parallel]{clusterExport}}.
#' @param assign_layer A logical input that defines whether or not layer IDs should be assigned to observed depths, based on the computed depths of Sigma layers. This method requires a column named 'depth' in \code{dat}.
#' @param assign_layer_method A character hich defines the method by which layer IDs should be assigned to observed depths, if \code{assign_layer = TRUE}. Implemented options are \code{"nearest"} or \code{"fractional"}. If \code{assign_layer_method = "nearest"}, then each depth observation is assigned the layer ID of the nearest node. If \code{assign_layer_method = "fractional"}, a fractional layer ID is computed based on the depths of the two nodes which surround the observed depth.
#' @param warning_threshold A number which defines the number of metres beyond the approximate depth of the seabed below mean sea level (i.e., \code{h$h}, see above) after which the function will warn the user if there are any deeper depth observations. This may be due to inaccuracies in recorded depths or locations. However, note that in areas of complex bathymetry, there may well be areas in each mesh cell that are much deeper than the depth of the WeStCOMS layer at the seabed. This is why a \code{warning_threshold} is provided.
#' @param verbose A logical input which defines whether or not to display messages to the console detailing function progress.
#'
#' @details The function currently only supports nearest neighbour interpolation, extracting tidal predictions for specified integer hours. To calculate layer depths for non integer hours, a custom approach is necessary.
#'
#' @return The function returns a dataframe, as inputted but with the following columns added: 'index_hour', the row in the WeStCOMS files which corresponds to the inputted hour; 'index_mesh', the column in the WeStCOMS files which corresponds to the inputted mesh_ID; 'h', the depth (m) of the seabed in that cell below mean sea level; el, the tidal elevation on the inputted hour and for the inputted mesh cell, columns 'l1',... 'ln' where 'n' is the deepest layer in the inputted \code{siglev} dataframe, containing the depths (m) of each layer at the specified location(s) and time(s). If \code{assign_layer = TRUE}, a column, 'layer_ID', is also returned; this is the ID of the layer assigned to each inputted depth observation. This column has one or more attributes: "method" is "nearest" or "fractional" and, if \code{assign_layer_method = "fractional"}, then the column also has the "computational_details" attribute which returns the information required to compute fractional layer IDs, such as the IDs of the surrounding layers, their depths and the weights applied to each of these to compute the fractional layer ID (see Examples).
#'
#' @examples
#'
#' #### Define dat
#' # Imagine we want to compute the depths of layers at the following timestamps
#' timestamp <- as.POSIXct(c("2016-03-01 00:00:00",
#'                           "2016-03-01 01:00:00",
#'                           "2016-03-02 00:00:00",
#'                           "2016-03-02 01:00:00"), tz = "UTC")
#' # For all of these nodes:
#' nodes <- as.integer(as.character(dat_mesh_around_nodes$ID))
#' dat <- expand.grid(timestamp = timestamp, mesh_ID = nodes)
#' # dat should be arranged by timestamp
#' dat <- dplyr::arrange(dat, timestamp)
# Add necessary columns
#' dat$date_name <- date_name(dat$timestamp, define = "date_name")
#' dat$hour <- lubridate::hour(dat$timestamp)
#' head(dat)
#'
#' #### Define other function inputs
#' h <- dat_h; colnames(h) <- c("ID", "h")
#' match_mesh <- data.frame(mesh = dat_nodexy$node_id, index = 1:length(dat_nodexy$node_id))
#' dir2load <- system.file("WeStCOMS_files/tidal_elevation/",
#'                         package = "WeStCOMSExploreR", mustWork = TRUE)
#'
#' #### Example (1) Compute layer depths for all layers using default options
#' dol1 <- compute_layer_depth(dat = dat,
#'                             h = h,
#'                             siglev = dat_siglev,
#'                             match_hour = data.frame(hour = 0:23, index = 1:24),
#'                             match_mesh = match_mesh,
#'                             dir2load = dir2load,
#'                             extension = ".mat",
#'                             cl = NULL,
#'                             pass2varlist = NULL
#' )
#' # Examine outputs
#' head(dol1, 3)
#' tail(dol1, 3)
#' # The depths of h and layer 10 (the seabed) should be similar:
#' dol1$h_from_h <- h$h[match(dat$mesh_ID, h$ID)]
#' head(dol1[, c("mesh_ID", "h", "h_from_h", "l10")])
#'
#' #### Example (2) Incorporate parallel loading of tidal elevation files
#' dol2 <- compute_layer_depth(dat = dat,
#'                             h = h,
#'                             siglev = dat_siglev,
#'                             match_hour = data.frame(hour = 0:23, index = 1:24),
#'                             match_mesh = match_mesh,
#'                             dir2load = dir2load,
#'                             extension = ".mat",
#'                             cl = parallel::makeCluster(2L),
#'                             pass2varlist = NULL
#' )
#' head(dol2, 3)
#'
#' #### Example (3) Assign observed depths to the nearest layer
#' # Imagine we have observations at the following depths.
#' # (Here, the use of match is simply to constrain hypothetical depths
#' # ... to be below the maximum depth of the seabed)
#' dat$depth <- h$h[match(dat$mesh_ID, h$ID)] - 5
#' dol3 <- compute_layer_depth(dat = dat,
#'                             h = h,
#'                             siglev = dat_siglev,
#'                             match_hour = data.frame(hour = 0:23, index = 1:24),
#'                             match_mesh = match_mesh,
#'                             dir2load = dir2load,
#'                             extension = ".mat",
#'                             assign_layer = TRUE,
#'                             assign_layer_method = "nearest"
#' )
#' head(dol3)
#' table(dol3$layer_ID)
#' # The layer_ID column contains the "method" attribute = "nearest"
#' attributes(dol3$layer_ID)
#'
#' #### Example (4) Assign observed depths fractional layer IDs
#' # ... based on the depths of surrounding layers
#' dol4 <- compute_layer_depth(dat = dat,
#'                             h = h,
#'                             siglev = dat_siglev,
#'                             match_hour = data.frame(hour = 0:23, index = 1:24),
#'                             match_mesh = match_mesh,
#'                             dir2load = dir2load,
#'                             extension = ".mat",
#'                             assign_layer = TRUE,
#'                             assign_layer_method = "fractional"
#' )
#' head(dol4)
#' range(dol4$layer_ID)
#' # The layer_ID column contains the "method" attribute and the "computation_details" attribute
#' # ... The latter is a dataframe containing information needed to calculate fractional layer IDs.
#' str(attributes(dol4$layer_ID))
#' # For the computational_methods attribute, columns are as follows:
#' # layer_nearest: the ID of the nearest layer
#' # layer_2ndnearest: the ID of the other surrounding layer
#' # depth_nearest: the depth of the nearest layer
#' # depth_2ndnearest: the depth of the other surrounding layer
#' # diff1: the absolute difference in depth between the observation and the first layer
#' # diff2: the absolute difference in depth between the observation and the second layer
#' # w1: the weight applied to the depth of the first layer
#' # w2 the weight applied to the depth of the second layer
#' # diff_layer: the absolute difference in depth between the two layers
#'
#' #### Example (5) When layer IDs are being assigned to depths, potentially erroneous depth
#' # ... observations (i.e., observations deeper than the seabed) produce a warning:
#' \dontrun{
#' dat$depth[1:10] <- dat$depth[1:10] + 30
#' dol5 <- compute_layer_depth(dat = dat,
#'                             h = h,
#'                             siglev = dat_siglev,
#'                             match_hour = data.frame(hour = 0:23, index = 1:24),
#'                             match_mesh = match_mesh,
#'                             dir2load = dir2load,
#'                             extension = ".mat",
#'                             assign_layer = TRUE,
#'                             assign_layer_method = "fractional",
#'                             warning_threshold = 0
#' )
#' head(dol5)
#' }
#'
#' @author Edward Lavender
#' @export
#'

#######################################
#######################################
#### compute_layer_depth()

compute_layer_depth <-
  function(dat,
           h,
           siglev,
           match_hour = data.frame(hour = 0:23, index = 1:24),
           match_mesh = NULL,
           dir2load,
           extension = ".mat",
           corrupt = NULL,
           cl = NULL,
           pass2varlist = NULL,
           assign_layer = FALSE,
           assign_layer_method = "nearest",
           warning_threshold = 0,
           verbose = TRUE
           ){


    ########################################
    #### Check and process dat as required

    #### Algorithm start
    t1 <- Sys.time()
    if(verbose) cat("Step 1: Initial checks/processing of dat...\n")

    #### Check data has been provided correctly
    stopifnot(all(c("date_name", "hour", "mesh_ID") %in% colnames(dat)),
              all(c("ID", "h") %in% colnames(h)),
              all(c("layer", "siglev") %in% colnames(siglev))
              )

    #### Use absolute values for depths (siglev, dat$depth)
    # This simplifies later calculations.
    siglev$siglev <- abs(siglev$siglev)
    if(!is.null(dat$depth)) abs(dat$depth)

    #### Exclude corrupt files
    if(!is.null(corrupt)){
      pos_corrupt <- which(dat$date_name %in% corrupt)
      if(length(pos_corrupt) > 0){
        warning(paste(length(pos_corrupt), "obserations associated with corrupt files excluded. \n"))
        dat <- dat[-c(pos_corrupt), ]
      }
    }

    #### Exclude any dates without associated files
    files <- list.files(dir2load, pattern = paste0("*", extension))
    stopifnot(length(files) > 0)
    file_codes <- as.numeric(substr(files, 1, 6))
    pos_unavailable <- which(!(dat$date_name %in% file_codes))
    if(length(pos_unavailable) > 0){
      warning(paste(length(pos_unavailable), "obserations with unavailable predictions excluded. \n"))
      dat <- dat[-c(pos_unavailable), ]
    }

    #### Check that observations remain in dat
    stopifnot(nrow(dat) > 0)

    #### dat should be arranged by date_name
    # (This is to avoid issues when we add tidal elevations back to the dataframe,
    # ... having computed them for each element in a list)
    stopifnot(!is.unsorted(dat$date_name))


    ########################################
    #### Implements required to load FVCOM files

    if(verbose) cat("Step 2: Getting ready to load in FVCOM files to extract tidal elevation(s)...\n")

    #### Define indices to extract tidal elevation values
    # ... (required to compute depth of layers)
    dat$index_hour <- match_hour$index[match(dat$hour, match_hour$hour)]
    if(!is.null(match_mesh)){
      dat$index_mesh <- match_mesh$index[match(dat$mesh_ID, match_mesh$mesh)]
    } else{
      dat$index_mesh <- dat$mesh_ID
    }

    #### Extract depth of nodes below mean sea level
    if(class(dat$mesh_ID) != "integer"){
      warning("class(dat$mesh_ID) converted to integer.")
      dat$mesh_ID <- as.integer(as.character(dat$mesh_ID))
    }
    if(class(h$ID) != "integer"){
      warning("class(h$ID) converted to integer.")
      dat$mesh_ID <- as.integer(as.character(dat$mesh_ID))
    }
    dat$h <- h$h[match(dat$mesh_ID, h$ID)]

    #### Define a list with one element for each unique date_name
    dat_ls <- split(dat, f = dat$date_name)

    #### Set up paralelisation if required
    if(!is.null(cl) & !is.null(pass2varlist)){
      varlist <- pass2varlist
      parallel::clusterExport(cl = cl, varlist = varlist)
    }


    ########################################
    #### Load FVCOM files

    #### Loop over every date_name and extract tidal heights for mesh IDs/hours of interest
    if(verbose) cat("Step 3: Loading FVCOM files to obtain tidal elevation(s)...\n")
    els <- pbapply::pblapply(dat_ls, cl = cl, FUN = function(d){

      #### Load tidal elevation values for selected date
      # (these are needed to calculate depths of layers)
      tidal_elevation <- R.matlab::readMat(paste0(dir2load, d$date_name[1], extension))
      tidal_elevation <- tidal_elevation$data

      #### Extract tidal elevation (needed to compute depth)
      el <- tidal_elevation[cbind(d$index_hour, d$index_mesh)]

      #### Return tidal elevation
      return(el)

    })
    if(!is.null(cl)) parallel::stopCluster(cl)


    #### Add tidal elevations to dataframe
    dat$el <- as.numeric(unlist(els))


    ########################################
    #### Compute depths

    #### Use tidal elevation to compute depths of layers
    # ... at specified times and in specified locations
    # ... We'll generate a list, with one element for each layer
    if(verbose) cat("Step 4: Computing depths...\n")
    siglev_ls <- split(siglev, f = siglev$layer)
    depth_layers_ls <- pbapply::pblapply(siglev_ls, function(sig){
      x <- sig$siglev * (dat$h + dat$el)
      d <- data.frame(x)
      colnames(d) <- paste0("l", sig$layer[1])
      # attr(d, "layer") <- sig$layer[1]
      # attr(d, "siglev") <- sig$siglev
      return(d)
    })

    #### Add columns to dat
    depth_cols <- dplyr::bind_cols(depth_layers_ls)
    dcmat <- as.matrix(depth_cols)
    dat[, colnames(depth_cols)] <- depth_cols


    ########################################
    #### Assign layer IDs

    #### Assign layer to each observation if requested
    if(assign_layer){

      #### Checks
      if(verbose) cat("Step 5: Assigning layer IDs to observed depths based on Sigma layer depths...\n")
      stopifnot(!is.null(dat$depth))
      if(!(assign_layer_method %in% c("nearest", "fractional"))){
        warning("Input to assign_layer_method not implemented. Options are 'nearest' or 'fractional'. Defaulting to 'nearest'.")
      }

      #### Identify nearest layer
      # (This is required for both assign_method == "nearest" and "fractional")
      dmat <- abs(dat$depth - dat[, colnames(depth_cols)])
      which_min <- apply(dmat, 1, which.min)
      dat$layer_nearest <- siglev$layer[which_min]
      if(assign_layer_method == "nearest") {
        colnames(dat)[which(colnames(dat) %in% "layer_nearest")] <- "layer_ID"
        attr(dat$layer_ID, "method") <- "nearest"
      }


      #### Define fractional layer number
      # (This is useful if observatiouns lie between layers, which will usually be the case).
      if(assign_layer_method == "fractional"){

        #### Method
        # Identify two nearest layers
        # Extract the depths of the two nearest layers
        # Calculate the different in depth between the inputted depth and the depth of the two nearest layers
        # Compute a weighted average of the two nearest layer numbers to define a fractional layer

        #### Define nearest layers and associated depths
        dat$depth_nearest <- dcmat[cbind(1:nrow(dcmat), which_min)]
        # dat$depth_nearest <- abs(dat$depth_nearest)
        # The second layer to choose depends on whther or not the observation is above or below
        # ... the nearest layer:
        dat$layer_2ndnearest <- ifelse(abs(dat$depth) < dat$depth_nearest, dat$layer - 1, dat$layer + 1)
        # Ensure that layer_2ndnearest cannot be < 1 or > 11:
        pos2low <- which(dat$layer_2ndnearest < 1)
        if(length(pos2low) > 0) dat$layer_2ndnearest[pos2low] <- 1
        pos2high <- which(dat$layer_2ndnearest > 11)
        if(length(pos2high) > 0) dat$layer_2ndnearest[pos2high] <- 11
        # Define depth of 2ndnearest layer:
        dat$depth_2ndnearest <- dcmat[cbind(1:nrow(dcmat), dat$layer_2ndnearest)]
        # dat$depth_2ndnearest <- abs(dat$depth_2ndnearest)

        #### Compute weights to calculate fractional layer ID
        dat$diff1 <- abs(dat$depth - dat$depth_nearest)
        dat$diff2 <- abs(dat$depth - dat$depth_2ndnearest)
        dat$diff_layer <- abs(dat$depth_nearest - dat$depth_2ndnearest)
        dat$w1 <- abs(dat$diff_layer - dat$diff1)/dat$diff_layer
        dat$w2 <- abs(dat$diff_layer - dat$diff2)/dat$diff_layer

        #### Compute weighted average of layer numbers to define fractional layer ID
        dat$layer_ID <- dat$layer_nearest * dat$w1 + dat$layer_2ndnearest * dat$w2

        #### Adjustments in case the gap between layers is 0
        # ... i.e., observations lies on the boundary
        # ... or observation is far beyond boundary (which will produce a warning below)
        pos_equal <- which(dat$diff_layer == 0)
        if(length(pos_equal) > 0){
          dat$w1[pos_equal] <- NA
          dat$w2[pos_equal] <- NA
          dat$layer_ID[pos_equal] <- dat$layer_nearest[pos_equal]

          #### Check for erroneous fractional depths
          if(any(dat$layer_ID < 1) | any(dat$layer_ID > 11)){
            warning("Incorrect dat$layer_ID values < 1 or > 11 computed. Check whether depth observations are reasonable given maximum available depths in each cell.")
          }
        }

        #### Remove uneeded columns to simplify outputs and convert these to attributes instead
        attr(dat$layer_ID, "method") <- "fractional"
        cols2attr <- c("layer_nearest", "layer_2ndnearest",
                      "depth_nearest", "depth_2ndnearest",
                      "diff1", "diff2",
                      "w1", "w2",
                      "diff_layer")
        dat2attr <- dat[, cols2attr]
        attr(dat$layer_ID, "computation_details") <- dat2attr
        dat[, c(cols2attr)] <- NULL
      }

    }

    #### Warn the user if there are observations deeper than the depth of the seabed below mean sea level (+ a warning threshold)
    if(!is.null(warning_threshold)){
        pos2warn <- which(dat$depth > (dat$h + warning_threshold))
        if(length(pos2warn) > 0) {
          warning(paste(length(pos2warn), "depth observations deeper than the depth of the seabed below mean sea level (+ the warning threshold, warning_threshold)."))
        }
    }

    #### End time
    t2 <- Sys.time()
    tdiff <- round(difftime(t2, t1))
    if(verbose) cat(paste0("Algorithm duration approximately ", round(tdiff), " ",  methods::slot(tdiff, "units"), ".\n"))

    #### Return dataframe
    return(dat)

  }

#### End of code.
#######################################
#######################################
