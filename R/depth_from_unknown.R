#' @title Compute the depth of Sigma layers at specified location(s) and time(s)
#' @description In FVCOM, three-dimensional hydrodynamic conditions are predicted for each node (or element) at 11 Sigma layers (or between these layers) for every hour of every day. The depth of each layer depends on the depth of the seabed below mean sea level (in a given location), the tidal elevation (in a given location and at a given time) and a constant which adjusts this depth for each layer, according to the equation \eqn{depth_{t, l, n} = s_{l} \times (h_n + el_{n, t})} where \eqn{s} is a constant multiplier for layer \eqn{l}, \code{h} is the depth of node \code{n} below mean sea level and \code{el} is the tidal elevation predicted at that node at hour \code{t}. If values for \eqn{h_n} and \eqn{el_{n, t}} are already known, \code{\link[WeStCOMSExploreR]{depth_from_known}} calculates the depths of the layers accordingly. Otherwise, \code{\link[WeStCOMSExploreR]{depth_from_unknown}} first extracts necessary parameters for these variables using \code{\link[WeStCOMSExploreR]{extract}} and then uses these to calculate the depths of all layers specified in \code{siglev} at specified times and locations. To implement (see \code{\link[WeStCOMSExploreR]{depth_from_unknown}}), the user must supply a dataframe which contains the times (dates and hours) for which depths should be calculated, as well as a vector of constants (\code{siglev}) which are used to calculate the depths of corresponding layers. If requested, the function can use the computed depths of each layer to assign user-supplied depths Sigma layer IDs using nearest neighbour interpolation or fractional layer IDs using linear interpolation.

#' @param dat 	A dataframe which defines the FVCOM file date names, hours, and mesh (node) IDs for which layer depths should be calculated (see \code{\link[WeStCOMSExploreR]{extract}}). The dataframe may contain column named 'depth', containing (absolute) depths (m), if you want to assign layers IDs to depth observations (see Description). The dataframe may also contain a column named 'layer' if you want to compute the depths of a particular layer at each row in \code{dat}, rather than all layers in \code{siglev} (see below). The dataframe should be arranged by 'date_name'.
#' @param h A dataframe which, for each mesh ID, defines the (absolute) depth of the seabed in that cell below mean sea level. Columns should be named 'ID' and 'h' respectively.
#' @param siglev A dataframe which, for each Sigma layer, defines the (absolute value of the) siglev constant. Columns should be named 'layer' and 'siglev' respectively.
#' @param match_hour A dataframe with two integer columns named 'hour' and 'index' which defines the index in FVCOM arrays (i.e. the row) which corresponds to each hour (see \code{\link[WeStCOMSExploreR]{extract}}).
#' @param match_mesh (optional) A dataframe with two columns named 'mesh' and 'index' which defines the index in FVCOM arrays (columns or sheets for 2- and 3-dimensional arrays respectively) which corresponds to each mesh cell (see \code{\link[WeStCOMSExploreR]{extract}}).
#' @param corrupt A vector of numbers, representing WeStCOMS date names, which define corrupt files (see \code{\link[WeStCOMSExploreR]{extract}}).
#' @param read_fvcom A function which is used to load files (see \code{\link[WeStCOMSExploreR]{extract}}).
#' @param dir2load A string which defines the directory from which to load the FVCOM arrays that contain the tidal elevation predictions required to calculate depth (see \code{\link[WeStCOMSExploreR]{extract}}).
#' @param extension A string which defines the extension of the FVCOM arrays (see \code{\link[WeStCOMSExploreR]{extract}}).
#' @param cl (optional) A cluster objected created by the parallel package (see \code{\link[WeStCOMSExploreR]{extract}}).
#' @param pass2varlist A list containing the names of exported objects. This may be required if \code{cl} is supplied. This is passed to the \code{varlist} argument of \code{\link[parallel]{clusterExport}}. Exported objects must be located in the global environment (see \code{\link[WeStCOMSExploreR]{extract}}).
#' @param depth_of_specified A logical input that defines whether or not to calculate the depths of all layers in \code{siglev} for each row in \code{dat} (\code{depth_of_specified = FALSE}), or just the depths of specified layers, specified via \code{dat$layer} (\code{depth_of_specified = TRUE}).
#' @param assign_layer A logical input that defines whether or not layer IDs should be assigned to observed depths, based on the computed depths of Sigma layers. This method requires a column named 'depth' in \code{dat}.
#' @param assign_layer_method A character which defines the method by which layer IDs should be assigned to observed depths, if \code{assign_layer = TRUE}. Implemented options are \code{"nearest"} or \code{"fractional"}. If \code{assign_layer_method = "nearest"}, then each depth observation is assigned the layer ID of the nearest node. If \code{assign_layer_method = "fractional"}, a fractional layer ID is computed based on the depths of the two nodes which surround the observed depth.
#' @param warning_threshold A number which defines the number of metres beyond the approximate depth of the seabed below mean sea level (i.e., \code{h$h}, see above) after which the function will warn the user if there are any deeper depth observations. This may be due to inaccuracies in recorded depths or locations. However, note that in areas of complex bathymetry, there may well be areas in each mesh cell that are much deeper than the depth of the WeStCOMS layer at the seabed. This is why a \code{warning_threshold} is provided.
#' @param verbose A logical input which defines whether or not to display messages to the console detailing function progress.
#'
#' @details The function currently only supports nearest neighbour interpolation, extracting tidal predictions for specified integer hours. To calculate layer depths for non integer hours, a custom approach is necessary.
#'
#' @return The function returns a dataframe, as inputted but with the following columns added: 'index_hour', the row in the FVCOM files which corresponds to the inputted hour; 'index_mesh', the column in the FVCOM files which corresponds to the inputted mesh_ID; 'h', the depth (m) of the seabed in that cell below mean sea level; and 'el', the tidal elevation on the inputted hour and for the inputted mesh cell. If \code{depth_of_specified = TRUE} and \code{dat$depth} is supplied, then the dataframe also contains 'siglev', the siglev value for the specified layers and 'depth_layer', the depth of each inputted layer. Otherwise, the depth of every layer specified in \code{siglev} are provided in columns named 'l1',... 'ln' where 'n' is the deepest layer in the inputted \code{siglev} dataframe. If \code{assign_layer = TRUE}, a column, 'layer_ID', is also returned; this is the ID of the layer assigned to each inputted depth observation. This column has one or more attributes: 'method' is "nearest" or "fractional" and, if \code{assign_layer_method = "fractional"}, then the column also has the "computational_details" attribute which returns the information required to compute fractional layer IDs, such as the IDs of the surrounding layers, their depths and the weights applied to each of these to compute the fractional layer ID (see Examples).
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
#' dol1 <- depth_from_unknown(dat = dat,
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
#' dol2 <- depth_from_unknown(dat = dat,
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
#' dol3 <- depth_from_unknown(dat = dat,
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
#' dol4 <- depth_from_unknown(dat = dat,
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
#' dol5 <- depth_from_unknown(dat = dat,
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
#' #### Example 6) Compute the depths of specified layers with depth_of_specified = TRUE,
#' # ... rather than all layers in siglev.
#' set.seed(1)
#' dat$layer <- sample(1:10, nrow(dat), replace = TRUE)
#' dol6 <- depth_from_unknown(dat = dat,
#'                             h = h,
#'                             siglev = dat_siglev,
#'                             match_hour = data.frame(hour = 0:23, index = 1:24),
#'                             match_mesh = match_mesh,
#'                             dir2load = dir2load,
#'                             depth_of_specified = TRUE
#' )
#' utils::head(dol6, 3)
#'
#' #### Example (7) Compute the depths of specified layers and assign layer_IDs to 'observed' depths
#' dat$depth <- 1
#' set.seed(1)
#' dat$layer <- sample(1:10, nrow(dat), replace = TRUE)
#' dol7 <- depth_from_unknown(dat = dat,
#'                             h = h,
#'                             siglev = dat_siglev,
#'                             match_hour = data.frame(hour = 0:23, index = 1:24),
#'                             match_mesh = match_mesh,
#'                             dir2load = dir2load,
#'                             depth_of_specified = TRUE,
#'                             assign_layer = TRUE,
#'                             assign_layer_method = "fractional"
#' )
#' utils::head(dol7, 3)
#' # dat$depth_layer is the depth of dat$layer
#' # dat$layer_ID is the layer ID assigned to the 'observed' depth (dat$depth)
#'
#' @author Edward Lavender
#' @export
#'

#######################################
#######################################
#### depth_from_unknown()

depth_from_unknown <-
  function(dat,
           h,
           siglev,
           match_hour = data.frame(hour = 0:23, index = 1:24),
           match_mesh = NULL,
           corrupt = NULL,
           read_fvcom = function(con) R.matlab::readMat(con)$data,
           dir2load,
           extension = ".mat",
           cl = NULL,
           pass2varlist = NULL,
           depth_of_specified = FALSE,
           assign_layer = FALSE,
           assign_layer_method = "nearest",
           warning_threshold = 0,
           verbose = TRUE
           ){


    ########################################
    #### Initial checks

    #### Algorithm start
    t1 <- Sys.time()
    if(verbose) {
      cat("WeStCOMSExploreR::depth_from_unknown() called...\n")
      cat("Step 1: Implementing initial checks...\n")
    }

    #### Check h and siglev have been provided correctly
    stopifnot(all(c("ID", "h") %in% colnames(h)),
              all(c("layer", "siglev") %in% colnames(siglev))
              )

    #### Use absolute values for depths (siglev, dat$depth)
    # This simplifies later calculations.
    siglev$siglev <- abs(siglev$siglev)
    if(!is.null(dat$depth)) abs(dat$depth)

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

    #### dat should be arranged by date_name
    # (This is to avoid issues when we add tidal elevations back to the dataframe,
    # ... having computed them for each element in a list)
    stopifnot(!is.unsorted(dat$date_name))

    #### Other processing implemented internally by extract()]
    # Check column names in dat
    # Exclude corrupt files
    # Exclude dates without any dates without associated files


    ########################################
    #### Extract tidal predictions with extract()

    #### Use extract() to extract tidal heightrs
    if(verbose) cat("Step 2: Calling WeStCOMSExploreR::extract() to extract tidal elevation(s)...\n")
    dat <- extract(dat,
                   match_hour = match_hour,
                   match_mesh = match_mesh,
                   match_layer = NULL,
                   corrupt = corrupt,
                   read_fvcom = read_fvcom,
                   dir2load = dir2load,
                   extension = extension,
                   cl = cl,
                   pass2varlist = pass2varlist,
                   verbose = verbose)
    dat$el <- dat$wc
    dat$wc <- NULL


    ########################################
    #### Compute depths

    #### Use tidal elevation to compute depths of layers
    # ... at specified times and in specified locations
    # ... We'll generate a list, with one element for each layer
    if(verbose) cat("Step 3: Computing depths...\n")

    #### If the user has requested depth for inputted layers,
    # we'll define these, add them to the dataframe and return.
    if(depth_of_specified){
      stopifnot(!is.null(dat$layer))
      dat$siglev <- siglev$siglev[match(dat$layer, siglev$layer)]
      if("depth_layer" %in% colnames(dat)){
        warning("dat$depth_layer column overwritten. \n")
      }
      dat$depth_layer <- dat$siglev * (dat$h + dat$el)
    }

    #### Continue to define the depths of all layers
    # either because depth_of_specified is false
    # or depth_of_specified is TRUE and assign_layer is TRUE
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
      if(verbose) cat("Step 4: Assigning layer IDs to observed depths based on Sigma layer depths...\n")
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

      #### Warn the user if there are observations deeper than the depth of the seabed below mean sea level (+ a warning threshold)
      if(!is.null(warning_threshold)){
        pos2warn <- which(dat$depth > (dat$h + warning_threshold))
        if(length(pos2warn) > 0) {
          warning(paste(length(pos2warn), "depth observations deeper than the depth of the seabed below mean sea level (+ the warning threshold, warning_threshold)."))
        }
      }

    }

    #### Exclude excess columns if depth_of_specified
    if(depth_of_specified){
      dat[, colnames(depth_cols)] <- NULL
    }

    #### End time
    t2 <- Sys.time()
    tdiff <- round(difftime(t2, t1))
    if(verbose) cat(paste0("WeStCOMSExploreR::depth_from_unknown() algorithm duration approximately ", round(tdiff), " ",  methods::slot(tdiff, "units"), ".\n"))

    #### Return dataframe
    return(dat)

  }

#### End of code.
#######################################
#######################################
