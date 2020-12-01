#' @title Validate FVCOM predictions with observations
#'
#' @description For specified location(s), layer(s) and timestamp(s), this function pairs observations with corresponding FVCOM predictions. Locations and observations can be derived from different datasets. This means that animal movement datasets, in which locations and observations may be derived from different electronic tags, can be used for validation. The function uses nearest neighbour interpolation to extract FVCOM predictions for the nearest hour, at the specified layer and at the nearest mesh cell for which predictions are available relative to user inputs. Paired observations and predictions can be used to examine model skill (i.e., prediction accuracy).
#'
#' @param dat_obs1 A dataframe which contains the location(s), layer(s) and timestamp(s) at which environmental observations have been made. These should be defined in columns named 'lat' and 'long', 'location', 'layer' and 'timestamp' respectively. A column 'key' may also need to be included (see Details). Location(s) are assumed to be in World Geodetic System format (i.e. WGS 84). Observations can either be located in this dataframe, in a column called 'obs', or in a separate dataframe, (\code{dat_obs2}), see below.
#' @param dat_obs2 A dataframe which contains the environmental observations which will be used to validate model predictions. This must contain timestamps ('timestamp'), observations ('obs'). A column 'key' may also need to be included (see Details). Note that observations can be provided in \code{dat_obs1} but, for some validation datasets, observations and locations are in separate datasets (see Details). \code{dat_obs2} allows for this flexibility.
#' @param threshold_match_gap A numeric input which defines the duration (s) between the timestamp of a known location and that of a corresponding observation before/after which these timestamps in \code{dat_obs1} are removed. This is useful if \code{dat_obs2} is provided and if observations are only available for a sample of the timestamps at which locations are known. In this scenario, matching observations via the nearest timestamp may be inappropriate because there may be long gaps between the times of known locations and observations. In this case, all of the 'nearest' observation(s) that are more than \code{threshold_match_gap} s away (either before or after) the required timestamp(s) are removed.
#' @param mesh A \code{\link[sp]{SpatialPolygonsDataFrame-class}} object which defines the WeStCOMS mesh created by \code{\link[fvcom.tbx]{build_mesh}}. The coordinate reference system for the mesh should match inputted coordinates (e.g., lat, long) (see \code{dat_obs1}).
#' @param match_hour A dataframe with two integer columns named 'hour' and 'index' which defines the index in FVCOM arrays (i.e. the row) which corresponds to each hour (see \code{\link[fvcom.tbx]{extract}}).
#' @param match_layer A dataframe with two integer columns named 'layer' and 'index' which defines the index in FVCOM arrays (i.e. the column) which corresponds to each layer n (see \code{\link[fvcom.tbx]{extract}}).
#' @param match_mesh (optional) A dataframe with two columns named 'mesh' and 'index' which defines the index in FVCOM arrays (columns or sheets for 2d and 3d arrays respectively) which corresponds to each mesh cell (see \code{\link[fvcom.tbx]{extract}}).
#' @param corrupt A vector of numbers, representing WeStCOMS date names, which define corrupt files (see \code{\link[fvcom.tbx]{extract}}).
#' @param read_fvcom A function which is used to load files (see \code{\link[fvcom.tbx]{extract}}).
#' @param dir2load A string which defines the directory from which to load FVCOM arrays containing predictions (see \code{\link[fvcom.tbx]{extract}}).
#' @param extension A string which defines the extension of the FVCOM arrays (see \code{\link[fvcom.tbx]{extract}}).
#' @param cl (optional) A cluster objected created by the parallel package (see \code{\link[fvcom.tbx]{extract}}).
#' @param pass2varlist A list containing the names of exported objects. This may be required if \code{cl} is supplied. This is passed to the \code{varlist} argument of \code{\link[parallel]{clusterExport}}. Exported objects must be located in the global environment (see \code{\link[fvcom.tbx]{extract}}).
#' @param verbose A logical input which defines whether or not to display messages to the console detailing function progress.
#'
#' @details To use this function, the user must supply a dataframe (\code{dat_obs1}) which contains the locations(s) , layer(s) and time(s) (in a column named 'timestamp'). These columns must be named 'long' and 'lat', 'layer' and 'timestamp' respectively. A column, 'key', may also need to be included (see below). Location(s) are assumed to be in World Geodetic System format (i.e. WGS 84). Observations can either be located in this dataframe, in a column called 'obs', or in a separate dataframe (\code{dat_obs2}) with columns 'timestamp', 'obs' and 'key'. One situation where this latter option is useful is for animal movement data when the location of the animal is known from one tag type (e.g. passive acoustic telemetry) and observations are recorded by another tag type (e.g. an archival tag), possibly at a different resolution. In this scenario, both dataframes should contain a column, 'key', which connects the factor level(s) (e.g. individuals) for which locations observations have been made in the first dataframe with the factor level(s) for which environmental observations (which will be used to validate the model) have been made. In this case, a nearest neighbour matching approach is used to add observations into the first dataframe (\code{dat_obs1}) from the second dataframe (\code{dat_obs2}) by selecting those that occurred closest in time to the timestamps stipulated in the first dataframe (this is why is is important to have a 'key' to distinguish among factor levels). The function uses a mesh supplied by the user to determine the FVCOM mesh nodes/elements within which each location lies (i.e. nearest neighbour interpolation). With this information, the function loads in each FVCOM file from a user-defined directory, extracts the relevant model predictions from this file, and then returns the original dataframe with predictions added. FVCOM files can be loaded in parallel via \code{cl} and \code{pass2varlist} arguments.
#'
#' @return The function returns a dataframe containing the columns in \code{dat_obs1} with some additions. If \code{dat_obs2} is used to add observations to this dataframe, the dataframe returned also contains a column with environmental observations, 'obs', the timestamps at which these were made ('timestamp_obs'), extracted from \code{dat_obs2}, and the difference (seconds) between the timestamps of known locations and the timestamps at which observations were made ('difftime'). 'mesh_ID' defines the node (or element) within which each observation occurred and 'index_row', 'index_mesh' (and, if applicable, 'index_layer') provide a reference to the cells in the FVCOM arrays from which model predictions were extracted. 'wc' contains the predicted conditions from WeStCOMS and 'diff' contains the difference between observed and predicted conditions (i.e. 'obs' - 'wc').
#'
#' @examples
#'
#' ###########################################
#' #### Example (1): Temperature observations at specified locations and times
#'
#' #### Hypothetical scenario
#' # Imagine we have recorded surface temperature observations
#' # ... with standard probes in the following locations and at the specified times.
#'
#' #### Define dataframe:
#' # First, define imaginary timestamps and locations at which we made observations
#' set.seed(1)
#' timestamp <- as.POSIXct(c("2016-03-01", "2016-03-02"), tz = "UTC")
#' xy_node <- sp::coordinates(dat_mesh_around_nodes)
#' xy_sel <- sample(1:nrow(xy_node), size = length(timestamp))
#' long <- as.numeric(xy_node[xy_sel, 1])
#' lat <- as.numeric(xy_node[xy_sel, 2])
#' dat_obs1 <- data.frame(timestamp = timestamp, long = long, lat = lat, layer = 1)
#' # Second, add imaginary observations to the dataframe:
#' dat_obs1$obs <- stats::runif(nrow(dat_obs1), 7, 10)
#'
#' #### Define match dataframes to provide the link between dat_obs1 and the WeStCOMS arrays:
#' match_hour <- data.frame(hour = 0:1, index = 1:2)
#' match_layer <- data.frame(layer = 1:2, index = 1:2)
#' match_mesh <- data.frame(mesh = dat_nodexy$node_id, index = 1:length(dat_nodexy$node_id))
#'
#' #### Define mesh CRS:
#' proj <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#' raster::crs(dat_mesh_around_nodes) <- proj
#'
#' #### Define path from which to load predictions:
#' path <- system.file("WeStCOMS_files/temp",
#'                     package = "fvcom.tbx", mustWork = TRUE)
#' path <- paste0(path, "/")
#'
#' #### Implement validation:
#' validation <-
#'   validate(dat_obs1 = dat_obs1,
#'                    dat_obs2 = NULL,
#'                    mesh = dat_mesh_around_nodes,
#'                    match_hour = match_hour,
#'                    match_layer = match_layer,
#'                    match_mesh = match_mesh,
#'                    dir2load = path,
#'                    extension = ".mat",
#'                    corrupt = NULL,
#'                    cl = NULL,
#'                    pass2varlist = NULL,
#'                    verbose = TRUE
#'   )
#' # Examine output:
#' validation
#'
#' #### Check that each node IDs was correctly identified given supplied coordinates:
#' # Compare selected nodes from which coordinates were inputted
#' # ... with the nodes identified by the function:
#' as.character(dat_mesh_around_nodes$ID)[xy_sel]; validation$mesh_ID
#' # We can see we have identified the correct position in the array corresponding to each node:
#' cbind(validation[, c("index_mesh", "mesh_ID")], match_mesh[validation$index_mesh, ])
#'
#' ### We can see that the data for each node has been extracted correctly:
#' mapply(list.files(path, full.names = TRUE),
#'        split(validation, f = validation$date_name),
#'        FUN = function(con, df){
#'          # Load in file
#'          sample <- R.matlab::readMat(con)
#'          sample <- sample$data
#'          # Extract data (include df$layer here since 3d field):
#'          sample <- sample[df$index_hour, df$index_layer,  df$index_mesh]
#'          # Check identical:
#'          print(identical(sample, df$wc))
#'        })
#'
#'
#' ###########################################
#' #### Example (2): A validation dataset from animal movement data
#'
#' #### Hypothetical scenario
#' # Imagine that our validation dataset comes from animal movement data
#' # ... in which locations are known from one dataset and observations have been made
#' # ... and stored in another dataset.
#'
#' #### Define dat_obs1
#' # Define some timestamps (on 00:00 hours to match sample data in package):
#' timestamp <- as.POSIXct(c("2016-03-01 00:00:32", "2016-03-02 00:00:30"), tz = "UTC")
#' # Define locations
#' long <- as.numeric(xy_node[xy_sel, 1])
#' lat <- as.numeric(xy_node[xy_sel, 2])
#' # Define dataframe
#' dat_obs1 <- data.frame(timestamp = timestamp, long = long, lat = lat, layer = 1, key = 1)
#'
#' #### Define dat_obs2
#' # Imagine we have collected observations every 2 minutes
#' timestamp_obs <-
#'   seq(as.POSIXct("2016-03-01", tz = "UTC"), as.POSIXct("2016-03-03", tz = "UTC"), by = "2 mins")
#' dat_obs2 <- data.frame(timestamp = timestamp_obs,
#'                        key = 1,
#'                        obs = stats::runif(length(timestamp_obs), 7, 10))
#'
#' #### Implement validation:
#' validation <-
#'   validate(dat_obs1 = dat_obs1,
#'                    dat_obs2 = dat_obs2,
#'                    mesh = dat_mesh_around_nodes,
#'                    match_hour = match_hour,
#'                    match_layer = NULL,
#'                    match_mesh = match_mesh,
#'                    dir2load = path,
#'                    extension = ".mat",
#'                    corrupt = NULL,
#'                    cl = NULL,
#'                    pass2varlist = NULL,
#'                    verbose = TRUE
#'   )
#' #### Examine output:
#' validation
#'
#' #### We can see that the algorithm has identified observations
#' # ... at the nearest point in time to known locations
#' validation[, c("timestamp", "timestamp_obs", "obs")]
#' cbind(validation$obs, dat_obs2[dat_obs2$timestamp %in% validation$timestamp_obs, "obs"])
#' # The algorithm then uses this information to extract outputs from WeStCOMS.
#'
#' @export
#' @author Edward Lavender
#'

################################################
################################################
#### validate()

validate <-
  function(dat_obs1,
           dat_obs2 = NULL,
           threshold_match_gap = NULL,
           mesh,
           match_hour = data.frame(hour = 0:23, index = 1:24),
           match_layer = NULL,
           match_mesh = NULL,
           extension = ".mat",
           corrupt = NULL,
           read_fvcom = function(con) R.matlab::readMat(con)$data,
           dir2load,
           cl = NULL,
           pass2varlist = NULL,
           verbose = TRUE
  ){


    ################################################
    #### Set up

    #### Algorithm start
    t1 <- Sys.time()
    if(verbose) {
      cat("fvcom.tbx::validate() called...\n")
      cat("Step 1: Initial processing of dat_obs1...\n")
    }

    #### Initial checks
    # Check dat_obs1 contains minimum required columns
    stopifnot(all(c("timestamp", "lat", "long") %in% colnames(dat_obs1)))

    #### Add hour, date and date_name to dat_obs1
    # Add hour (dat_obs1 must contain 'timestamp' column):
    dat_obs1$hour <- lubridate::hour(lubridate::round_date(dat_obs1$timestamp, unit = "hour"))
    # Define date and date_name (dat_obs1 must contain 'timestamp' column):
    dat_obs1$date <- as.Date(dat_obs1$timestamp)
    dat_obs1$date_name <- date_name(dat_obs1$date, define = "date_name")

    #### Add mesh IDs to dataframe
    # ... use find_cells()
    # ... use return = 1 to implement this only for unique coordinates
    # ... and then match them to dat_obs1 for speed.
    if(verbose) cat("Step 3: Determining the corresponding WeStCOMS mesh location for each observation...\n")
    mesh_IDs <- find_cells(lat = dat_obs1$lat,
                           long = dat_obs1$long,
                           mesh = mesh,
                           proj = raster::crs(mesh),
                           f = function(x) as.integer(as.character(x)),
                           return = 1
    )
    mesh_IDs$xy <- paste0(mesh_IDs$long, "_", mesh_IDs$lat)
    dat_obs1$xy <- paste0(dat_obs1$long, "_", dat_obs1$lat)
    dat_obs1$mesh_ID <- mesh_IDs$mesh_ID[match(dat_obs1$xy, mesh_IDs$xy)]
    dat_obs1$xy <- NULL


    ################################################
    #### Add observations from dat_obs2 to dat_obs1, if necessary

    #### Scenario 1: dat_obs2 is NULL
    # Simply ensure that 'obs' has been provided in dat_obs1
    if(is.null(dat_obs2)){
      stopifnot(!is.null(dat_obs1$obs))

    #### Scenario 2: dat_obs2 has been provided...
    # Then we'll add observations using pair_ts(), accounting for different keys
    } else{
      if(verbose) cat("Step 2: Adding observations to dat_obs1 from dat_obs2...\n")

      #### Check obs have been provided in dat_obs2 and keys have been provided
      stopifnot(!is.null(dat_obs2$obs),
                !is.null(dat_obs1$key),
                !is.null(dat_obs2$key),
                all(unique(dat_obs1$key) %in% unique(dat_obs2$key))
                )

      #### Implement pair_ts() to pair timeseries
      # Define control match gap appropriately
      if(!is.null(threshold_match_gap)){
        control_match_gap <- "remove"
        min_gap <- -threshold_match_gap
        max_gap <- threshold_match_gap
      } else{
        control_match_gap <- NULL
        min_gap <- NULL
        max_gap <- NULL
      }
      # Implement pair_ts(), adding a column 'timestamp_obs' rather than 'obs' directly
      # ... into dat_obs1 (it is useful to retain this column).
      dat_obs2$timestamp_obs <- dat_obs2$timestamp
      dat_obs1 <- pair_ts(d1 = dat_obs1,
                          d2 = dat_obs2,
                          time_col = "timestamp",
                          key_col = "key",
                          val_col = "timestamp_obs",
                          method = "match_ts_nearest_by_key",
                          min_gap = min_gap,
                          max_gap = max_gap,
                          units = "secs",
                          control_beyond_gap = control_match_gap
                          )
      if(is.null(dat_obs1)){
        stop("No observations remaining in dat_obs1 after implementation of matching proceedure.")
      }
      # Add the observations back in by matching timestamp_obs.
      dat_obs1$obs <- dat_obs2$obs[match(dat_obs1$timestamp_obs, dat_obs2$timestamp_obs)]
    }


    ################################################
    ################################################
    #### Add wc predictions to dataframe

    if(verbose) cat("Step 4: Calling fvcom.tbx::extract() to extract predictions...\n")
    dat_obs1_wc <- extract(dat = dat_obs1,
                           match_hour = match_hour,
                           match_layer = match_layer,
                           match_mesh = match_mesh,
                           corrupt = corrupt,
                           read_fvcom = read_fvcom,
                           dir2load = dir2load,
                           extension = extension,
                           cl = cl,
                           pass2varlist = pass2varlist,
                           verbose = verbose)

    #### Difference between observed and expected values
    dat_obs1_wc$diff <- dat_obs1_wc$obs - dat_obs1_wc$wc

    #### End time
    t2 <- Sys.time()
    tdiff <- round(difftime(t2, t1))
    if(verbose) cat(paste0("fvcom.tbx::validate() algorithm duration approximately ", round(tdiff), " ",  methods::slot(tdiff, "units"), ".\n"))

    #### Return dataframe
    return(dat_obs1_wc)

  }


#### End of code.
################################################
################################################
