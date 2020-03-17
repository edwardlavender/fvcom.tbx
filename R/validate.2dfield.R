#' @title Validate WeStCOMS predictions across 2d fields with observations
#'
#' @description For specified location(s), layer(s) and timestamp(s), this function pairs observations with corresponding WeStCOMS predictions. Locations and observations can be derived from different datasets. This means that animal movement datasets, in which locations and observations may be derived from different electronic tags, can be used for validation. The function uses nearest neighbour interpolation to extract WeStCOMS predictions for the nearest hour, at the specified layer and at the nearest mesh cell for which predictions are available relative to user inputs. Paired observations and predictions can be used to examine WeStCOMS skill (i.e., prediction accuracy).
#'
#' @param dat_obs1 A dataframe which contains the location(s), layer(s) and timestamp(s) at which environmental observations have been made. These should be defined in columns named 'lat' and 'long', 'location', 'layer' and 'timestamp' respectively. A column 'key' may also need to be included (see Details). Location(s) are assumed to be in World Geodetic System format (i.e. WGS 84). Observations can either be located in this dataframe, in a column called 'obs', or in a separate dataframe, (\code{dat_obs2}), see below.
#' @param dat_obs2 A dataframe which contains the environmental observations which will be used to validate WeStCOMS predictions. This must contain timestamps ('timestamp'), observations ('obs'). A column 'key' may also need to be included (see Details). Note that observations can be provided in \code{dat_obs1} but, for some validation datasets, observations and locations are in separate datasets (see Details). \code{dat_obs2} allows for this flexibility.
#' @param threshold_match_gap A numeric input which defines the gap between the timestamp of a known location and that of a corresponding observation before/after which these timestamps in \code{dat_obs1} are removed. This is useful if \code{dat_obs2} is provided and if observations are only available for a sample of the timestamps at which locations are known. In this scenario, matching observations via the nearest timestamp may be inappropriate because there may be long gaps between the times of known locations and observations.
#' @param mesh A \code{\link[sp]{SpatialPolygonsDataFrame-class}} object which defines the WeStCOMS mesh created by \code{\link[WeStCOMSExploreR]{build.mesh}}.
#' @param match_hour A dataframe with two columns named 'hour' and 'index' which defines thew index in WeStCOMS files (i.e. the row) which corresponds to each hour. The default dataframe is usually appropriate, if WeStCOMS files have not been subsetted. However, if WeStCOMS files have been subsetted (e.g. selecting rows corresponding to hours 12 and 13), then rows 1 and 2 in WeStCOMS files now represent hours 12 and 13, not hours 0 and 1. \code{match_hour} provides this link (see Examples). All WeStCOMS files are assumed to have the same structure.
#' @param match_layer A dataframe with two columns named 'layer' and 'index' which defines the index in WeStCOMS files (i.e. the column) which corresponds to each layer. This is only necessary if you are working with 3d fields and if you are working with a subset of WeStCOMS files: if WeStCOMS files have been subsetted (e.g. by selecting layers corresponding to hours 2 and 3), then columns 1 and 2 in WeStCOMS files now represent layers 2 and 3, not hours 1 and 2. \code{match_layer} provides this link (see Examples). All WeStCOMS files are assumed to have the same structure.
#' @param match_mesh (optional) A dataframe with two columns named 'mesh' and 'index' which defines the index in WeStCOMS files (columns or sheetts for 2d and 3d arrays respectively) which corresponds to each mesh cell. This only needs to be provided if you are working with a subset of WeStCOMS files: in this sitation, mesh IDs 5, 6, 7, for example, may not correspond to index 5, 6, 7 in WeStCOMS files. Hence, \code{match_mesh} provides this link (see Examples). All WeStCOMS files are assumed to have the same structure.
#' @param dir2load A string which defines the directory from which to load WeStCOMS files. In this directory, WeStCOMS file names are assumed to follow the standard naming convention (i.e., yymmdd; see \code{\link[WeStCOMSExploreR]{date.name}}. All files with the pattern \code{*extension}, see \code{extension} are assumed to be WeStCOMS files.
#' @param extension A string which defines the extension of the WeStCOMS files. The default is \code{".mat"}.
#' @param corrupt A vector of numbers, representing WeStCOMS date names, which define corrupt files. These will not be loaded.
#' @param cl (optional) A cluster objected created by the parallel package. If supplied, the algorithm is implemented in parallel. Note that the connection with the cluster is stopped within the function.
#' @param pass2varlist A list of character vector of names of objects to export to be passed to the \code{varlist} argument of \code{\link[parallel]{clusterExport}}.
#' @param verbose A logical input which defines whether or not to display messages to the console detailing function progress.
#'
#' @details To use this function, the user must supply a dataframe (\code{dat_obs1}) which contains the locations(s) , layer(s) and time(s) (in a column named 'timestamp'). These columns must be named 'long' and 'lat', 'layer' and 'timestamp' respectively. A column, 'key', may also need to be included (see below). Location(s) are assumed to be in World Geodetic System format (i.e. WGS 84). Observations can either be located in this dataframe, in a column called 'obs', or in a separate dataframe, (\code{dat_obs2}) with columns 'timestamp', 'obs' and 'key'. One situation where this latter option is useful is for animal movement data when the location of the animal is known from one tag type (e.g. passive acoustic telemetry) and observations are recorded by another tag type (e.g. an archival tag), possibly at a different resolution. In this scenario, both dataframes should contain a column, 'key', which connects the factor level(s) (e.g. individuals) for which locations observations have been made in the first dataframe with the factor level(s) for which environmental observations (which will be used to validate WeStCOMS) have been made. In this case, a nearest neighbour matching approach, implemented using \code{\link[utils.add]{match_closest}}, is used to add observations into the first dataframe (\code{dat_obs1}) from the second dataframe (\code{dat_obs2}) by selecting those that occured closest in time to the timestamps stipulated in the first dataframe (this is why is is important to have a 'key' to distinguish among factor levels). The function uses a mesh supplied by the user to determine the WeStCOMS mesh nodes/elements within which each location lies (i.e. nearest neighbour interpolation). With this information, the function loads in each FVCOM file from a user-defined directory, extracts the relevant model predictions from this file, and then returns the original dataframe with preditions added. FVCOM files can be loaded in parallel via \code{cl} and \code{pass2varlist} arguments.
#'
#' @return The function returns a dataframe containing the columns in \code{dat_obs1} with some additions. If \code{dat_obs2} is used to add observations to this dataframe, the dataframe returned also contains a column with environmental observations, 'obs', the timestamps at which these were made ('timestamp_obs'), extracted from \code{dat_obs2}, and the difference (seconds) between the timestamps of known locations and the timestamps at which observations were made ('difftime'). 'meshID' defines the node (or element) within which each observation occured and 'index_row', 'index_mesh' (and, if applicable, 'index_layer') provide a reference to the cells in the WeStCOMS files from which model predictions were extracted. 'wc' contains the predicted conditions from WeStCOMS and 'diff' contains the difference between observed and predicted conditions (i.e. 'obs' - 'wc').
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
#' path <-
#'  paste0(system.file("WeStCOMS_files/temp", package = "WeStCOMSExploreR", mustWork = TRUE), "/")
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
#' as.character(dat_mesh_around_nodes$ID)[xy_sel]; validation$meshID
#' # We can see we have identified the correct position in the array corresponding to each node:
#' cbind(validation[, c("index_mesh", "meshID")], match_mesh[validation$index_mesh, ])
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
#' ###########################################
#' #### Example (2): A validation dataset from animal movement data
#'
#' #### Hypothetical scenario
#' # Imagine that our valiidation dataset comes from animal movement data
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
#### validate_2dfield

validate <-
  function(dat_obs1,
           dat_obs2 = NULL,
           threshold_match_gap = NULL,
           mesh,
           match_hour = data.frame(hour = 0:23, index = 1:24),
           match_layer = NULL,
           match_mesh = NULL,
           dir2load,
           extension = ".mat",
           corrupt = NULL,
           cl = NULL,
           pass2varlist = NULL,
           verbose = TRUE
  ){



    ################################################
    ################################################
    #### Set up

    #### algorithm start
    t1 <- Sys.time()
    if(verbose) cat("Step 1: Initial processing of dat_obs1...\n")
    # Define output list [not currently implemented]
    # output <- list()
    # Check dat_obs1 contains minimum required columns
    stopifnot(all(c("timestamp", "lat", "long") %in% colnames(dat_obs1)))

    #### Add necessary columns
    # Add hour (dat_obs1 must contain 'timestamp' column):
    dat_obs1$hour <- lubridate::hour(lubridate::round_date(dat_obs1$timestamp, unit = "hour"))
    # Define date and date_name (dat_obs1 must contain 'timestamp' column):
    dat_obs1$date <- as.Date(dat_obs1$timestamp)
    dat_obs1$date_name <- date.name(dat_obs1$date, define = "date_name")

    #### Remove any corrupt files from dat_obs1
    # This stops errors later when we used dat_obs1$date_name to load files
    if(!is.null(corrupt)){
      pos_corrupt <- which(dat_obs1$date_name %in% corrupt)
      lpc <- length(pos_corrupt)
      if(lpc > 0){
        if(verbose){
          cat(paste0(lpc, " records in dat_obs1 associated with corrupt files. These observations will be removed.\n"))
        }
        dat_obs1 <- dat_obs1[-c(pos_corrupt), ]
      }
    }

    #### Remove any observations in dat_obs1 before/after first/last file:
    files <- list.files(dir2load, pattern = paste0("*", extension))
    stopifnot(length(files) > 0)
    file_codes <- as.numeric(substr(files, 1, 6))
    first_date_name <- min(file_codes)
    last_date_name <- max(file_codes)
    dat_obs1 <- dat_obs1[dat_obs1$date_name >= first_date_name &
                           dat_obs1$date_name <= last_date_name, ]
    stopifnot(all(dat_obs1$date_name %in% file_codes))

    #### Check observations remain after processing
    stopifnot(nrow(dat_obs1) > 0)


    ################################################
    ################################################
    #### Add observations from dat_obs2 to dat_obs1, if necessary

    #### Define step 2
    if(verbose) cat("Step 2: Checking and/adding observations to dat_obs1...\n")

    #### First, check that if dat_obs2 is NULL , then 'obs' have been provided in dat_obs1
    if(is.null(dat_obs2)){
      stopifnot(!is.null(dat_obs1$obs))

    #### Else, if dat_obs2 has been provided...
    # Then we'll add observations using utils.add::match_closest(), accounting for different keys
    } else{

      #### Check obs have been provided in dat_obs2 and keys have been provided
      stopifnot(!is.null(dat_obs2$obs),
                !is.null(dat_obs1$key),
                !is.null(dat_obs2$key),
                all(unique(dat_obs1$key) %in% unique(dat_obs2$key))
                )

      ################################################
      #### Remove locations outside observation windows

      #### Loop over the dataframe for each key and adjust
      # ... dataframe accounting to timing of location/observations:
      # Before implementing match closest (below), we'll adjust dat_obs1_key so that we limit the number of timestamps with locations
      # ... before the first observation/after the last known location
      # ... This avoids issues with match closest (e.g. if we have lots of known locations after the
      # ... last observation (e.g. because an archival tag was recovered early), then we'd get very large
      # ... negative values for match_gap because the algorithm matches each location to the final
      # ... observation, which may have been long ago).
      # We could deal with this after match_closest(), which we do because the approach below
      # ... doesn't deal with breaks in dat_obs2, but match_closest() can be slow, so
      # ... we'll implement this inital pre-processing first.

      cat("Processing dat_obs1 for each key...\n")
      dat_obs1_key_ls <- split(dat_obs1, f = dat_obs1$key)
      if(!is.null(threshold_match_gap)){

        #### Define a list of processed outputs
        dat_obs1_key_ls <-
          pbapply::pblapply(dat_obs1_key_ls, function(dat_obs1_key){
            # subset dat_obs2 to focus in on the correct key too:
            dat_obs2_key <- dat_obs2[dat_obs2$key == dat_obs1_key$key[1], ]
            # define positions to keep and remove and a processed dataframe:
            first_obs <- min(dat_obs2_key$timestamp); last_obs <- max(dat_obs2_key$timestamp)
            pos2keep <- which(dat_obs1_key$timestamp >= (first_obs - threshold_match_gap) &
                                dat_obs1_key$timestamp <= (last_obs + threshold_match_gap))
            lpos2keep <- length(pos2keep)
            lpos2rem <- nrow(dat_obs1_key) - lpos2keep
            dat_obs1_key <- dat_obs1_key[pos2keep, ]
            # define output list:
            ls <- list(pos2keep = pos2keep, lpos2keep = lpos2keep, lpos2rem = lpos2rem, dat_obs1_key = dat_obs1_key)
            return(ls)
          })

        #### Define dataframe showing the number of positions to be removed for each key
        lpos2keep <- sapply(dat_obs1_key_ls, function(elm) elm$lpos2keep)
        lpos2rem <- sapply(dat_obs1_key_ls, function(elm) elm$lpos2rem)
        keys <- names(dat_obs1_key_ls)
        dp <- data.frame(key = keys, n_keep = lpos2keep, n_remove = lpos2rem)
        keys2rem <- NULL
        if(any(dp$n_keep == 0)){
          keys2rem <- as.character(dp$key[which(dp$n_keep == 0)])
        }

        #### Relay results to user
        # if(verbose) {
        #  cat("dat_obs1 processing results: for each key, the number of retained versus removed observations (because corresponding timestamps are too far outside the window for which observations are available: \n")
        #  print(dp)
        #  if(!is.null(keys2rem)) { cat(paste0("\n", "key(s) excluded from dat_obs1 are: \n")); cat(keys2rem); cat("\n") }
        # }

        #### Process dat_obs1_key_ls
        dat_obs1_key_ls <- lapply(dat_obs1_key_ls, function(elm) elm$dat_obs1_key)
        # Remove any elements for keys to be removed
        if(!is.null(keys2rem)){
          dat_obs1_key_ls[which(names(dat_obs1_key_ls) %in% keys2rem)] <- NULL
        }
      }

      ################################################
      #### Match observations

      cat("Matching observations to dat_obs1 for each key...\n")
      dat_obs1_key_ls <-
        pbapply::pblapply(dat_obs1_key_ls, function(dat_obs1_key){
          #### testing
          # dat_obs1_key <- dat_obs1_key_ls[[1]]

          #### subset dat_obs2 to focus in on the correct key too:
          dat_obs2_key <- dat_obs2[dat_obs2$key == dat_obs1_key$key[1], ]

          #### Check dat_obs2_key is ordered, which is required by match_closest:
          stopifnot(!is.unsorted(dat_obs2_key$timestamp))

          #### match closest:
          match_closest_pos <- utils.add::match_closest(dat_obs1_key$timestamp, dat_obs2_key$timestamp)
          # Checks:
          # rand_check_pos <- sample(x = 1:nrow(dat_obs1_key), size = 10)
          # data.frame(dat_obs1_key$timestamp[rand_check_pos], dat_obs2_key$timestamp[match_closest_pos[rand_check_pos]])

          #### Add observations to df
          dat_obs1_key$obs <- dat_obs2_key$obs[match_closest_pos]
          dat_obs1_key$timestamp_obs <- dat_obs2_key$timestamp[match_closest_pos]

          #### Additional post-processing
          # Implement additional processing to remove observations for which the gaps between known locations/observations
          # ... are too large (i.e., those due to gaps in dat_obs2)
          dat_obs1_key$difftime <- difftime(dat_obs1_key$timestamp_obs, dat_obs1_key$timestamp, units = "secs")
          if(!is.null(threshold_match_gap)){
            pos2rem <- which(dat_obs1_key$difftime < -threshold_match_gap | dat_obs1_key$difftime > threshold_match_gap)
            if(length(pos2rem) > 0) dat_obs1_key <- dat_obs1_key[-c(pos2rem), ]
          }

          #### Relay match_gap results to user
          # (no longer implemented)
          # if(check_match_gap){
          #  match_gap <- difftime(dat_obs1_key$timestamp_obs, dat_obs1_key$timestamp)
          #  match_gap_units <- attributes(match_gap)$units
          #  if(verbose){
          #    cat(paste0("\n For key ", dat_obs1_key$key[1],
          #              ", the approximate range in the difference between the timestamp recorded dat_obs1 and dat_obs2 is ",
          #              floor(range(match_gap)[1]), ":", ceiling(range(match_gap)[2]), " ", match_gap_units, ".\n"))
          #  }
          #  # histogram of the differences
          #  plot.pretty::pretty_hist(as.numeric(match_gap),
          #                           xlab = paste0("Time Gap (", match_gap_units, ")"),
          #                           ylab = "Frequency",
          #                           mtext_args = list())
          # }

          #### Return processed dataframe
          return(dat_obs1_key)
        })

      #### Bind dat_obs1_key_ls back into a single dataframe, which now has 'obs' added
      # ... appropriately for each key
      dat_obs1 <- do.call(rbind, dat_obs1_key_ls)

    }


    ################################################
    ################################################
    #### Add node IDs to dataframe

    if(verbose) cat("Step 3: Determining the corresponding WeStCOMS mesh location for each observation...\n")
    # Define coordinates as spatial points; these are assumed to be in WGS84
    xysp <- dat_obs1[, c("long", "lat")]
    proj <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    xysp <- sp::SpatialPoints(xysp, proj4string = proj)
    # Define the node (or element if a mesh with elements is supplied) within which
    # ... each location lies. unique() and match() could be used here to
    # ... improve speed by only sampling unique meshIDs.
    meshIDs <- sp::over(xysp, mesh)
    # Add meshID to observations; careful, use as.character(), bcause meshIDs$ID are a factor:
    dat_obs1$meshID <- as.numeric(as.character(meshIDs$ID))


    ################################################
    ################################################
    #### Add wc predictions to dataframe

    if(verbose) cat("Step 4: Getting ready to load in FVCOM files...\n")

    #### Define indices for extracting data correctly
    ## hour
    if(is.null(match_hour)) {
      dat_obs1$index_hour <- dat_obs1$hour
    } else {
      dat_obs1$index_hour <- match_hour$index[match(dat_obs1$hour, match_hour$hour)]
    }
    ## layer (if applicable)
    if(!is.null(dat_obs1$layer)) {
      if(is.null(match_layer)) {
        dat_obs1$index_layer <- dat_obs1$layer
      } else{
        dat_obs1$index_layer <- match_layer$index[match(dat_obs1$layer, match_layer$layer)]
      }
    }
    ## Mesh
    if(is.null(match_mesh)) {
      dat_obs1$index_mesh <- dat_obs1$meshID
    } else{
      dat_obs1$index_mesh <- match_mesh$index[match(dat_obs1$meshID, match_mesh$mesh)]
    }

    #### Define cluster
    if(!is.null(cl)){
      varlist <- pass2varlist
      parallel::clusterExport(cl, varlist)
    }

    #### Loop over every dataframe (date_name)
    # ...load in the FVCOM data array and add the model outputs to the dataframe
    if(verbose) cat("Step 5: Loading FVCOM files and adding model predictions to dat_obs1...\n")
    dat_obs1_ls <- split(dat_obs1, f = dat_obs1$date_name)
    dat_obs1_ls_wc <-
      pbapply::pblapply(dat_obs1_ls, cl = cl, function(df){

        #### Define first df for testing
        # df <- dat_obs1_ls[[1]]

        #### Define connection and load file
        con <- paste0(dir2load, df$date_name[1], extension)
        if(verbose) cat(paste0("\n Loading file ", df$date_name[1], extension, "...\n"))
        wc <- R.matlab::readMat(con)
        wc <- wc$data

        #### Extract wc predictions for each position, depending on whether
        # ... or not we're dealing with a 2d or 3d array.
        # Note use use of cbind() in the indexing which is necessary to return
        # ... a single value for index (e.g. index [1, 2, 3]).
        if(length(dim(wc)) == 3){
          stopifnot(!is.null(df$layer))
          df$wc <- wc[cbind(df$index_hour, df$index_layer, df$index_mesh)]
        } else if(length(dim(wc)) == 2){
          df$wc <- wc[cbind(df$index_hour, df$index_mesh)]
        } else{
          stop("2d or 3d matrix should be supplied")
        }

        # Print first value for checking purposes
        # if(verbose) cat(paste0("wc[1] = ", df$wc[1]))

        #### Return dataframe
        return(df)
      })

    if(!is.null(cl)) parallel::stopCluster(cl = cl)

    #### Define dataframe
    dat_obs1_wc <- do.call(rbind, dat_obs1_ls_wc)

    #### Difference between observed and expected temperatures
    dat_obs1_wc$diff <- dat_obs1_wc$obs - dat_obs1_wc$wc

    #### End time
    t2 <- Sys.time()
    tdiff <- round(difftime(t2, t1))
    if(verbose) cat(paste0("Algorithm duration approximately ", round(tdiff), " ",  methods::slot(tdiff, "units"), ".\n"))

    #### Return dataframe
    return(dat_obs1_wc)

  }


#### End of code.
################################################
################################################
