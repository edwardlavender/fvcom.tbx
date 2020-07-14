#' @title Define new 2 dimensional hydrodynamic fields
#' @description This function defines new 2 dimensional hydrodynamic fields from WeStCOMS outputs, such as thermocline strength.
#'
#' @param vars A character vector of variables for which new files should be created. Currently supported options are: (1) 'thermocline_strength'; (2) 'wind_speed'; (3) 'wind_direction'; (4) current_speed; (5) current velocity; (6) sun angle.
#' @param nodexy A dataframe containing node ids and coordinates at which sun angle will be calculated. The dataframe should have three columns: node_id, x and y. See \code{?WeStCOMSExploreR::dat_nodexy} for the dataset included in WeStCOMSExploreR as a guide. Note that is is important that the nodexy dataframe should represent the full mesh, rather than a subset of a mesh if you want to treat sun angle in the same way as other WeStCOMS outputs. For example, if you plan to iterate over variables and select a subset of nodes (via indexing) across which to plot the mesh each time, it is essential that the arrays are all of the same dimensions to avoid indexing issues.
#' @param dir The directory from which to load WeStCOMS files and in which to save new files. For each of the inputted \code{vars}, the following folders are assumed to exist: "temp", "uwind_speed", "vwind_speed", "uvelocity", "vvelocity" from which model outputs can be loaded.
#' @param date_name A vector of date names that correspond to the names of WeStCOMS files and the dates for which you want to create outputs.
#' @param cl (optional) A cluster objected created by the parallel package. If supplied, the algorithm is implemented in parallel. Note that the connection with the cluster is stopped within the function.
#' @param pass2varlist A list of character vector of names of objects to export to be passed to the \code{varlist} argument of \code{\link[parallel]{clusterExport}}. This is required if you specify some function arguments via objects, rather than directly. (See the use of \code{path} and \code{vars1} in the examples.) These objects must be located in the global environment.
#'
#'@details Thermocline strength is approximated as the difference in temperature between the surface and the bottom, which is much computationally faster than calculating other metrics (e.g. the standard deviation in temperature across layers). Note that this means that the thermocline_strength option cannot be applied to the data supplied with this package, which is a small subset of the temperature data that does not include the full 10 layers. Speeds and directions are given in m/s and degrees respectively. Direction is the direction of mass flow. Sun angle is calculated using \code{\link[WeStCOMSExploreR]{compute_field_sun_angle}}.
#'
#' @examples
#' #### (1) Implement function on a single processor
#' # Define directory of the folder containing temperatures:
#' path <- system.file("WeStCOMS_files/", package = "WeStCOMSExploreR", mustWork = TRUE)
#' # Define variables for which we'll create new files:
#' vars1 <- c("wind_speed", "wind_direction", "sun_angle")
#' # These vars assume that a 'uwind_speed' and 'vwind_speed' folder exist,
#' # ... from which the necessary data can be loaded
#' # Examine of path:
#' list.files(path)
#' # Use create_wcdirs() to create empty folders for these new variables:
#' create_wcdirs(dir = path, vars = vars1)
#' # Define new fields
#' compute_field(vars = vars1,
#'                   nodexy = WeStCOMSExploreR::dat_nodexy,
#'                   dir = path,
#'                   date_name = c("160301", "160302")
#'                   )
#' # List files again:
#' list.files(path, recursive = TRUE)
#'
#' #### (2) Implement function on multiple cores
#' # (This is most beneficial for a long vector of date_name),
#' # but, for demonstration purposes...:
#' cl <- parallel::makeCluster(2L)
#' compute_field(vars = vars1,
#'                   nodexy = WeStCOMSExploreR::dat_nodexy,
#'                   dir = path,
#'                   date_name = c("160301", "160302"),
#'                   cl = cl,
#'                   pass2varlist = list("path", "vars1"))
#' # Note that the connection with the cluster is closed internally within the function.
#'
#'@author Edward Lavender
#'@export



################################################
################################################
#### compute_field

compute_field <-
  function(
    vars,
    nodexy,
    dir,
    date_name,
    cl = NULL,
    pass2varlist = list(NULL)){


    ################################################
    ################################################
    #### Set up


    ################################################
    #### Checks:

    #### Define variables that are currently implemented
    # ... and stop if any of the inputted variables are not currently supported.
    vars_implemented <- c("thermocline_strength",
                          "wind_speed",
                          "wind_direction",
                          "current_speed",
                          "current_direction",
                          "sun_angle")

    if(any(!(vars %in% vars_implemented))){
      stop("Implemented value to 'vars' is not supported.")
    }

    ################################################
    #### Sun angle parameters

    #### Outside of the loop, define parameters to calculate sun angle
    # ... if requested
    if("sun_angle" %in% vars){
      # Identify the full list of unique nodes:
      # Use the nodexy dataframe rather than a created mesh object to define IDs (via mesh$ID),
      # ... because there are more nodes in the nodexy dataframe (i.e. in raw fvcom outputs)
      # ... than in mesh (because you can't draw a complete polygon around every node)...
      # ... which can lead to indexing issues otherwise (e.g. when you select a subset of nodes
      # ... from a full array for plotting, you need to be sure you have the full array)
      ID <- nodexy$node_id
      lID <- length(ID)

      # Define a matrix, use hours 1:24 to maintain consistency with other 'raw' WeStCOMS outputs.
      # Use every node in the
      sun_angle_mat <- matrix(NA, nrow = 24, ncol = lID)

      # Define dataframe of mesh IDs and coordinates for speed:
      IDxy <- data.frame(ID = ID, x = nodexy$x, y = nodexy$y)

    }

    ################################################
    #### export cluster objects if necessary

    # If a cluster has been supplied, then
    # ... the following objects need to be exported:
    if(!is.null(cl)){
      varlist_internal <- list("compute_field_sun_angle")
      varlist <- append(varlist_internal, pass2varlist)
      parallel::clusterExport(cl = cl, varlist = varlist)
      }



    ################################################
    ################################################
    #### Use pblapply to loop over date_names in parallel...

    #### loop over every date_name...
    pbapply::pblapply(
      date_name,
      function(dn){


        ################################################
        #### Thermocline

        #### Load temperature data:
        if("thermocline_strength" %in% vars){
          file2load <- paste0(dir, "temp", "/", dn, ".mat")
          fvcom <- R.matlab::readMat(file2load)
          dat <- fvcom$data

          #### Thermocline stength (layers 1 and 10)
          # Input: params = list(thermocline_strength = list(range, sd)),
          # Loop over every function specified...
          # lapply(params$thermocline_strength, function(fun){
          #  # Apply that function to the 3d data to calculate a 2d field:
          #  dat_ts <- apply(dat, c(1, 3), fun)
          #  # Save in appropriate folder:
          #  file2save <- paste0(dir, "thermocline_strength_", "fun", "/", date_name, ".RData")
          #  saveRDS(dat_tsp, file2save)
          #  }) # close lapply function and lappluy

          #### Thermocline strength:
          # Define the subset of data related to thermocline strength, i.e. layers 1 and 10
          dat_ts <- dat[, c(1, 10), ]
          # Calculate temperature difference from the surface to the bottom
          # +ve numbers mean the temperature at the surface is higher than at the bottom
          # the more positive the number, the stronger the thermocline
          # This generates a 2d array with one row for every hour and one column for every node
          # The value in each cell is the difference in temperaturee between the surface and the near-bottom layer
          # ... for that hour in that node.
          dat_tsp <- dat[, 1, ] - dat[, 2, ]
          # Save in appropriate folder:
          file2save <- paste0(dir, "thermocline_strength", "/", dn, ".RData")
          saveRDS(dat_tsp, file2save)

        } # close if(thermocline_strength_logic){

        #### Thermocline depth
        # Not currently implemented.


        ################################################
        ################################################
        #### Wind

        #### Load wind data:
        if(any(c("wind_speed", "wind_direction") %in% vars)){
          # Define u and v files
          ufile2load <- paste0(dir, "uwind_speed", "/", dn, ".mat")
          vfile2load <- paste0(dir, "vwind_speed", "/", dn, ".mat")
          # Define u and v data
          ufvcom <- R.matlab::readMat(ufile2load)
          udat   <- ufvcom$data
          vfvcom <- R.matlab::readMat(vfile2load)
          vdat   <- vfvcom$data
        } # close if(any(c(wind_speed_logic, wind_speed_logic))){

        #### Calculate wind speed
        if("wind_speed" %in% vars){
          # This returns a single 2d matrix, with one row for every hour and one column for every node
          # ... with the wind speed in each cell in m/s
          wind_speed <- sqrt(udat^2 + vdat^2)
          # Save in appropriate folder:
          file2save <- paste0(dir, "wind_speed", "/", dn, ".RData")
          saveRDS(wind_speed, file2save)
        } # close if(wind_speed_logic){

        #### Calculate wind_direction
        if("wind_direction" %in% vars){
          # calculate direction of mass flow:
          wind_direction <- 90 - atan2(y = vdat, x = udat) * 180/pi
          # adjust direction to be positive:
          posneg <- which(wind_direction < 0)
          wind_direction[posneg] <- wind_direction[posneg] + 360
          # Save in appropriate folder:
          file2save <- paste0(dir, "wind_direction", "/", dn, ".RData")
          saveRDS(wind_direction, file2save)
        } # close if(wind_direction_logic){


        ################################################
        #### Velocity

        #### Load velocity data:
        if(any(c("current_speed", "current_direction") %in% vars)){
          # Define u and v files
          ufile2load <- paste0(dir, "uvelocity", "/", dn, ".mat")
          vfile2load <- paste0(dir, "vvelocity", "/", dn, ".mat")
          # Define u and v data
          ufvcom <- R.matlab::readMat(ufile2load)
          udat   <- ufvcom$data
          vfvcom <- R.matlab::readMat(vfile2load)
          vdat   <- vfvcom$data
        } # close if(!any(c(current_speed_logic, wind_speed_logic))){

        #### Calculate current_speed
        if("current_speed" %in% vars){
          # This returns a single 3d matrix, with one row for every hour and one column
          # ... for every layer and one sheet for every node
          # ... with the velocity in each cell in m/s
          current_speed <- sqrt(udat^2 + vdat^2)
          # Save in appropriate folder:
          file2save <- paste0(dir, "current_speed", "/", dn, ".RData")
          saveRDS(current_speed, file2save)
        } # close if(current_speed_logic){

        #### Calculate current direction
        if("current_direction" %in% vars){
          # calculate direction of mass flow:
          current_direction <- 90 - atan2(y = vdat, x = udat) * 180/pi
          posneg <- which(current_direction < 0)
          current_direction[posneg] <- current_direction[posneg] + 360
          # Save in appropriate folder:
          file2save <- paste0(dir, "current_direction", "/", dn, ".RData")
          saveRDS(current_direction, file2save)
        } # close if(current_direction_logic){


        ################################################
        #### sun_angle

        #### Calculate sun_angle
        sun_angles_across_mesh <-
          compute_field_sun_angle(nodexy = nodexy,
                                date = date_name(dn, define = "date"),
                                date_name = dn,
                                tz = "UTC",
                                hours = 1:24,
                                sun_angle_mat = sun_angle_mat,
                                degrees = TRUE,
                                dir2save = paste0(dir, "sun_angle", "/"),
                                verbose = FALSE
                                )

    }, # close plpapply function
    cl = cl) # close pblapply

    # close cluster internally inside function
    if(!is.null(cl)){
      parallel::stopCluster(cl)
    }

  } # close function compute_field


#### End of code.
################################################
################################################
