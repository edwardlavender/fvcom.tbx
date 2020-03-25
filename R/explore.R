#' @title Explore WeStCOMS model outputs in 2d
#' @description This function computes summary statistics and/or maps environmental conditions for multiple environmental variables and/or timepoints. The user passes the function a dataframe specifying the variables to be included and their properties, the directory of the WeStCOMS files from which they can be loaded. For each variable, the function loads in the WeStCOMS files for each date in turn, computes summary statistics and/or plots a map for the hours specified. Iterative loading in of the files is necessary given the size/memory requirements of WeStCOMS files.
#'
#' @param field2d A dataframe that defines the variables and their properties for which the outputs will be explored. This must contain the following columns: \code{cov2d}, a character vector of variable names which correspond to the names of the folders in which the model outputs are located (see \code{dir2load}, below); \code{dim}, a character vector of "3d" or "2d" that defines whether or not that variable is resolved across WeStCOMS layers; \code{cov2dlayer}, a numeric vector that defines the layer for which outputs are of interest;   \code{resolved_at}, a character vector of "node" or "element" which defines whether or not that variable is resolved at the nodes or elements of an unstructured grid; \code{mesh_type}, a character vector of the corresponding mesh type ("element" or "node" across which that variable is structured); \code{extension}, a character vector of the extension of the model outputs (usually ".mat" but ".RData" for outputs created by \code{\link[WeStCOMSExploreR]{compute_new2dfield}}); \code{vector_field}, a logical vector defining whether or not that variable is a scalar (FALSE) or vector field (TRUE). For vector fields, the letters "wind" or "current" must be within the variable name in order for the algorithm to determine whether data should be loaded from the "uwind_speed" and "vwind_speed" or "uvelocity" and "vvelocity" folders in \code{dir2load} (see below).
#' @param mesh_around_nodes A mesh, created by \code{\link[WeStCOMSExploreR]{build_mesh}}, that surrounds nodes. This is required for variables that are resolved at nodes.
#' @param dataID_node A vector of node IDs which specifies the IDs of nodes within \code{mesh_around_nodes} for which there are model outputs. This provides a link between the array data loaded into the environment and the mesh. This is necessary because WeStCOMS arrays have a column 1,..., n for each node at which the variable is resolved. For full arrays, the column numbers correspond exactly to the IDs of mesh nodes. However, for subsetted arrays, column number x may not correspond to mesh ID x. Supplying dataID_node maintains the link between array column numbers and mesh cell IDs.
#' @param mesh_around_elements A mesh, created by \code{\link[WeStCOMSExploreR]{build_mesh}}, that surrounds elements. This is required for variables that are resolved at elements.
#' @param dataID_element A vector of element IDs which specifies the IDs of nodes within \code{mesh_around_elements} for which there are model outputs (see above).
#' @param dir2load A string defining the directory which contains folders, corresponding to the variables named in \code{field2d$cov2d} from which model outputs can be loaded. For all scalar fields, the variable supplied in \code{field2d$cov2d} must also be the name of the folder within \code{dir2save} from which data are loaded. For velocity fields (i.e. wind velocity or current velocity), the \code{field2d$cov2d} element must contain the letters "wind" or "current". U and V vector components are then loaded from the "uwind_speed" and "vwind_speed" or "uvelocity" and "vvelocity" folders respectively
#' @param date_name A vector of date names for which environmental outputs will be examined for each variable.
#' @param compute_sun_angle A logical input defining whether or not to compute sun angle.
#' @param sun_angle_param A list containing some essential parameters that are passed to \code{\link[WeStCOMSExploreR]{compute_sun_angle_field}} if sun angle is computed. This list should contain elements with the names: nodexy; tz; hours; degrees; dir2save; and verbose. These are described in \code{\link[WeStCOMSExploreR]{compute_sun_angle_field}}. All other parameters are for calculating sun angle are fixed or computed internally.
#' @param makeplot A logical input defining whether or not to make plots.
#' @param plot_param A list of parameters required to make plots. These parameters either wrap around, or are passed as arguments, to \code{\link[WeStCOMSExploreR]{plot2dfield}}. The list needs to contain elements with the names: hours4plots, par_op, coastline, zlab, zlab_line, vector_scale, dir2save. hours4plots is a numeric vector of all the hours for which to create plots on a given day. par_op is an output from \code{graphics::par}. coastline is an object used to plot coastline (see \code{\link[WeStCOMSExploreR]{plot2dfield}}); and zlab, zlab_line and vector_scale are vectors that define the labels on the z axis, their distance from the z axis and the scale of the arrows used to plot vectors; for each inputted variable. Other graphical parameters are not variable specific and passed as additional arguments outside of this list (see below).
#' @param compute_summary_stats A logical input defining whether or not summary statistics should be calculated.
#' @param summary_stats_param A list of parameters necessary to calculate summary statistics. This list should contain elements with the following names: hours4stats, row_specific and funs. See \code{\link[WeStCOMSExploreR]{summarise2dfield}}.
#' @param parallelise A character specifying whether or not to parallise over variables \code{"vars"} or dates, \code{"date_name"}. This is only applicable if a cluster is supplied (see below).
#' @param cl (optional) A cluster objected created by the parallel package. If supplied, the algorithm is implemented in parallel. Note that the connection with the cluster is stopped within the function.
#' @param pass2varlist A list of character vector of names of objects to export to be passed to the \code{varlist} argument of \code{\link[parallel]{clusterExport}}. This is required if you specify some function arguments via objects, rather than directly. These objects must be located in the global environment.
#' @param ... Additional graphical parameters that can be passed to \code{\link[WeStCOMSExploreR]{plot2dfield}} and will affect all plots.
#'
#' @return If \code{makeplot = TRUE}, the function will produce plots, either saved to file or displayed (the latter is only possible if \code{cl = NULL}). If \code{compute_summary_stats = TRUE}, the function will also return a list of dataframes, with one element for each environmental variable. Each dataframe the following columns: a date, hour and a column for each summary statistic specified.
#'
#' @seealso \code{\link[WeStCOMSExploreR]{build_mesh}}, \code{\link[WeStCOMSExploreR]{summarise2dfield}}, \code{\link[WeStCOMSExploreR]{plot2dfield}}
#'
#' @examples
#'
#' # Define dataframe
#' field2d <- data.frame(cov2d = c("temp",
#'                                 "tidal_elevation",
#'                                 "wind_velocity"),
#'                      dim = c("3d", "2d", "2d"),
#'                      cov2dlayer =  c(1, 1, 1),
#'                      resolved_at = c("node", "node", "element"),
#'                      mesh_type = c("element", "element", "node"),
#'                      extension = c(".mat", ".mat", ".RData"),
#'                      vector_field = c(FALSE, FALSE, TRUE)
#'                      )
#'
#' # Define directory from which to load data
#' dir2load <- system.file("WeStCOMS_files/", package = "WeStCOMSExploreR", mustWork = TRUE)
#'
#' # Define directory to save plots
#' dir2save <- paste0(dir2load, "pngs/")
#' dir.create(dir2save) # create png folder
#' create_wcdirs(dir2save, paste0(field2d$cov2d, "/")) # add folder within this for each variable
#'
#' # Use dataframe to examine outputs over space and time:
#' WeStCOMSExploreR::explore(
#'   # Define arguments relating to data input...
#'     field2d = field2d,
#'     mesh_around_nodes = WeStCOMSExploreR::dat_mesh_around_nodes,
#'     dataID_node = WeStCOMSExploreR::dat_nodexy$node_id,
#'     mesh_around_elements = WeStCOMSExploreR::dat_mesh_around_elements,
#'     dataID_element = WeStCOMSExploreR::dat_trinodes$element_id,
#'     dir2load = dir2load,
#'     date_name = c("160301", "160302"),
#'     # If you want to compute sun angle, this is how you need to specify the list...
#'     # ... Just change the objects supplied to each list element.
#'     compute_sun_angle = TRUE,
#'     sun_angle_param = list(nodexy = WeStCOMSExploreR::dat_nodexy,
#'     tz = "UTC",
#'     hours = 1:2,
#'     degrees = TRUE,
#'     dir2save = paste0(dir2load, "sun_angle/"),
#'     verbose = FALSE),
#'     # To make plots, you need to supply a list in the following format
#'     # Again, just change the objects supplied to each element:
#'     makeplot = TRUE,
#'     plot_param = list(hours4plots = 1,
#'     par_op = par(oma = c(3, 3, 3, 7)),
#'     coastline = WeStCOMSExploreR::dat_coast_around_oban,
#'     zlab = c(expression(paste("Temperature (", degree, ")")),
#'     "Tidal Elevation (m)",
#'     expression(paste("Wind Velocity (m", s^-1, ")")),
#'     expression(paste("Sun Angle (", degree, ")"))),
#'     zlab_line = c(3, 3, 3, 3),
#'     vector_scale = c(NA, NA, 0.0008, NA),
#'     # Note that if dir2save = NULL, the plots will be displayed
#'     # ... and not saved to file.
#'     dir2save = dir2save
#'     ),
#'     # To compute summary statistics, again, you need to supply the following list:
#'     compute_summary_stats = TRUE,
#'     summary_stats_param = list(hours4stats = 1:2,
#'     row_specific = TRUE,
#'     funs = list(mean = mean, min = min, max =  max)),
#'     # Options for parallelisation:
#'     parallelise = "vars",
#'     cl = parallel::makeCluster(2L),
#'     pass2varlist = list("dir2load", "dir2save")
#'     )
#'
#' # view files
#' list.files(dir2save, recursive = TRUE)
#'
#' @author Edward Lavender
#' @export

################################################
################################################
#### explore

explore <-
  function(
    field2d,
    mesh_around_nodes,
    dataID_node,
    mesh_around_elements,
    dataID_element,
    dir2load,
    date_name,

    compute_sun_angle = FALSE,
    sun_angle_param = list(),

    makeplot = TRUE,
    plot_param = list(),

    compute_summary_stats = TRUE,
    summary_stats_param = list(),

    parallelise = "vars",
    cl = NULL,
    pass2varlist #
    ,...
  ){


    ################################################
    #### Set up

    #### Define date
    date <- date_name(date_name, define = "date")
    dndf <- data.frame(date = date, date_name = date_name)

    #### Define unique mesh IDs for which we have data and in the mesh
    # mesh IDs for which we have data:
    dataID_element <- as.numeric(as.character(dataID_element))
    dataID_node <- as.numeric(as.character(dataID_node))
    # mesh IDs: it is for these ids that we will extract fvcom model outputs and plot them:
    element_ids <- as.numeric(as.character(mesh_around_nodes$ID))
    prism_ids <- as.numeric(as.character(mesh_around_elements$ID))

    #### Create a blank matrix in which to store sun angles
    if(compute_sun_angle){
      # Define a matrix in which we'll store (and then save) sun angles for every hour on a given day
      sun_angle_mat <- matrix(NA, nrow = length(sun_angle_param$hours), ncol = nrow(sun_angle_param$nodexy))
    }

    #### Define a list for summary statistics
    if(compute_summary_stats){
      #### Define a list in which we'll store a dataframe of summary statistics
      # sls <- list()
      #### Define a template dataframe in which to store summary statistics for any given variable
      sdf_tmp <- expand.grid(date_name = dndf$date_name, hour = summary_stats_param$hours4stats)
      sdf_tmp <- sdf_tmp[, c("date_name", "hour")]
      # Add dates using matching with the dataframe defined above (faster):
      sdf_tmp$date <- dndf$date[match(sdf_tmp$date_name, dndf$date_name)]
      # Define timestamps:
      sdf_tmp$timestamp <- as.POSIXct(sdf_tmp$date, tz = "UTC") + (sdf_tmp$hour * 60 * 60)
      # Calculate the number of columns prior to adding columnns for statistics...
      ncol1 <- ncol(sdf_tmp)
      # Add other columns to hold statistics
      statistics_names <- names(summary_stats_param$funs)
      for(statistic in statistics_names){
        sdf_tmp[, statistic] <- NA
      }
      # Recheck number of columsn
      ncol2 <- ncol(sdf_tmp)
    } # close if(compute_summary_stats){

    #### Define coordinates for vector field arrows
    if(!is.null(mesh_around_elements)){
      # Vector fields are resolved at elements
      # So, we'll use the mesh_around_elements object
      # ... which is based on prisms (i.e. prism ids)
      # For each prism id, we need to extract its coordinates:
      # The quickest way of doing this is to extract the coordinates from the mesh:
      element_xy <- sp::coordinates(mesh_around_elements)
    }

    #### Define cluster appropriately
    if(parallelise == "vars"){
      cl1 <- cl
      cl2 <- NULL
    } else if(parallelise == "date_name"){
      cl1 <- NULL
      cl2 <- cl
    } else{
      cl1 <- NULL
      cl2 <- NULL
    }


    #####
    if(!is.null(cl1) | !is.null(cl2)){
      varlist_fns <- list("summarise2dfield",
                          "compute_sun_angle_field",
                          "plot2dfield")
      varlist <- append(varlist_fns, pass2varlist)

    }

    if(!is.null(cl1)){
      parallel::clusterExport(cl = cl1, varlist = varlist)
    } else if(!is.null(cl2)){
      parallel::clusterExport(cl = cl2, varlist = varlist)
    }






    ################################################
    #### Loop over each environmental variable

    sls <-
      pbapply::pblapply(
        cl = cl1,
        X = 1:nrow(field2d),
        FUN = function(i){

          #### Define variable, and associated properties
          ev <- field2d$cov2d[i]
          evl <- field2d$cov2dlayer[i]
          evdim <- field2d$dim[i]
          extension <- field2d$extension[i]
          vector_field <- field2d$vector_field[i]
          print(paste0("For environmental variable", ev, "...."))

          # Variable specific plot parameters
          if(makeplot){
            zlab <- plot_param$zlab[i]
            zlab_line <- plot_param$zlab_line[i]
            vector_scale <- plot_param$vector_scale[i]
          }


          # Specify the function we'll use to load the file
          # (This depends on whether its a .mat or .RData file):
          if(extension == ".mat"){
            readFun <- R.matlab::readMat
          } else if(extension == ".RData"){
            readFun <- readRDS
          }

          #### Print statement:
          # cat(paste0("\n Calculating summary statistics and/or creating plots for environmental variable ", i, "/", nrow(field2d), "...\n"))

          #### Define mesh:
          # Define the appropriate mesh type for that variable
          # ... and the ids of the polygons in that mesh.
          if(field2d$mesh_type[i] == "element"){
            mesh <- mesh_around_nodes
            mesh_ids <- element_ids
            data_ids <- dataID_node
          } else {
            mesh <- mesh_around_elements
            mesh_ids <- prism_ids
            data_ids <- dataID_element
          }

          #### Copy template dataframe
          if(compute_summary_stats & !vector_field){
            sdf <- sdf_tmp
            sdf$cov2d <- ev
          }



          #### For every day...
          print("Looping over every day to calculate summary statistics and/or make plots...")
          sdf <-
            pbapply::pblapply(
              cl = cl2,
              X = 1:length(date_name),
              FUN = function(day){

                #### Extract date and date_name
                date_current <- dndf$date[day]
                date_name_current <- dndf$date_name[day]

                #### load file and extract data
                if(!vector_field){
                  # For all variables, except sun angle if we've chosen to compute that variable,
                  # ... we can load WeStCOMS files...
                  if(ev != "sun_angle" | ev == "sun_angle" && compute_sun_angle == FALSE){
                    dirfile <- paste0(dir2load, ev, "/", date_name_current, extension)
                    dat <- readFun(dirfile)
                    if(extension == ".mat"){
                      dat <- dat$data
                    }

                    # Else, if we're dealing with sun_angle and this needs to be computed,
                    # Then we'll compute this using compute_sun_angle_field():
                  } else if(ev == "sun_angle" & compute_sun_angle){
                    # Define matrix
                    sun_angle_mat2save <-
                      compute_sun_angle_field(nodexy = sun_angle_param$nodexy,
                                            date = date_current,
                                            date_name = date_name_current,
                                            tz = sun_angle_param$tz,
                                            hours = sun_angle_param$hours,
                                            sun_angle_mat = sun_angle_mat,
                                            degrees = sun_angle_param$degrees,
                                            dir2save = sun_angle_param$dir2save,
                                            verbose = sun_angle_param$verbose
                      )

                    # Define as dat for loop continuation
                    dat <- sun_angle_mat2save

                  } # Close else if(ev == "sun_angle" & compute_sun_angle){

                  #### If we're dealing with a vector field, we need to load two files:
                } else{
                  if(stringr::str_detect(string = ev, pattern = "wind")){
                    # Define u and v files
                    udirfile <- paste0(dir2load, "uwind_speed", "/", date_name_current, ".mat")
                    vdirfile <- paste0(dir2load, "vwind_speed", "/", date_name_current, ".mat")
                  } else if(stringr::str_detect(string = ev, pattern = "current")){
                    # Define u and v files
                    udirfile <- paste0(dir2load, "uvelocity", "/", date_name_current, ".mat")
                    vdirfile <- paste0(dir2load, "vvelocity", "/", date_name_current, ".mat")
                  }

                  # Define u and v data
                  ufvcom <- R.matlab::readMat(udirfile)
                  udat   <- ufvcom$data
                  vfvcom <- R.matlab::readMat(vdirfile)
                  vdat   <- vfvcom$data
                } # close else{ if we're dealing with a vector field...

                #### Extract data from model outputs:
                if(!vector_field){
                  if(evdim == "3d"){
                    datsbt <- dat[, evl, ]
                  } else{
                    datsbt <- dat
                  }
                } else if(vector_field){
                  if(evdim == "3d"){
                    udatsbt <- udat[, evl, ]
                    vdatsbt <- vdat[, evl, ]
                  } else{
                    udatsbt <- udat
                    vdatsbt <- vdat
                  }
                }

                #### Link column numbers, data IDs and mesh IDs
                # Define data: use datsbt or udatsbt for vector fields (only need to use one data source)
                if(!vector_field){ linkdat <- datsbt } else{ linkdat <- udatsbt }
                # Define a dataframe of column numbers in the model output
                # ... and corresponding IDs that they correspond to:
                IDlink <- data.frame(coln = 1:ncol(linkdat), data_ids = data_ids)
                # Add the IDs of the mesh (for which we'll actually do plotting) to this:
                IDlink$mesh_ids <- mesh_ids[match(IDlink$data_ids, mesh_ids)]
                # Idenbtify the column numbers which correspond to the mesh ids
                # ... for which we want data
                cols2extract <- IDlink$coln[!is.na(IDlink$mesh_ids)]

                # Extract a subset of nodes based on the mesh using indexing if requested
                # The user supplied all the IDs of the nodes at which they have calculated data
                # Those IDs correspond to columns 1:ncol(datsbt)
               # if(thin_using_mesh){
                  if(!vector_field){
                    datsbt <- datsbt[, cols2extract]
                  } else{
                    udatsbt <- udatsbt[, cols2extract]
                    vdatsbt <- vdatsbt[, cols2extract]
                  }
               # } # close  if(thin_using_mesh){


                #### If the user has selected to calculate summary statistics
                # ...(which is only possible for non vector fields):
                if(compute_summary_stats & !vector_field){
                  # Subset datsbt further for hours of interest:
                  datsbt4stats <- datsbt[summary_stats_param$hours4stats, ]
                  # Use summarise2dfield to calculate summary statistics
                  sy <- summarise2dfield(data = datsbt4stats,
                                         row_specific = summary_stats_param$row_specific,
                                         funs = summary_stats_param$funs)
                  # Identify the rows in the sdf_tmp dataframe which have been specified
                  # ... for the date_name_current:
                  rows <- which(sdf_tmp$date_name == date_name_current)
                  # Add summary statistics in the appropriate positions
                  sdf_tmp_tz_current <- sdf_tmp[rows, ]
                  sdf_tmp_tz_current <- sdf_tmp_tz_current[, (ncol1+1):ncol2] <- sy
                  sdf_tmp_tz_current$hour <- summary_stats_param$hours4stats
                  sdf_tmp_tz_current$date_name <- date_name_current
                } # close if(compute_summary_stats & !vector_field){


                #### If the user has selected to save plots...
                if(makeplot){

                  #### for every hour...
                  hour <- NULL
                  for(hour in plot_param$hours4plots){

                    #### Extract data required (i.e. for specific hour)
                    # ... and create 'data' object for plot2dfield function
                    # Note that we have already ensured that mesh ids and datsbt
                    # ... correspond to each other by using cols2extract
                    if(!vector_field){
                      datsbt <- datsbt[hour, ]
                      data <- data.frame(ID = mesh_ids, fvcom = datsbt)
                    } else{
                      udatsbt <- udatsbt[hour, ]
                      vdatsbt <- vdatsbt[hour, ]
                      data <- list(udata = data.frame(ID = mesh_ids, fvcom = udatsbt),
                                   vdata = data.frame(ID = mesh_ids, fvcom = vdatsbt))
                    }

                    #### Set up plot to save (in appropriate directory)
                    if(!is.null(plot_param$dir2save)){
                      png_name <- paste0(plot_param$dir2save, ev, "/", date_name_current, "_", hour, ".png")
                      grDevices::png(png_name, height = 10, width = 10, units = "in", res = 300)
                    }

                    # Define plotting margins
                    plot_param$par_op

                    # Plot graph
                    plot2dfield(coastline = plot_param$coastline,
                                mesh = mesh,
                                vector_field = vector_field,
                                data = data,
                                zlab = zlab,
                                zlab_line = zlab_line,
                                arrow_scale = vector_scale,
                                main = paste0(date_current, " ", hour, ":00"),...)

                    #### Save figure
                    if(!is.null(plot_param$dir2save)){
                      grDevices::dev.off()
                    }

                  } # close  for(hour in hours){
                } # close if(makeplot){

                if(compute_summary_stats & !vector_field){
                  return(sdf_tmp_tz_current)}

              }) # close pblapply over date_name



          #### Add the complete summary stats dataframe to the list:
          if(compute_summary_stats & !vector_field){
            #sls[[i]] <- sdf
            sdf <- dplyr::bind_rows(sdf)
            sdf <- sdf[, c("date_name", "hour", names(summary_stats_param$funs))]
            return(sdf)
          }

        }) # close pblapply over environmental variables

    if(!is.null(cl1)){
      parallel::stopCluster(cl1)
    } else if(!is.null(cl2)){
      parallel::stopCluster(cl2)
    }

    if(compute_summary_stats){
      names(sls) <- as.character(field2d$cov2d)
      sls <- plyr::compact(sls)
      return(sls)
    }



  } # close function


#### End of code.
################################################
################################################
