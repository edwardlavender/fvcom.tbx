#' @title Explore WeStCOMS model outputs in 2d
#' @description This function computes summary statistics and/or maps environmental conditions for multiple environmental variables and/or timepoints. The user passes the function a dataframe specifying the variables to be included, their properties, and the directory of the WeStCOMS files from which they can be loaded. For each variable, the function loads in the WeStCOMS files for each date in turn, computes summary statistics and/or plots a map for the hours specified. Iterative loading in of the files is necessary given the size/memory requirements of WeStCOMS files.
#'
#' @param field2d A dataframe that defines the variables and their properties for which the outputs will be explored. This must contain the following columns: 'cov2d', a character vector of variable names which correspond to the names of the folders in which the model outputs are located (see \code{dir2load}, below); 'mesh_type', a character vector of the corresponding mesh type ("element" or "node" across which that variable is structured); 'extension', a character vector of the extension of the model outputs (e.g. ".mat" or ".rds" for outputs created by \code{\link[WeStCOMSExploreR]{compute_field_from_fvcom}}); 'vector_field', a logical vector defining whether or not that variable is a scalar (FALSE) or vector field (TRUE). For vector fields, the letters "wind" or "current" must be within the variable name in order for the algorithm to determine whether data should be loaded from the "uwind_speed" and "vwind_speed" or "uvelocity" and "vvelocity" folders in \code{dir2load} (see below). (see \code{\link[WeStCOMSExploreR]{extract}}). may contain dir2save
#' @param dat_ls A named list of dataframes, with one element for each \code{field2d$cov2d}. Each element should comprise a dataframe which defines the WeStCOMS file date names, hours, and mesh (node) IDs for which layer depths should be calculated (see \code{\link[WeStCOMSExploreR]{extract}}).
#' @param match_hour A dataframe with two integer columns named 'hour' and 'index' which defines the index in WeStCOMS files (i.e. the row) which corresponds to each hour (see \code{\link[WeStCOMSExploreR]{extract}}).
#' @param match_layer A dataframe with two integer columns named 'layer' and 'index' which defines the index in WeStCOMS files (i.e. the column) which corresponds to each layer (see \code{\link[WeStCOMSExploreR]{extract}}).
#' @param match_mesh_around_nodes A dataframe with two columns named 'mesh' and 'index' which defines the index in WeStCOMS files (columns or sheets for 2d and 3d arrays respectively) which corresponds to each node cell (see \code{\link[WeStCOMSExploreR]{extract}}).
#' @param match_mesh_around_elements A dataframe with two columns named 'mesh' and 'index' which defines the index in WeStCOMS files (columns or sheets for 2d and 3d arrays respectively) which corresponds to each element cell (see \code{\link[WeStCOMSExploreR]{extract}}).
#' @param corrupt A vector of numbers, representing WeStCOMS date names, which define corrupt files (see \code{\link[WeStCOMSExploreR]{extract}}).
#' @param read_fvcom_ls A named list of functions, with one element for each \code{field2d$cov2d}. Each element should contain a function is used to load files for that variable (see \code{\link[WeStCOMSExploreR]{extract}}).
#' @param dir2load_ls A named list of directories, with one element for each \code{field2d$cov2d}. Each element should be a string or a vector of strings defines the directory from which to load WeStCOMS files (see \code{\link[WeStCOMSExploreR]{extract}}).
#' @param mesh_around_nodes A mesh, created by \code{\link[WeStCOMSExploreR]{build_mesh}}, that surrounds nodes. This is required for plotting variables that are resolved at nodes.
#' @param mesh_around_elements A mesh, created by \code{\link[WeStCOMSExploreR]{build_mesh}}, that surrounds elements. This is required for plotting variables that are resolved at elements.
#' @param make_plot A logical input defining whether or not to make plots.
#' @param plot_param A list of parameters required to make plots. These parameters either wrap around, or are passed as arguments, to \code{\link[WeStCOMSExploreR]{plot2dfield}}. The list needs to contain elements with the names: 'hours4plots', 'par_op', 'coastline', 'zlab', 'zlab_line', 'vector_scale' and 'dir2save'. 'hours4plots' is a numeric vector of all the hours for which to create plots on a given day. 'par_op' is an output from \code{\link[graphics]{par}}. 'coastline' is an object used to plot coastline (see \code{\link[WeStCOMSExploreR]{plot2dfield}}); and 'zlab', 'zlab_line' and 'vector_scale' are vectors that define the labels on the z axis, their distance from the z axis and the scale of the arrows used to plot vectors; for each inputted variable. Other graphical parameters are not variable specific and passed as additional arguments outside of this list (see below).
#' @param png_param A named list of parameters passed to \code{\link[grDevices]{png}} to customise plots saved as .png files.
#' @param compute_summary_stats A logical input defining whether or not summary statistics should be calculated.
#' @param summary_stats_param A list of parameters necessary to calculate summary statistics. This list should contain elements with the following names: 'hours4stats', 'row_specific' and 'funs'. See \code{\link[WeStCOMSExploreR]{summarise2dfield}}.
#' @param parallelise A character specifying whether or not to parallel over variables \code{"vars"} or dates, \code{"date_name"}. This is only applicable if a cluster is supplied (see below).
#' @param cl (optional) A cluster objected created by the parallel package. If supplied, the algorithm is implemented in parallel. Note that the connection with the cluster is stopped within the function.
#' @param pass2varlist A list of character vector of names of objects to export to be passed to the \code{varlist} argument of \code{\link[parallel]{clusterExport}}. This is required if you specify some function arguments via objects, rather than directly. These objects must be located in the global environment.
#' @param verbose A logical input which defines whether or not to display messages to the console detailing function progress.
#' @param ... Additional graphical parameters that can be passed to \code{\link[WeStCOMSExploreR]{plot2dfield}} and will affect all plots.
#'
#' @return If \code{make_plot = TRUE}, the function will produce plots, either saved to file or displayed (the latter is only possible if \code{cl = NULL}). If \code{compute_summary_stats = TRUE}, the function will also return a list of dataframes, with one element for each environmental variable. Each dataframe the following columns: a date, hour and a column for each summary statistic specified.
#'
#' @seealso \code{\link[WeStCOMSExploreR]{build_mesh}}, \code{\link[WeStCOMSExploreR]{summarise2dfield}}, \code{\link[WeStCOMSExploreR]{plot2dfield}}
#'
#' @examples
#' ############################
#' ############################
#' #### Example (1): Implement explore() with a single variable
#'
#' #### Define field2d, a dataframe which defines variables and details
#' # ... needed for extraction.
#' field2d <- data.frame(cov2d = "temp",
#'                       mesh_type = "element",
#'                       extension = ".mat",
#'                       vector_field = FALSE,
#'                       dir2save = "/Users/el72/Desktop/")
#'
#' #### Define dat_ls, a list of dataframes which defines
#' # ... the specific FVCOM outputs we'll extract.
#' # We'll extract all predictions for two days from example files for the top layer:
#' timestamp <- as.POSIXct(c("2016-03-01", "2016-03-02"), tz = "UTC")
#' dat <- expand.grid(timestamp = timestamp, mesh_ID = dat_nodexy$node_id)
#' dat$date_name <- date_name(dat$timestamp)
#' dat$hour <- lubridate::hour(dat$timestamp)
#' dat$mesh_ID <- dat_nodexy[1:nrow(dat), "node_id"]
#' dat$layer <- 1
#' dat <- dat[, c("date_name", "hour", "layer", "mesh_ID")]
#' dat_ls <- list(temp = dat)
#'
#' #### Define other general parameters for extract()
#' match_hour <- data.frame(hour = 0:1, index = 1:2)
#' # match_layer <- NULL # not needed in this example
#' match_mesh <-
#'   data.frame(mesh = dat_nodexy$node_id, index = 1:length(dat_nodexy$node_id))
#' match_mesh_around_nodes <-
#'   data.frame(mesh = dat_nodexy$node_id, index = 1:length(dat_nodexy$node_id))
#' match_mesh_around_elements <- NULL # not needed in this example
#' # corrupt <- NULL # not needed in this example
#'
#' #### Define a NAMED list of functions to load files
#' read_fvcom_ls <- list(temp = function(con) R.matlab::readMat(con)$data)
#'
#' #### Define a NAMED list of directories used to load files
#' root <- system.file("WeStCOMS_files/",
#'                      package = "WeStCOMSExploreR", mustWork = TRUE)
#' dir2load_ls <- list(temp = paste0(root, "temp/"))
#'
#' #### Define mesh(es)
#' # We'll use the example data files.
#' mesh_around_nodes <- dat_mesh_around_nodes
#' # mesh_around_elements <- NULL
#'
#' #### Define plot parameters
#' make_plot <- TRUE
#' plot_param = list(hours4plots = unique(dat$hour)[1],
#'                   par_op = par(oma = c(3, 3, 3, 7)),
#'                   coastline = dat_coast_around_oban,
#'                   zlab = expression(paste("Temperature (", degree, ")")),
#'                   zlab_line = 3,
#'                   vector_scale = NA
#' )
#' # png_param <- list() # not needed in this example
#'
#' #### Define compute stats parameters
#' compute_summary_stats <- TRUE
#' summary_stats_param <-
#'   list(row_specific = TRUE,
#'        funs = list(mean = function(x) mean(x, na.rm = TRUE),
#'                    min = function(x) min(x, na.rm = TRUE))
#'   )
#'
#' #### Parallelise param
#' # We'll set inside the function.
#' # cl <- NULL
#' # pass2varlist = list()
#' # parallelise <- "vars"
#' # verbose <- TRUE
#'
#' #### Implement explore() for a single variable
#' explore(
#'   field2d = field2d,
#'   dat_ls = dat_ls,
#'   match_hour = data.frame(hour = 0:23, index = 1:24),
#'   match_layer = NULL,
#'   match_mesh_around_nodes = match_mesh_around_nodes,
#'   match_mesh_around_elements = NULL,
#'   corrupt = NULL,
#'   read_fvcom_ls = read_fvcom_ls,
#'   dir2load_ls = dir2load_ls,
#'   mesh_around_nodes = dat_mesh_around_nodes,
#'   mesh_around_elements = NULL,
#'   make_plot = TRUE,
#'   plot_param = plot_param,
#'   png_param = list(),
#'   compute_summary_stats = TRUE,
#'   summary_stats_param = summary_stats_param,
#'   parallelise = "vars",
#'   cl = NULL,
#'   pass2varlist = NULL,
#'   verbose = TRUE
#' )
#'
#' @author Edward Lavender
#' @export

################################################
################################################
#### explore

explore <-
  function(
    field2d,
    dat_ls,
    match_hour = data.frame(hour = 0:23, index = 1:24),
    match_layer = NULL,
    match_mesh_around_nodes = NULL,
    match_mesh_around_elements = NULL,
    corrupt = NULL,
    read_fvcom_ls = list(),
    dir2load_ls = list(),
    mesh_around_nodes = NULL,
    mesh_around_elements = NULL,
    make_plot = TRUE,
    plot_param = list(),
    png_param = list(),
    compute_summary_stats = TRUE,
    summary_stats_param = list(),
    parallelise = "vars",
    cl = NULL,
    pass2varlist = NULL,
    verbose = TRUE,...
  ){


    ################################################
    ################################################
    #### Set up

    #### Initialisation
    if(verbose) {
      t1 <- Sys.time()
      cat("WeStCOMSExploreR::explore() called...\n")
      cat("Step 1: Set up...\n")
    }

    #### Check field2d
    ## Check object classes and values in field2d
    check_names(arg = "field2d",
                input = field2d,
                req = c("cov2d", "mesh_type", "extension", "vector_field"),
                extract_names = colnames,
                type = all
    )
    if(!all(field2d$mesh_type %in% c("node", "element"))){
      stop("field2d$mesh_type contains unsupported inputs. Only these inputs are allowed: 'node', 'element'.")
    }

    #### Check dat_ls
    check_class(arg = "dat_ls",
                input = dat_ls,
                to_class = "list",
                type = "stop")
    check_named_list(arg = "dat_ls", input = dat_ls, ignore_empty = FALSE)
    check_names(arg = "dat_ls", input = dat_ls, req = field2d$cov2d, extract_names = names, type = all)

    #### Check directories
    # Check dir2load_ls is named list
    check_class(arg = "dir2load_ls", input = dir2load_ls, to_class = "list", type = "stop")
    check_named_list(arg = "dir2load_ls", input = dir2load_ls, ignore_empty = FALSE)
    # Check dir2load_ls includes required names
    check_names(arg = "dir2load_ls", input = dir2load_ls, req = field2d$cov2d, extract_names = names, type = all)
    # Check dir2load_ls is correctly supplied
    dir2load_ls_names <- names(dir2load_ls)
    dir2load_ls <- lapply(1:length(dir2load_ls), function(i){
      ev_dirs <- sapply(1:length(dir2load_ls[[i]]), function(j){
        chk <- check_dir(arg = paste0("dir2load_ls[['", names(dir2load_ls)[[i]], "']][", j, "]"),
                         input = dir2load_ls[[i]][j],
                         check_slash = TRUE)
        return(chk)
      })
      return(ev_dirs)
    })
    names(dir2load_ls) <- dir2load_ls_names
    # Check dir2save is correctly supplied
    if(make_plot & rlang::has_name(field2d, "dir2save")){
      for(i in 1:nrow(field2d)){
        field2d$dir2save[i] <- check_dir(arg = paste0("field2d$dir2save[", i, "]"),
                                         input = field2d$dir2save[i],
                                         check_slash = TRUE)
      }
    }

    #### Check read_fvcom_ls list
    check_class(input = read_fvcom_ls, to_class = "list", type = "stop")
    check_named_list(input = read_fvcom_ls, ignore_empty = FALSE)

    #### Check plot_param list
    if(make_plot){
      check_class(input = plot_param, to_class = "list", type = "stop")
      check_named_list(input = plot_param, ignore_empty = FALSE)
      check_names(arg = "plot_param",
                  input = plot_param,
                  req = c("hours4plots", "par_op", "coastline", "zlab", "zlab_line", "vector_scale"),
                  extract_names = names,
                  type = all
      )
      for(i in 1:length(dat_ls)){
        if(!all(plot_param$hours4plots %in% dat_ls[[i]]$hour)){
          stop(paste0("All plot_param$hours4plots values should be within dat_ls[[,", i, "]]$hour."))
        }
      }
      check_class(input = png_param, to_class = "list", type = "stop")
      check_named_list(input = png_param, ignore_empty = TRUE)
    }

    #### Check compute_summary_stats list
    if(compute_summary_stats){
      check_class(input = summary_stats_param, to_class = "list", type = "stop")
      check_named_list(input = summary_stats_param, ignore_empty = FALSE)
      check_names(arg = "summary_stats_param",
                  input = summary_stats_param,
                  req = c("row_specific", "funs"),
                  extract_names = names,
                  type = all)
      check_named_list(input = summary_stats_param$funs)
    }

    #### Check parallelise
    if(!is.null(cl)) check_value(arg = "parallelise", input = parallelise, supp = c("date_name", "vars"), warn = TRUE)

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

    #### Cluster export
    # Define varlist
    if(!is.null(cl1) | !is.null(cl2)){
      varlist_fns <- list("extract",
                          "plot2dfield")
      varlist <- append(varlist_fns, pass2varlist)
    }
    # Cluster export
    if(!is.null(cl1)){
      parallel::clusterExport(cl = cl1, varlist = varlist)
    } else if(!is.null(cl2)){
      parallel::clusterExport(cl = cl2, varlist = varlist)
    }


    ################################################
    ################################################
    #### Loop over each environmental variable

    if(verbose) cat("Step 2: Looping over environmental variables...\n")
    sls <-
      pbapply::pblapply(
        cl = cl1,
        X = 1:nrow(field2d),
        FUN = function(i){


          ################################################
          #### Environmental-variable specific set up

          #### Testing
          # i <- 1

          #### Define variable, and associated properties
          if(verbose) cat(paste0("Step 2A: Isolating environmental variable '", field2d$cov2d[i], "'...\n"))
          # variable attributes
          ev <- field2d$cov2d[i]
          evl <- field2d$cov2dlayer[i]
          extension <- field2d$extension[i]
          vector_field <- field2d$vector_field[i]
          if(make_plot & rlang::has_name(field2d, "dir2save")) dir2save <- field2d$dir2save[i] else dir2save <- NULL

          #### Additional information for extract()
          dat <- dat_ls[[ev]]
          read_fvcom <- read_fvcom_ls[[ev]]
          ev_dir2load <- dir2load_ls[[ev]]

          #### Variable specific plot parameters
          if(make_plot){

            ## Define mesh:
            # Define the appropriate mesh type for that variable
            # ... and the ids of the polygons in that mesh.
            if(field2d$mesh_type[i] == "element"){
              mesh <- mesh_around_nodes
              match_mesh <- match_mesh_around_nodes
            } else if(field2d$mesh_type[i] == "node"){
              mesh <- mesh_around_elements
              match_mesh <- match_mesh_around_elements
            }

            ## Other param
            zlab <- plot_param$zlab[i]
            zlab_line <- plot_param$zlab_line[i]
            vector_scale <- plot_param$vector_scale[i]
          }


          ################################################
          ################################################
          #### Loop over every day and implement three steps:
          # ... 1) Extract model outputs
          # ... 2) Compute summary statistics
          # ... 3) Make plots

          if(verbose) cat("Step 2A: Looping over each day to extract model predictions, compute summary statistics and/or make plots...\n")
          sdf <-
            pbapply::pblapply(
              cl = cl2,
              X = unique(dat$date_name),
              FUN = function(day){

                #### Testing
                # day <- dat$date_name[1]


                ################################################
                #### Extract model outputs using extract()

                #### Subset dat for specific day
                # Daily approach implemented to avoid running out of memory with large datasets.
                if(verbose) cat(paste0("On day ", day, "...\n"))
                dat_sbt <- dat[dat$date_name %in% day, ]

                #### load file and extract data
                # Implement extract()
                if(verbose) cat("Step 2B(i): Preparing to extract model predictions...\n")
                dat_ls <-
                  lapply(ev_dir2load, function(con){
                    ex <- extract(dat = dat_sbt,
                                  match_hour = match_hour,
                                  match_layer = match_layer,
                                  match_mesh = match_mesh,
                                  corrupt = corrupt,
                                  read_fvcom = read_fvcom,
                                  dir2load = con,
                                  extension = extension,
                                  cl = NULL,
                                  pass2varlist = NULL,
                                  verbose = verbose)
                    return(ex)
                  })
                if(length(dat_ls) == 1) {
                  dat_sbt <- dat_ls[[1]]
                } else{
                  udat_sbt <- dat_ls[[1]]
                  vdat_sbt <- dat_ls[[2]]
                }


                ################################################
                #### Compute summary statistics

                #### If the user has selected to calculate summary statistics
                if(compute_summary_stats){

                  if(verbose) cat("Step 2B(ii): Computing summary statistics ...\n")

                  if(vector_field){
                    message(paste0("Summary statistics not computed for vector field '", ev, "'."))

                  } else{

                    #### Compute summary statistics for every hour, if specified:
                    if(summary_stats_param$row_specific){

                      sry_ls <- lapply(1:length(summary_stats_param$funs), function(i){
                        f <- summary_stats_param$funs[[i]]
                        f_name <- names(summary_stats_param$funs)[i]
                        sry_tbl <- tapply(dat_sbt$wc, list(date_name = dat_sbt$date_name, hour = dat_sbt$hour), f)
                        sry_dat <- data.frame(date_name = rownames(sry_tbl), hour = colnames(sry_tbl), stat = as.numeric(sry_tbl))
                        sry_dat[, f_name] <- sry_dat$stat
                        sry_dat$stat <- NULL
                        sry_dat$hour = as.integer(sry_dat$hour)
                        if(i == 1) return(sry_dat) else return(sry_dat[, 3, drop = FALSE])
                      })

                      #### Compute summary statistics across all hours, if specified:
                    } else{

                      sry_ls <- lapply(1:length(summary_stats_param$funs), function(i){
                        f <- summary_stats_param$funs[[i]]
                        f_name <- names(summary_stats_param$funs)[i]
                        sry_tbl <- tapply(dat_sbt$wc, date_name, f)
                        sry_dat <- data.frame(date_name = rownames(sry_tbl), stat = as.numeric(sry_tbl))
                        sry_dat[, f_name] <- sry_dat$stat
                        sry_dat$stat <- NULL
                        sry_dat$hour = as.integer(sry_dat$hour)
                        if(i == 1) return(sry_dat) else return(sry_dat[, 2, drop = FALSE])
                      })
                    }

                    #### Bind all summary statistics for the current day into one dataframe
                    sry <- dplyr::bind_cols(sry_ls)
                  }
                }


                ################################################
                #### Make plots for each day/hour

                if(make_plot){
                  if(verbose) cat("Step 2B(iii): Producing plots...\n")

                  #### Make plot for every hour specified
                  for(hour in plot_param$hours4plots){
                    if(verbose) cat(paste0("On hour ", hour, "...\n"))

                    #### Extract data required (i.e. for specific hour)
                    # ... and create 'data' object for plot2dfield function.

                    # Note that we have already ensured that mesh ids and datsbt
                    # ... correspond to each other by using cols2extract
                    if(!vector_field){
                      data <- data.frame(ID = dat_sbt$mesh_ID, fvcom = dat_sbt$wc)
                    } else{
                      data <- list(udata = data.frame(ID = udat_sbt$mesh_ID, fvcom = udat_sbt$wc),
                                   vdata = data.frame(ID = vdat_sbt$mesh_ID, fvcom = vdat_sbt$wc))
                      print(data)
                    }

                    #### Set up plot to save (in appropriate directory)
                    if(!is.null(dir2save)){
                      filename <- paste0(dir2save, "/", day, "_", hour, ".png")
                      png_out <- list(filename = filename)
                      if(length(png_param) > 0) png_out <- rlist::list.merge(png_out, png_param)
                      do.call(grDevices::png, png_out)
                    }

                    #### Define plotting margins
                    plot_param$par_op

                    # Plot graph
                    plot2dfield(coastline = plot_param$coastline,
                                mesh = mesh,
                                vector_field = vector_field,
                                data = data,
                                zlab = zlab,
                                zlab_line = zlab_line,
                                arrow_scale = vector_scale,
                                main = paste0(day, " ", hour, ":00"),...)

                    #### Save figure
                    if(!is.null(dir2save)) grDevices::dev.off()
                  }
                }

                #### Return list of summary statistics for that day.
                if(compute_summary_stats & !vector_field) return(sry)

              })


          ################################################
          #### Outputs for each environmental variables

          #### Bind summary statistics computed for each day
          sry <- dplyr::bind_rows(sdf)
          return(sry)


        }) # close pblapply over environmental variables

    #### Close cluster
    if(!is.null(cl1)){
      parallel::stopCluster(cl1)
    } else if(!is.null(cl2)){
      parallel::stopCluster(cl2)
    }

    #### Process summary statistics
    if(compute_summary_stats){
      # Define names
      names(sls) <- as.character(field2d$cov2d)
      # Compact list
      sls <- sls[which(!sapply(sls, is.null))]
    }

    #### Algorithm duration
    if(verbose){
      t2 <- Sys.time()
      tdiff <- round(difftime(t2, t1))
      cat(paste0("WeStCOMSExploreR::explore() algorithm duration approximately ", round(tdiff), " ",  methods::slot(tdiff, "units"), ".\n"))
    }

    #### Return outputs
    if(compute_summary_stats) return(sls)

  }


#### End of code.
################################################
################################################
