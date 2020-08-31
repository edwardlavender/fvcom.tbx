#' @title Compute new hydrodynamic fields
#' @description This function computes new hydrodynamic fields from FVCOM outputs, such as thermocline strength. To compute new fields, the user must specify a list of file names, each file consisting of an environmental array from which new fields can be computed. These are loaded sequentially or in parallel into R by the function. For each array or combination of arrays, a user-supplied function is applied to compute a new field. The resultant field is either returned or saved.
#'
#' @param source_file A list. Each element should comprise a vector of complete file paths (including the directory path, the file name and extension) containing all environmental arrays containing predictions for a given day that are required to compute a new field. Often, only one file is required to compute a new field (e.g., thermocline strength can be computed from temperature predictions). However, sometimes, multiple files may be required in each list element (e.g., current direction is computed from u and v component vector arrays).
#' @param sink_file (optional) A vector of names (excluding file extensions) for the arrays that are derived for each source file/file combination. This is required if files are to be saved (see \code{dir2save}).
#' @param read_fvcom A function which is used to load files (and, if necessary, extract the WeStCOMS array from the loaded object). The function should take a single single argument, the file connection, as input, and load the file. The default is \code{function(con) = R.matlab::readMat(con)$data} but other functions may be required depending on the format of FVCOM files (e.g. \code{\link[base]{readRDS}}).
#' @param calc A function which, as input, takes a list of the array(s) that are required to compute a new field for a particular day and computes this new field. See \code{\link[WeStCOMSExploreR]{calc_thermocline}}, \code{\link[WeStCOMSExploreR]{calc_speed}} and \code{\link[WeStCOMSExploreR]{calc_direction}} for examples. A custom function can be provided in the same vein.
#' @param cl (optional) A cluster objected created by the parallel package. If supplied, the algorithm is implemented in parallel. Note that the connection with the cluster is stopped within the function.
#' @param varlist A list of character vector of names of objects to export to be passed to the \code{varlist} argument of \code{\link[parallel]{clusterExport}}.
#' @param dir2save (optional) A string which defines the directory in which to save files. If provided, the function will save a .rds file with the specified name in the specified directory (see \code{\link[base]{saveRDS}}). If not provided, the function will return a list of arrays, with one element for each file/file combination.
#' @param verbose A logical input which defines whether or not to display messages to the console detailing function progress.
#' @param ... Other arguments passed to the user-supplied \code{calc} function.
#'
#' @return For each file/file combination, the function computes a new field comprising predictions for each cell in the original file(s). Arrays are either returned in a list to the workspace (with one element for each file) or, if \code{dir2save} is provided, saved to a user-supplied directory as a .rds file.
#'
#' @examples
#' #### Define overall path from which to load FVCOM files
#' path <- system.file("WeStCOMS_files/",
#'                     package = "WeStCOMSExploreR", mustWork = TRUE)
#'
#' #### Example (1): Compute a new field derived from only one type of FVCOM file
#' # ... using default options.
#' # ... E.g., thermocline strength, derived from temperature fields.
#'
#' ## Step A: Define a list of source files:
#' # Define the dates for which we want FVCOM files:
#' dates <- as.Date(c("2016-03-01", "2016-03-02"))
#' # Define corresponding file patterns:
#' date_names <- date_name(dates, define = "date_name")
#' # Define a list of files from which we'll compute thermocline stength:
#' files <- list.files(paste0(path, "temp/"),
#'                     pattern = paste0(date_names, collapse = "|"),
#'                     full.names = TRUE)
#' # The list should have one element comprising all the files required for each date:
#' files <- lapply(files, function(file) file)
#' files
#'
#' ## Step B: Define a function to compute thermcline strength:
#' # We could use the calc_thermocline_strength() helper function
#' # ... but for transparency we'll define this explicitly here:
#' calc_thermocline_strength_helper <-
#'   function(l){
#'     # The function takes in a list of all necessary files for a specific day
#'     # ... to compute thermocline strength. Here, there is only one element
#'     # ... because we only need the temperature predictions.
#'     fvcom_array <- l[[1]]
#'     # Here, we'll calculate thermocline strength as the difference in temp
#'     # ... between the min and max layers:
#'     n_layer <- dim(fvcom_array)[2]
#'     fvcom_field <- fvcom_array[, c(1, n_layer), ]
#'     fvcom_field <- fvcom_field[, 1, ] - fvcom_field[, 2, ]
#'     return(fvcom_field)
#'   }
#'
#' #### Step 3: Implement compute_field_from_fvcom()
#' thermoline_ls <-
#'   compute_field_from_fvcom(source_file = files,
#'                            sink_file = date_names,
#'                            read_fvcom =
#'                              function(con) R.matlab::readMat(con)$data,
#'                            calc = calc_thermocline_strength_helper
#'   )
#' utils::str(thermoline_ls)
#'
#' #### Example (2) Save files on the fly by using dir2save argument
#' dir2save <- paste0(path, "thermocline_strength/")
#' dir.create(dir2save)
#' thermoline_ls <-
#'   compute_field_from_fvcom(source_file = files,
#'                            sink_file = date_names,
#'                            read_fvcom =
#'                               function(con) R.matlab::readMat(con)$data,
#'                            calc = calc_thermocline_strength_helper,
#'                            dir2save = dir2save)
#' # NULL elements are returned by the function because
#' # ... we have saved files instead:
#' utils::str(thermoline_ls)
#' list.files(dir2save)
#'
#' #### Example (3) Compute fields for each file in parallel
#' thermoline_ls <-
#'   compute_field_from_fvcom(source_file = files,
#'                            sink_file = date_names,
#'                            read_fvcom = function(con) R.matlab::readMat(con)$data,
#'                            calc = calc_thermocline_strength_helper,
#'                            cl = parallel::makeCluster(2L),
#'                            varlist = "calc_thermocline_strength_helper")
#' utils::str(thermoline_ls)
#'
#' #### Example (4) Compute fields requiring multiple input files for each day
#' # ... E.g., wind speed, derived from u and v vectors.
#'
#' ## Step A: Define a list of source files.
#' # Define the path to the u and v files:
#' path_u <- paste0(path, "uwind_speed")
#' path_v <- paste0(path, "vwind_speed")
#' # Define the source files:
#' source_u <- list.files(path_u, full.names = TRUE)
#' sourve_v <- list.files(path_v, full.names = TRUE)
#' # Define a list comprising u and v files with one element for each day:
#' files   <- mapply(c, source_u, sourve_v, SIMPLIFY = FALSE)
#' utils::str(files)
#'
#' ## Step B: Compute wind speeds using calc_speed() helper function:
#' wind_speed_ls <-
#'   compute_field_from_fvcom(source_file = files,
#'                            sink_file = date_names,
#'                            read_fvcom = function(con) R.matlab::readMat(con)$data,
#'                            calc = calc_speed
#'   )
#' utils::str(wind_speed_ls)
#'
#' @author Edward Lavender
#' @export
#'

compute_field_from_fvcom <-
  function(
    source_file = list(),
    sink_file = NULL,
    read_fvcom = function(con) R.matlab::readMat(con)$data,
    calc,
    cl = NULL,
    varlist = NULL,
    dir2save = NULL,
    verbose = TRUE,...){

    #### Checks
    if(verbose){
      cat("WeStCOMSExploreR::compute_field_from_fvcom() called...\n")
      cat("Step 1/2: Checking user inputs...\n")
    }
    ## Check dir2save
    if(!is.null(dir2save)) {
      # Check directory
      dir2save <- check_dir(arg = "dir2save", input = dir2save, check_slash = TRUE)
      # Files and names should be of the same length
      check_length(arg = "sink_file", input = sink_file, req_length = length(source_file), req_arg = "source_file")
    }

    ## Exclude corrupt files
    # dat <- exclude_corrupt(dat, corrupt)

    ## Exclude any dates without associated files
    # dat <- exclude_unavailable(dat, dir2load, pattern = paste0("*", extension))

    #### Loop over every file, load, define new field and save (if requested):
    if(verbose){
      if(is.null(dir2save)){
        cat("Step 2/2: Computing and returning thermocline fields...\n")
      } else {
        cat("Step 2/2: Computing and saving thermocline fields...\n")
      }
    }
    if(!is.null(cl)) parallel::clusterExport(cl = cl, varlist = varlist)
    lout <-
      pbapply::pblapply(1:length(source_file), cl = cl, function(i){

        ## Define file connections and file name
        if(!is.null(dir2save)) file_name <- sink_file[i]

        ## Load file(s)
        fvcom_array_list <- lapply(source_file[[i]], function(file_con) read_fvcom(file_con))

        ## Compute field from list of arrays using user-supplied function
        fvcom_field <- calc(fvcom_array_list,...)

        ## Save file if requested and return NULL or else return the file
        if(!is.null(dir2save)){
          file_save <- paste0(dir2save, file_name, ".rds")
          saveRDS(fvcom_field, file_save)
          return(NULL)
        } else{
          return(fvcom_field)
        }

      })
    if(!is.null(cl)) parallel::stopCluster(cl)

    #### Return a list of files
    names(lout) <- sink_file
    return(lout)

  }
