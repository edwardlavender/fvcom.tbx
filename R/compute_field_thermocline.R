#' @title Compute thermocline strength as a 2d field
#' @description This function computes thermocline strength as a new environmental field from FVCOM temperature predictions. To compute the thermocline strength, the user must specify a vector of file names, each comprising a 3 dimensional array of temperatures (for each hour x layer x node). These are loaded sequentially or in parallel into R by the function. For each temperature array, the function either returns or saves a 2 dimensional array in which each cell contains a metric of the thermocline strength on that hour in that node (see Details).
#'
#' @param source_file A vector of file names (including file extensions). Each file should refer to a 3d array comprising FVCOM temperature predictions for a given day, with dimensions corresponding to hours x layers x nodes. These file names are combined with \code{dir2load} (see below) and then loaded into R.
#' @param sink_file (optional) A vector of names (excluding file extensions) for the thermocline arrays that are derived for each \code{source_file}. This is required if files are to be saved (see \code{dir2save}).
#' @param dir2load A character string that defines the directory from which to load files.
#' @param read_fvcom A function which is used to load files (and, if necessary, extract the WeStCOMS array from the loaded object). The function should take a single single argument, the file connection, as input, and load the file. The default is \code{function(con) = R.matlab::readMat(con)$data} but other functions may be required depending on the format of FVCOM files (e.g. \code{\link[base]{readRDS}}).
#' @param cl (optional) A cluster objected created by the parallel package. If supplied, the algorithm is implemented in parallel. Note that the connection with the cluster is stopped within the function.
#' @param varlist A list of character vector of names of objects to export to be passed to the \code{varlist} argument of \code{\link[parallel]{clusterExport}}.
#' @param dir2save (optional) A string which defines the directory in which to save files. If provided, the function will save a .rds file with the specified name in the specified directory. If not provided, the function will return a list of thermocline files, with one element for each file.
#' @param verbose A logical input which defines whether or not to display messages to the console detailing function progress.
#'
#' @details Thermocline strength is approximated as the difference in temperature between the surface and the bottom. This is much computationally faster than calculating other metrics (e.g. the standard deviation in temperature across layers). Positive numbers indicate the temperature at the surface is higher than at the bottom. Note that this means that the thermocline strength option cannot be applied properly to the data supplied with this package, which is a small subset of the temperature predictions that does not include the full 10 layers.
#'
#' @return For each \code{source_file}, the function computes a 2 dimensional array comprising thermocline strength for all hours and nodes specified in the file. Arrays are either returned in a list to the workspace (with one element for each file) or, if \code{dir2save} is provided, saved to a user-supplied directory as a .rds file.
#'
#' @examples
#' #### Example (1): Compute thermocline strength using default options
#' # Define directory of the folder containing temperature files:
#' path <- system.file("WeStCOMS_files/",
#'                     package = "WeStCOMSExploreR", mustWork = TRUE)
#' dir2load <- paste0(path, "temp/")
#' # Define files for which we'll create thermocline stength:
#' files <- list.files(dir2load, pattern = ".mat")
#' # Define corresponding names for thermocline arrays:
#' names <- substr(files, 1, 6)
#' # Implement function
#' thermoline_ls <-
#'   compute_field_thermocline(source_file = files,
#'                             sink_file = names,
#'                             dir2load = dir2load,
#'                             read_fvcom =
#'                               function(con) R.matlab::readMat(con)$data)
#' utils::str(thermoline_ls)
#'
#' #### Example (2) Save files on the fly by using dir2save argument
#' dir2save <- paste0(path, "thermocline_strength/")
#' dir.create(dir2save)
#' thermoline_ls <-
#'   compute_field_thermocline(source_file = files,
#'                             sink_file = names,
#'                             dir2load = dir2load,
#'                             read_fvcom =
#'                               function(con) R.matlab::readMat(con)$data,
#'                             dir2save = dir2save)
#' # NULL elements are returned by the function because we have saved files instead:
#' utils::str(thermoline_ls)
#' list.files(dir2save)
#'
#' #### Example (3) Unavailable and/or corrupt files are excluded
#' #
#'
#' #### Example (4) Compute fields for each file in parallel
#' thermoline_ls <-
#'   compute_field_thermocline(source_file = files,
#'                             sink_file = names,
#'                             dir2load = dir2load,
#'                             read_fvcom = function(con) R.matlab::readMat(con)$data,
#'                             cl = parallel::makeCluster(2L))
#' utils::str(thermoline_ls)
#'
#' @author Edward Lavender
#' @export
#'
#'

compute_field_thermocline <-
  function(
    source_file,
    sink_file = NULL,
    dir2load,
    read_fvcom = function(con) R.matlab::readMat(con)$data,
    cl = NULL,
    varlist = NULL,
    dir2save = NULL,
    verbose = TRUE){

    #### Checks
    if(verbose){
      cat("WeStCOMSExploreR::compute_field_thermocline() called...\n")
      cat("Step 1/2: Checking user inputs...\n")
    }
    ## Files and names should be of the same length
    if(!is.null(dir2save)) check_length(arg = "sink_file", input = sink_file, req_length = length(source_file), req_arg = "source_file")
    ## Check directories exist
    check_dir(arg = "dir2load", input = dir2load)
    if(!is.null(dir2save)) check_dir(arg = "dir2save", input = dir2save)

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

        ## Define file and name
        file_con <- source_file[i]
        if(!is.null(dir2save)) file_name <- sink_file[i]

        ## Load file
        fvcom_array <- read_fvcom(paste0(dir2load, file_con))

        ## Compute field
        # Calculate temperature difference as surface - bottom
        # +ve numbers indicate the temperature at the surface is higher than at the bottom
        n_layer <- dim(fvcom_array)[2]
        fvcom_field <- fvcom_array[, c(min(n_layer), max(n_layer)), ]
        fvcom_field <- fvcom_array[, 1, ] - fvcom_array[, 2, ]

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


#### End of code.
#####################################
#####################################
