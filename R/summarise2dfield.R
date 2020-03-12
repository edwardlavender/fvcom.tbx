#' @title Calculate summary statistics over (2d) space and time
#' @description This function produces summary statistics of the variation in environmental conditions across a mesh on a given day. These statistics can be computed either separately for each specified hour, if the inputted array has a row corresponding to those hours, across all hours, or for a single hour if the inputted array contains a single row.
#'
#' @param data An n x m matrix, in which n are the hours and m are the mesh cells across which you want to calculate summary statistics.
#' @param row_specific A logical input specifying whether summary statistics should be calculated for every row (i.e. hour) in the matrix (\code{row_specific = TRUE}), or all rows (\code{row_specific = FALSE}).
#' @param funs A list of functions that you want to evaluate across all mesh cells for each of the specified hours. Each element in the list should be named with a name that corresponds to the function applied (see Examples).
#'
#' @return A dataframe with a row for each hour across which statistics were calculated and a column for each summary statistic. The column names correspond to the names provided in the \code{funs} argument.
#'
#' @examples
#'
#' #### Load in WeStCOMS data and process data as required into a 2d field:
#' path <- system.file("WeStCOMS_files/temp", package = "WeStCOMSExploreR", mustWork = TRUE)
#' # Define the path to the temperature file for 2016-03-01:
#' pathname <- file.path(path, "160301.mat")
#' # Read in the file using the R.matlab package:
#' sample <- R.matlab::readMat(pathname)
#' # Extract the model output (i.e. the 3d array) from the list using $data
#' sample <- sample$data
#' # Examine the structure of the array:
#' str(sample)
#' # For a 3d variable, define a specific layer across which to calculate summary statistics
#' sample <- sample[, 1, ]
#' str(sample)
#'
#' #### 1) Calculate row- (hour-) specific summary statistics across all mesh cells:
#' # We have a 2 x 802 matrix; rows 1, 2, correspond to hours 1 and 2 in this case
#' # ... so to calculate summary statistics across all mesh cells for each hour
#' # ... apply the summarise2sfields as follows:
#' ls <- summarise2dfield(data = sample,
#'                        row_specific = TRUE,
#'                        funs = list(mean = mean, min = min, max = max, sd = stats::sd)
#'                        )
#' # Examine outputs:
#' ls
#'
#' #### 2) Calculate summary statistics across all rows (hours) and mesh cells:
#' ls <- summarise2dfield(data = sample,
#'                        row_specific = FALSE,
#'                        funs = list(mean = mean, min = min, max = max, sd = stats::sd)
#'                        )
#' ls
#'
#' @author Edward Lavender
#'


##############################################
##############################################
#### summarise2dfield

summarise2dfield <-
  function(
    data,
    row_specific = TRUE,
    funs = list(mean = mean, min = min, max = max)
    ){

    #### Check that the functions in the list provided (funs) have names:
    if(is.null(names(funs))){
      stop("Please specify the names of the functions in the 'funs' argument list; e.g., funs = list(mean = mean)")
    } # close if(is.null(names(funs))){

    #### Define a vector of function names; we'll use these to name columns later.
    funs_names <- names(funs)

    #### If there are multiple hours for which we want to calculate summary statistics
    # ... then we'll transform the matrix into a dataframe and use tapply to calculate
    # ... summary statistics:
    if(row_specific & nrow(data) > 1){
      # Convert datsbt (a matrix with one row for each hour and a column for each node) into df:
      data_df <- as.data.frame(data)
      # Bring all nodes into a single column so, for each node ("cell"), we have 24 values
      # ... one for each hour (the order is preserved)
      data_df <- tidyr::gather(data = data_df, key = "cell", value = "value")
      # Because the order is preserved, we can add the hours:
      data_df$row_id <- rep(1:nrow(data), length(ncol(data)))
      # For each hour, calculate summary statistics across the whole area:
      # Loop over summary statistics to create a list of dataframes,
      # ... each element is a dataframe with a single column
      # ... corresponding to the statistic of interest
      # ... (we'll bind these together below)
      ls <- lapply(1:length(funs), function(i){
        # (Use tapply() rather than a loop for 1computational speed):
        s <- tapply(data_df$value, data_df$row_id, funs[[i]])
        # convert to a dataframe and add column name:
        sdf <- data.frame(as.numeric(s))
        colnames(sdf) <- funs_names[i]
        return(sdf)
      }) # close lapply function and lapply


    #### If the user wants to calculate summary statistics across all hours or only for a single hour...
    } else{
      # simply calculate the statistics across all cells:
      ls <- lapply(funs, function(statistic){
        s <- statistic(data)
        return(s)
      }) # close lapply function and lapply

    } # close else{

    #### Bind the elements of the list into a dataframe,
    # with one row for each hour,
    # and one column for each summary statistic:
    sy <- do.call(cbind, ls)

    #### Return the data.frame
    return(sy)

  } # Close function

#### End of code
##############################################
##############################################
