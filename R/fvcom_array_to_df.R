#' @title Convert an FVCOM array to a dataframe for \code{\link[fvcom.tbx]{plot_field_2d}}
#' @description This function converts an FVCOM array to a dataframe that, for a snapshot in time, contains the values of the cell at each specified ID; i.e, a 2-dimensional field that can be plotted using \code{\link[fvcom.tbx]{plot_field_2d}}.
#'
#' @param data A 2- or 3-dimensional array outputted by FVCOM for an environmental variable.
#' @param hour An integer specifying single hour (i.e. row) for which to extract model outputs.
#' @param layer An integer specifying a single layer for which to extract inputs. This is only required for 3-dimensional variables.
#' @param ID A numerical vector of columns (i.e. mesh IDs) for which to extract data.
#'
#' @return A dataframe with two columns: ID (a unique identifier for each node) and fvcom, the value resolved by FVCOM for the snapshot in time and, if applicable, the layer specified.
#'
#' @note The function extracts the values for the hours, layers and mesh IDs supplied from the array using indexing (since FVCOM outputs may not have column names defining mesh IDs). This means that a full array must be supplied for the outputs for the correct cells to be extracted: if the array is subsetted before being passed to this function, then the value at row 1, layer 1 and sheet 1 may not be reflective of hour, layer and mesh cell 1.
#'
#' @examples
#'
#' #### Step 1: Load in some WeStCOMS files...
#' # Define directory of the folder containing temperatures:
#' path <- system.file("WeStCOMS_files/temp", package = "fvcom.tbx", mustWork = TRUE)
#' # Define the path to the temperature file for 2016-03-01:
#' pathname <- file.path(path, "160301.mat")
#' # Read in the file using the R.matlab package:
#' sample <- R.matlab::readMat(pathname)
#' # Extract the model output (i.e. the 3-dimensional array) from the list using $data
#' sample <- sample$data
#' # Examine the structure of the array:
#' str(sample)
#' #### Step 2: Convert sample matrix to a dataframe for plotting...
#' dat_temp <- fvcom_array_to_df(data = sample,
#'                               hour = 1,
#'                               layer = 1,
#'                               ID = 1:length(dat_nodexy$node_id)
#'                               )
#' #### Step 3: Examine dataframe
#' head(dat_temp)
#'
#' @author Edward Lavender
#' @export
#'

fvcom_array_to_df <- function(data,
                             hour,
                             layer = NULL,
                             ID){

  #### Ensure that IDs are sorted in order
  # This means that, providing a full array has been provided,
  # ... when we construct the dataframe, the order of values in the sample
  # ... we extract corresponds sequentially to each ID.
  ID <- sort(as.numeric(as.character(ID)))

  #### Extract sample data:
  # If the user has not specified layer (i.e. for a 2-dimensional variable...)
  if(is.null(layer)){
    # Check the array is 2 dimensional:
    stopifnot(length(dim(data)) == 2)
    # Extract the data for the hour and ID specified:
    sample <- data[hour, ID]
  # Otherwise, we're dealing with a 2-dimensional variable and we also need to extract
  # ... data for the layer of interest:
  } else{
    # Check we are dealing with a 3-dimensional variable
    stopifnot(length(dim(data)) == 3)
    sample <- data[hour, layer, ID]
  }

  #### Define dataframe:
  d <- data.frame(ID = ID, fvcom = sample)

  #### Return dataframe:
  return(d)

} # close function


#### End of code.
#######################################
#######################################
