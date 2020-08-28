#############################
#############################
#### pipe

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


###############################
###############################
#### pracma::quiver()

#' @title quiver
#' @description Add a vector field to a plot
#'
#' @param x A numeric value specifying the x coordinate(s) of the start point of the arrow(s).
#' @param y A numeric value specifying the x coordinate(s) of the start point of the arrow(s).
#' @param u The u (easting) component vector.
#' @param v The v (northing) component vector.
#' @param scale A numeric value which scales the length of the arrow shaft.
#' @param length A numeric value which adjusts the length of the arrow head.
#' @param angle A numeric value which defines the angle (degrees) between the two sides of the arrow head.
#' @param ... Other graphical parameters that can be passed to \code{\link[graphics]{arrows}}.
#'
#' @source This function is copied from pracma::quiver but corrects a small bug in the function which stops the angle of arrow heads being adjustable.
#'
#' @note This function is not exported.
#' @keywords internal

quiver <-
  function (x, y, u, v, scale = 0.05, angle = 10, length = 0.1,
            ...)
  {
    stopifnot(is.numeric(x), is.numeric(y), is.numeric(u), is.numeric(v))
    graphics::arrows(x, y, x + scale * u, y + scale * v, angle = angle, length = length,
                     ...)
  }


###############################
###############################
#### colour_bar()

#' @title Add a colour bar to a plot
#' @description This function adds a colour bar to a plot
#'
#' @param uniform_values A numeric, pretty, uniform sequence of values that encompass the range of the variable for which the colour scale is to be plotted. This sequence should be the same as the sequence used to create colours on the plot originally.
#' @param colour_palette_fn The function used to provide colour on the main plot.
#' @param cols A sequence of colours can be provided instead of the function above.
#' @param legend_labels Legend labels for tick marks.
#' @param variable_name A variable name to accompany the colour bar.
#' @param variable_name_line A numeric value specifying the number of lines the variable_name is away from the axis.
#' @param cex The font size of the title, relative to the default.
#' @param cex.axis The font size of the legend labels for tick marks.
#'
#' @author Edward Lavender
#' @keywords internal

colour_bar <-
  function(
    uniform_values,
    colour_palette_fn,
    cols = NULL,
    legend_labels,
    variable_name,
    variable_name_line,
    cex = 1.8,
    cex.axis = 1.6){

    # Define max and min covariate values
    min_val <- min(uniform_values)
    max_val <- max(uniform_values)

    # Identify whether any of the pretty legend labels are less than or more than these min/max values
    # If so, we'll remove those values because we don't want to plot a legend axis
    # ... that is longer than the colour scale
    llr <- legend_labels[which(legend_labels < min_val)]
    lur <- legend_labels[which(legend_labels > max_val)]
    # If there are any lower legend labels that need to be removed (llr), remove these:
    if(length(llr) > 0){
      legend_labels <- legend_labels[!(legend_labels %in% llr)]
    }
    # Repeat with end legend upper labels that need to be removed (lur):
    if(length(lur) > 0){
      legend_labels <- legend_labels[!(legend_labels %in% lur)]
    }

    # Redefine max and min covariate values
    # ... using legend_labels
    min_val <- min(legend_labels)
    max_val <- max(legend_labels)

    # create a blank plot, setting limits appropriately
    graphics::plot(c(0,1), c(min_val, max_val),
                   xlim = c(0, 1),
                   ylim = c(min_val, max_val),
                   type = "n",
                   axes = F,
                   xlab = "", ylab = "",
                   main = "")

    # remove any uniform values outside of the range of our pretty legend axis:
    uniform_values <- uniform_values[which(uniform_values >= min_val & uniform_values <= max_val)]

    # Define the length of our uniform values sequence:
    lcp <- length(uniform_values)

    # Define colours if required:
    if(is.null(cols)){
      # Define a colour for each value using the colour palette function and the length:
      cols <- colour_palette_fn(lcp)
    }

    # For every colour in the colour palette (except the last one...
    for(i in 1:(lcp - 1)){
      # we will draw a rectangle and fill it with the appropriate colour
      graphics::rect(xleft = 0,
                     ybottom = uniform_values[i],
                     xright = 1,
                     ytop = uniform_values[i + 1],
                     col = cols[i],
                     border = NA)
    } # close for loop: for(i in 1:(lcp - 1)){

    # add an axis using pretty legend labels object
    graphics::axis(4, legend_labels, las = 1, cex.axis = cex.axis, pos = 1)
    graphics::mtext(side = 4, variable_name, cex = cex, line = variable_name_line)

    # close custom function
  }


###############################
###############################
#### match_ts_nearest() from Tools4ETS

#' @title Find the position in one vector that is nearest in time to a value in another dataframe
#' @import data.table
#'
#' @description This function is like \code{\link[base]{match}}, but the aim is, for a given sequence of times (\code{times}), to find the positions in another sequence of times (\code{lookup}) that are nearest in time to those in the first sequence. In other words, for every time inputted, \code{match_ts_nearest()} finds the position in another sequence of times which is nearest in time to that time. This is useful if, for example, you have an existing dataframe to which you want to add the observations, held in another dataframe, that are nearest in time to observations in the first dataframe (i.e., nearest neighbour interpolation). This function uses \code{\link[data.table]{data.table}} for fast matching, even with very large vectors.
#'
#' @param times A vector of timestamps for which you want to identify the position of the nearest timestamp in another vector (\code{lookup}).
#' @param lookup A vector of timestamps for which you will determine the position of the nearest timestamp to each time in \code{times}.
#'
#' @details If there are multiple matches, only the first is returned.
#'
#' @return For a sequence of times (\code{times}), the function returns a vector of the positions of the nearest times in another sequence (\code{lookup}).
#'
#' @author Edward Lavender
#' @keywords internal
#'

match_ts_nearest <- function(times, lookup){
  # Define data.table objects, adding indices to both tables;
  # We will use the index to ensure that observations are returned in the correct order.
  # This also means that 'times' input does not need to be arranged by time.
  dt1 <- data.table::data.table(t = times, index_times = 1:length(times))
  dt2 <- data.table::data.table(t = lookup, index_lookup = 1:length(lookup))
  # Set the key for both tables, arranging by time
  data.table::setkey(dt1, t)
  data.table::setkey(dt2, t)
  # Join the data tables by the observations that are nearest in time
  # This adds the index_lookup column to dt1, adding the values in index_lookup
  # ... that are nearest in time to the times in dt1
  djoin <- dt2[dt1, roll = "nearest", mult = "first"]
  # Reorder djoin by index_times so that the vector of positions in lookup
  # ... returned is in the appropriate order given order of inputted times
  # NB set index times to NULL to avoid 'no visible binding for global variable ‘index_times’'
  index_times <- NULL
  data.table::setkey(djoin, index_times)
  return(djoin$index_lookup)
}


###############################
###############################
#### match_ts_nearest_by_key() from Tools4ETS

#' @title Match timeseries by key and time
#' @description For two dataframes, \code{d1} and \code{d2}, this function finds the positions in the second dataframe which, for each key (e.g., factor level, individual) in the first dataframe, are nearest in time (i.e., nearest neighbour interpolation accounting for observations from different factor levels).
#'
#' @param d1 A dataframe which includes a column that defines factor levels and a column that defines timestamps. The names of these columns need to match those in \code{d2}.
#' @param d2 A dataframe which includes a column that defines factor levels and a column that defines timestamps. The names of these columns need to match those in \code{d1}.
#' @param key_col A character that defines the column name in \code{d1} and \code{d2} that distinguishes factor levels.
#' @param time_col A character that defines the column name in \code{d1} and \code{d2} that defines timestamps.
#'
#' @details If there are multiple matches, only the first is returned.
#'
#' @return For a dataframe comprising observations from a series of factor levels (e.g., individuals) collected through time, the function returns a vector of positions in a second dataframe which, for the appropriate factor level, are nearest in time.
#'
#' @author Edward Lavender
#' @keywords internal
#'

match_ts_nearest_by_key <- function(d1, d2, key_col, time_col){
  # Check dataframes contain required columns
  stopifnot(all(c(key_col, time_col) %in% colnames(d1)))
  stopifnot(all(c(key_col, time_col) %in% colnames(d2)))
  # Check that all keys in d1 are in d2 and, if not, return a warning
  if(!all(unique(d1[, key_col]) %in% unique(d2[, key_col]))){
    warning("Not all unique keys in d1 are found in d2.")
  }
  # Convert tibbles to dataframes: this is necessary to correctly define data.tables, below.
  if(inherits(d1, "tbl")) d1 <- data.frame(d1)
  if(inherits(d2, "tbl")) d2 <- data.frame(d2)
  # Define datatables
  dt1 <- data.table::data.table(ky = d1[, key_col], t = d1[, time_col],  d1_index = 1:nrow(d1))
  dt2 <- data.table::data.table(ky = d2[, key_col], t = d2[, time_col],  d2_index = 1:nrow(d2))
  # Set the key for both tables, arranging by key then time
  ky <- NULL; t <- NULL;
  data.table::setkey(dt1, ky, t)
  data.table::setkey(dt2, ky, t)
  # Join the data tables by the observations by key and nearest in time
  djoin <- dt2[dt1, roll = "nearest", mult = "first"]
  # Reorder djoin by d1_index to match input order
  d1_index <- NULL
  data.table::setkey(djoin, d1_index)
  return(djoin$d2_index)
}


###############################
###############################
#### pair_ts() from Tools4ETS

#' @title Pair timeseries
#' @description This function adds observations from one timeseries to another timeseries using a matching process (e.g., nearest neighbour interpolation). This is useful when you have a main dataframe to which you need to add observations (e.g., those occurring closest in time) from another dataframe.
#'
#' @param d1 A dataframe that contains, at a minimum, a vector of timestamps, to which observations are added from \code{d2}.
#' @param d2 A dataframe that contains, at a minimum, a vector of timestamps and associated observations, to be added to \code{d1}.
#' @param time_col A character that defines the name of the column that contains timestamps in \code{d1} and \code{d2}.
#' @param key_col (optional) A character that defines the name of the column that contains keys in \code{d1} and \code{d2}. This is required for \code{method = "match_ts_nearest_by_key"} (see below).
#' @param val_col A character that defines the name of the column that contains observations in \code{d2}.
#' @param method A character that defines the matching method. The options currently implemented are \code{"match_ts_nearest"}, which implements \code{\link[Tools4ETS]{match_ts_nearest}} and \code{"match_ts_nearest_by_key"} which implements \code{\link[Tools4ETS]{match_ts_nearest_by_key}}.
#' @param min_gap (optional) A number that defines the minimum time gap (in user-defined units, see \code{units}, below) between times in \code{d1} and the times of observations that are added to \code{d1} from \code{d2}. This is useful if, for instance, some of the nearest observations in \code{d2} occurred long before the nearest observations in \code{d1}. If provided, the function counts the number of observations which do not meet this requirement and, if requested via \code{control_beyond_gap}, removes these from the returned dataframe or sets them to NA (see below).
#' @param max_gap As above, for \code{min_gap}, but the maximum time gap.
#' @param units A character that defines the units of the inputted \code{min_gap} or \code{max_gap}. This is passed to \code{\link[base]{difftime}}.
#' @param control_beyond_gap A character that defines whether or not to rows from \code{d1} that contain observations from \code{d2} that exceed \code{min_gap} or \code{max_gap} to NA (\code{"NA"}) or to remove those rows (\code{"remove"}).
#'
#' @return The function returns a dataframe, \code{d1}, as inputted, with an added column (whose name is given by \code{val_col}), comprising values added from another dataframe, \code{d2}. Any observations in \code{d1} for which there are not observations in \code{d2} occurring within some time window (defined by \code{min_gap} and \code{max_gap}), if specified, are counted and, if requested, removed from the returned dataframe.
#'
#' @author Edward Lavender
#' @keywords internal

pair_ts <- function(d1,
                    d2,
                    time_col,
                    key_col = NULL,
                    val_col,
                    method = "match_ts_nearest",
                    min_gap = NULL,
                    max_gap = min_gap,
                    units = "mins",
                    control_beyond_gap = NULL){

  #### Identify current columns in d1
  cols_in_d1 <- colnames(d1)

  #### Checks
  stopifnot(time_col %in% colnames(d1) & time_col %in% colnames(d2))
  stopifnot(val_col %in% colnames(d2))
  check_value(arg = "method", input = method, supp = c("match_ts_nearest", "match_ts_nearest_by_key"))
  if(!is.null(control_beyond_gap))
    check_value(arg = "control_beyond_gap", input = control_beyond_gap, supp = c("NA", "remove"))

  #### Implement match_ts method
  if(method == "match_ts_nearest"){
    d1$position_in_d2 <- match_ts_nearest(d1[, time_col], d2[, time_col])
  } else if(method == "match_ts_nearest_by_key"){
    if(is.null(key_col)){
      stop("key_col must be specified for method = 'match_ts_nearest_by_key'")
    } else{
      stopifnot(key_col %in% colnames(d2))
    }
    d1$position_in_d2 <- match_ts_nearest_by_key(d1, d2, key_col = key_col, time_col = time_col)
  }

  #### Add values to d1 from d2
  d1[, val_col] <- d2[d1$position_in_d2, val_col]

  #### Check whether min or max gap have been exceeded, if requested
  if(!is.null(min_gap) | !is.null(max_gap)){

    ## Add times in d2 to d1
    d1$time_in_d2 <- d2[d1$position_in_d2, time_col]
    # Compute difference in time using specified units
    # Use drop = TRUE in case a tibble has been provided.
    d1$difftime <- as.numeric(difftime(d1[, "time_in_d2", drop = TRUE], d1[, time_col, drop = TRUE], units = units))

    ## Check whether the min_gap was exceeded
    min_gap_exceeded <- any(d1$difftime < min_gap, na.rm = TRUE)
    l_min_gap_exceeded <- length(which(min_gap_exceeded))
    if(min_gap_exceeded){
      warning(paste0(l_min_gap_exceeded, " observations exceeded min_gap."))
      if(!is.null(control_beyond_gap)){
        if(control_beyond_gap == "remove"){
          d1 <- d1[which(d1$difftime >= min_gap), ]
        } else if(control_beyond_gap == "NA"){
          d1[which(d1$difftime < min_gap), val_col] <- NA
        }
      }
    }

    ## Check whether the max_gap was exceeded
    max_gap_exceeded <- any(d1$difftime > max_gap, na.rm = TRUE)
    l_max_gap_exceeded <- length(which(max_gap_exceeded))
    if(max_gap_exceeded){
      warning(paste0(l_max_gap_exceeded, " observations exceeded max_gap."))
      if(!is.null(control_beyond_gap)){
        if(control_beyond_gap == "remove"){
          d1 <- d1[which(d1$difftime <= max_gap), ]
        } else if(control_beyond_gap == "NA"){
          d1[which(d1$difftime > max_gap), val_col] <- NA
        }
      }
    }

  } else{
    if(!is.null(control_beyond_gap)) warning("control_beyond_gap is ignored: both min_gap and max_gap are NULL.")
  }

  #### Return dataframe
  d1_to_return <- d1[, c(cols_in_d1, val_col)]
  if(nrow(d1_to_return) == 0){
    warning("No observations remain in d1; NULL returned.")
    return(NULL)
  } else return(d1_to_return)
}


######################################
######################################
#### check_names()

#' @title Check the names of an object contain required names
#' @description This function checks whether required names are contained within an object. If the object does not contain any/all required names (the precise criteria is controlled by the user), the function returns a helpful error message.
#' @param arg A character string which defines the argument of the parent function.
#' @param input An object for which the names need to be checked.
#' @param req A character vector of required names.
#' @param extract_names A function which is used to extract names from \code{input}, such as \code{\link[base]{names}} or \code{\link[base]{colnames}}.
#' @param type A function which defines the failure criteria. For example, if \code{type = all}, the function will return an error unless all the names in \code{req} are contained within \code{input}. This is the default. If \code{type = any}, the function will return an error only if none of the names in \code{req} are contained within \code{input}.
#' @return If the input fails the check, the function returns a helpful error message. Otherwise, nothing is returned.
#' @author Edward Lavender
#' @keywords internal
#'

check_names <- function(arg = deparse(substitute(input)), input, req, extract_names = names, type = any){
  input_names <- extract_names(input)
  if(!type(req %in% input_names)){
    req_names_missing <- req[which(!(req %in% input_names))]
    msg <- paste0("Argument ", arg, " does not contain ", deparse(substitute(type)), " required names. The following name(s) are missing:",
                  paste0("'", req_names_missing, collapse = ", "),
                  "'.")
    stop(msg)
  }
}

###############################
###############################
#### check_length()

#' @title Check the length of an input to a parent function
#' @description This function checks that the length of an input to a parent function is correct. If not, the function returns a helpful error message.
#' @param arg (optional) A character string which defines the argument of a parent function.
#' @param input An object.
#' @param req_length A number which defines the required length of \code{input}.
#' @param req_arg A character which defines the name of the object which defines the required length.
#' @return The function returns a helpful error message for unnamed lists (ignoring empty lists if requested) or the inputted list unchanged.
#' @author Edward Lavender
#' @keywords internal
#'

check_length <- function(arg = deparse(substitute(input)),
                         input,
                         req_length,
                         req_arg = deparse(substitute(req_length))){
  if(length(input) != req_length){
    stop(paste0("The length of the argument '", arg, "' (=", length(input), ") must be the same as the length of ", req_arg, " (=", req_length, ")."), call. = FALSE)
  }
}


###################################
###################################
#### check_class()

#' @title Check the class of an function input to a parent function
#' @description This function checks that the class of an input to a parent function is appropriate. If not, the function either produces a helpful error message or returns a warning.
#' @param arg A character string which defines the argument of the parent function.
#' @param input The input to an argument of a parent function.
#' @param if_class (optional) A character vector of classes of object. If supplied, the function will only proceed to check the class of the object if the \code{class(input)} is one of \code{if_class}. This is useful if \code{check_class()} is implemented in a loop.
#' @param to_class The required class of the input.
#' @param type A character which specifies whether to return an error (\code{"stop"}) or a warning ("warning").
#' @param coerce_input A function used to coerce \code{input} to the correct object type, if \code{type = "warning"}.
#' @return The function checks the class of the input. If the class is not the same as required by the parent function (i.e., as specified by \code{class}), the function returns a helpful error message, or a warning and an object whose class has been coerced to the correct class.
#' @author Edward Lavender
#' @keywords internal
#'

check_class <-
  function(arg = deparse(substitute(input)), input, if_class = NULL, to_class, type = "stop", coerce_input){

    #### Define whether or not to proceed:
    # Only proceed if if_class is NULL or, if supplied, then only proceed if the class of the object
    # ... is of type(s) in if_class
    proceed <- FALSE
    if(is.null(if_class)){
      proceed <- TRUE
    } else{
      if(inherits(input, if_class)) proceed <- TRUE
    }

    #### Check the class, if required
    if(proceed){
      # If the object is not of the necessary class
      if(!inherits(input, to_class)){
        # Either stop...
        if(type == "stop"){
          msg <- paste0("Argument '", arg, "' must be of class '", to_class, "', not class(es): '", paste(class(input), collapse = "', '"), "'.")
          stop(msg)
          # Or print a warning and use coerce_input() to convert the object to the desired class.
        } else if(type == "warning"){
          msg <- paste0("Argument '", arg, "' coerced to class '", to_class, "' from class(es): '", paste(class(input), collapse = "', '"), "'.")
          warning(msg)
          input <- coerce_input(input)
        }
      }
    }

    #### If we've passed all checks, return the input (possibly coerced to a new class)
    return(input)
  }


######################################
######################################
#### check_value()

#' @title Check the input value to a parent function argument
#' @description Within a function, this function checks the value of an input to an argument of that function. If the input value is supported, the function simply returns this value. If the input is not supported, the function either throws an error or returns a warning and the default value.
#'
#' @param arg A character string which defines the argument of the parent function.
#' @param input The input to an argument of a parent function.
#' @param supp A vector of supported input values for the argument in the parent function.
#' @param warn A logical input which defines whether or not to return a warning and the default value (see \code{default}) or an error.
#' @param default The default input value for the parent function.
#'
#' @return The function returns \code{input}, an error or a warning and \code{default} depending on whether or not \code{input} is within \code{supp} (i.e., whether or not the input to the argument of a parent function is supported).
#' @author Edward Lavender
#' @keywords internal
#'

check_value <- function(arg = deparse(substitute(input)), input, supp, warn = TRUE, default = supp[1]){
  # If the input is not in a vector of supported arguments...
  if(!(input %in% supp)){
    ## Provide a warning and revert to the default
    if(is.character(input)) input <- paste0("'", input, "'")
    if(warn){
      if(is.character(default)) default <- paste0("'", default, "'")
      warning(paste0("Argument '", arg, "' = ", input, " is not supported; defaulting to ", arg, " = ", default, ".\n"))
      input <- default
    } else{
      if(is.character(supp)) supp <- paste0("'", supp, "'")
      stop(paste0("Argument '", arg, "' = ", input, " is not supported. Supported option(s): ", paste0(supp, collapse = ", "), "."))
    }
  }
  # Return input
  return(input)
}


######################################
######################################
#### check_dir()

#' @title Check a directory exists
#' @description This function checks whether a directory exists and, if not, returns an informative error message. The inputted directory can be edited with the addition of a '/' if requested.
#' @param arg (optional) A character string which defines the argument of a parent function.
#' @param input A character string which defines a directory.
#' @param check_slash A logical input that defines whether or not to check the end of a string for '/'. If \code{TRUE} and a '/' is lacking, this is added to the returned directory.
#' @return The function checks whether or not a directory exists. If so, the function returns either the directory as inputted, or the directory with a '/' added to the end. If not, the function returns an informative error message.
#' @author Edward Lavender
#' @keywords internal
#'

check_dir <- function(arg = deparse(substitute(input)),
                      input,
                      check_slash = FALSE){

  #### Check the directory exists
  if(!dir.exists(input)){
    stop(paste0("The directory inputted to the argument '", arg, "' ('", input, "') does not exist."), call. = FALSE)
  }

  #### Check the directory ends in a /
  if(check_slash){
    end_is_slash <- substr(input, nchar(input), nchar(input)) == "/"
    if(!end_is_slash){

      message(paste0("'/' added to the directory inputted to the argument '", arg, "' ('", input, "')."))
      input <- paste0(input, "/")
    }
  }

  #### Return input, possibly updated with / if checks passed
  return(input)
}


###################################
###################################
#### check_named_list()

#' @title Check that a list is named
#' @description This function checks that the top level of a list is named (ignoring empty lists if requested). If the list is not named, the function returns a helpful error message. Otherwise, the list is returned unchanged. This is particularly useful within functions that use \code{\link[base]{do.call}} to evaluate lists of arguments.
#' @param arg (optional) A character string which defines the argument of a parent function.
#' @param input A list.
#' @param ignore_empty A logical input which defines whether or not to ignore empty lists.
#' @return The function returns a helpful error message for unnamed lists (ignoring empty lists if requested) or the inputted list unchanged.
#' @author Edward Lavender
#' @keywords internal

check_named_list <- function(arg = deparse(substitute(input)), input, ignore_empty = TRUE){
  list_is_empty <- (length(input) == 0)
  if(!list_is_empty | !ignore_empty){
    if(is.null(names(input)) | any(names(input) %in% "")){
      msg <- paste0("Argument '", arg, "' must be a named list.")
      stop(msg)
    }
  }
  return(input)
}

#### End of code.
###############################
###############################
