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


################################################
#### colour_bar

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



#### End of code.
################################################
################################################
