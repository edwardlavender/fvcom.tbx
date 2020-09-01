#' @title Map 2-dimensional fields
#' @description This function creates a map of the environmental conditions over space, as resolved across the nodes or elements of an unstructured grid, for a given depth/altitude, and time. Scalar fields are shown as a coloured surface, with the colour of a grid cell corresponding to the value of that cell. Vector fields are shown as coloured arrows ontop of the unstructured grid which point in the direction of mass flow and whose colour corresponds to the total magnitude of the vector, derived from the two (u and v) vector components
#'
#' @param coastline A spatial object, such as a shapefile (i.e. SpatialPolygonsDataFrame), that delineates the coastline in the study area. This is assumed to have been cropped to the study area (i.e. its extent should be within the region defined by xlim and ylim, see below).
#' @param mesh A SpatialPolygonsDataFrame, created by \code{\link[fvcom.tbx]{build_mesh}}, that defines the unstructured mesh in the study area across which environmental conditions have been resolved. This is also assumed to have been cropped to the study area (i.e. its extent should be within the region defined by xlim and ylim, see below). For conditions resolved at the nodes of an unstructured grid, a mesh around nodes (i.e. based on elements) is required. In contrast, for conditions resolved at the elements of an unstructured grid, a mesh around elements (i.e. based on nodes) is required.
#' @param vector_field A logical object defining whether the data to be plotted across the mesh are a scalar field (e.g. temperature), in which case \code{vector_field = FALSE} or a vector field (e.g. wind speed), in which case \code{vector_field = TRUE}.
#' @param data The data to be plotted across the mesh ID. For scalar fields, i.e., \code{vector_field = FALSE}, this is a single dataframe with two columns: ID, a numeric/factor/character column specifying the ID of each mesh cell, i.e., a unique reference to each element or node that matches that in \code{mesh}; and fvcom, a numeric column specifying the value assigned to that cell by the hydrodynamic model. For vector fields, this should be a named list with two elements: one called "udata" and one called "vdata"; each element in the list should contain a dataframe, as described above, that contain the u and v vector components resolved at each element respectively.
#' @param xlim A numeric pair of numbers which define the lower and upper x limits of the study region respectively. These should be on the same scale as the coordinate system used for the coastline and mesh.
#' @param ylim A numeric pair of numbers which define the lower and upper y limits of the study region respectively. These should be on the same scale as the coordinate system used for the coastline and mesh.
#' @param coastline_col The colour of the coastline (i.e. land). This can be specified as a character or numeric value. \code{"white"} is the default.
#' @param coastline_border The colour of the coastline border. \code{"black"} is the default.
#' @param coastline_lwd The thickness of the coastline border. \code{1} is the default.
#' @param coastline_lty The line type of the coastline border. \code{1} is the default.
#' @param mesh_border The colour of the unstructured mesh grid lines. \code{"lightgrey"} is the default.
#' @param mesh_lwd The thickness of the mesh grid lines. \code{1} is the default.
#' @param mesh_lty The line type of the mesh grid lines. \code{1} is the default.
#' @param ncols The number of colours in a spectrum that is used to colour either mesh cells (for scalar fields) or arrows (for vector fields) according to the value of the environmental variable resolved in that cell. Larger numbers result in smoother scales, which look as if they are continuous, which is desirable, but processing takes longer. \code{50000} is the default.
#' @param col_fn A function used to create colours. \code{\link[grDevices]{heat.colors}} is the default colour scheme.
#' @param colour_bar_add A logical input defining whether or not to add a colour bar to the map. \code{TRUE} is the default.
#' @param colour_bar_x A pair of numeric values defining the lower and upper x coordinates of the colour bar respectively.
#' @param colour_bar_y A pair of numeric values defining the lower and upper y coordinates of the colour bar respectively.
#'
#' @param element_xy (optional) For vector fields only, \code{element_xy} is matrix containing the coordinates of each element in the mesh. If this is not supplied, then these are obtained from the mesh object. However, if you are applying this function in a custom iterative procedure, supplying this matrix will improve computational performance (especially for large meshes) because it does not have to be recalculated every time.
#' @param arrow_angle For vector fields only, a numeric value specifying the angle between the two sides of the arrow head in degrees (see \code{\link[graphics]{arrows}}).
#' @param arrow_scale For vector fields only, a numeric value which scales the length of the arrows. Smaller numbers produce smaller arrows.
#' @param arrow_length For vector fields only, a numeric value which specifies the length of the arrow head.
#' @param arrow_lwd For vector fields only, the thickness of the arrows.
#' @param arrow_lty For vector fields only, the line type of the arrows.
#'
#' @param xlab An expression or string specifying the title of the x axis.
#' @param ylab An expression or string specifying the title of the y axis.
#' @param zlab An expression or string specifying the title of the z axis.
#' @param main An expression or string specifying the title of the plot.
#' @param xlab_line The number of lines the x axis title is away from the x axis.
#' @param ylab_line The number of lines the y axis title is away from the y axis.
#' @param zlab_line The number of lines the z axis title is away from the z axis.
#' @param main_line The number of lines the plot title is away from the top of the plot.
#' @param axes A logical input specifying whether or not to add axes to the plot. The function tries hard to find pretty labels within the limits set by \code{xlim} and \code{ylim}.
#' @param nticks A numeric value specifying the approximate number of ticks between the x and y limits of the plot. This is approximate because the actual number of ticks depends on how many 'pretty' points lie between the x and y limits.
#' @param cex A numerics value giving the amount by which axes titles should be magnified relative to the default. See \code{\link[graphics]{par}}.
#' @param cex.axis As above, but for the numbers assigned to axes tick marks.
#'
#' @return A plot of environmental conditions over an unstructured mesh at a snapshot in time.
#'
#' @author Edward Lavender
#'
#' @examples
#'
#' #### (1) Plot a scalar variable (e.g. surface temperature) over space at a snapshot in time.
#'
#' # This example plots temperature over space for a snapshot in time.
#' # The key arguments to note here are:
#' # mesh = dat_mesh_around_nodes; this is necessary because temperature is resolved at the nodes,
#' # ... so we need a grid which has a node at the heart of every grid cell.
#' # vector_field = FALSE; this is necessary because temperature is a scalar field.
#' # data = dat_temp; this is a pre-processed dataframe which provides a snapshot of
#' # ... of surface temperatures for a single timepoint.
#' xlim <- c(dat_area_boundaries$xlim1,
#'           dat_area_boundaries$xlim2)
#' ylim <- c(dat_area_boundaries$ylim1,
#'           dat_area_boundaries$ylim2)
#' plot_field_2d(coastline = dat_coast_around_oban,
#'               mesh = dat_mesh_around_nodes,
#'               vector_field = FALSE,
#'               data = dat_temp,
#'               xlim = xlim,
#'               ylim = ylim,
#'               ncols = 50,
#'               colour_bar_add = TRUE,
#'               colour_bar_x = c(xlim[2] + c(0.01, 0.02)),
#'               colour_bar_y = ylim,
#'               zlab = expression(paste("Temperature (", degree, "C)")),
#'               main = "2016-03-01"
#'               )
#'
#' #### (2) Plot tidal elevation over space at a snapshot in time.
#' ## Example not run simply to minimise CPU required for examples
#' \dontrun{
#' plot_field_2d(coastline = dat_coast_around_oban,
#'               mesh = dat_mesh_around_nodes,
#'               vector_field = FALSE,
#'               data = dat_tidal_elevation,
#'               xlim = c(dat_area_boundaries$xlim1,
#'                        dat_area_boundaries$xlim2),
#'               ylim = c(dat_area_boundaries$ylim1,
#'               dat_area_boundaries$ylim2),
#'               zlab = "Tidal Elevation (m)",
#'               main = "2016-03-01"
#'               )
#' }
#'
#' #### (3) Plot a vector field over space at a snapshot in time.
#'
#' # Note that vector fields are resolved at the elements of WeStCOMS, so
#' # ... we need a mesh around elements. Also note vector_field = TRUE.
#' # Note also that the data needs to be supplied as a list comprising udata and vdata
#' # ... (i.e. dataframes of the u and v vector components respectively).
#' plot_field_2d(coastline = dat_coast_around_oban,
#'               mesh = dat_mesh_around_elements,
#'               vector_field = TRUE,
#'               data = list(udata = dat_uwind_speed,
#'                            vdata = dat_vwind_speed),
#'               xlim = c(dat_area_boundaries$xlim1,
#'                        dat_area_boundaries$xlim2),
#'               ylim = c(dat_area_boundaries$ylim1,
#'                        dat_area_boundaries$ylim2),
#'               # You can adjust the properties of the arrows for vector fields:
#'               arrow_angle = 30,
#'               arrow_scale = 0.0001,
#'               arrow_length = 0.01,
#'               arrow_lwd = 0.5,
#'               arrow_lty = 1,
#'               zlab = "Wind Speed (m)",
#'               main = "2016-03-01"
#' )
#'
#' @export



##############################################
##############################################
#### plot_field_2d

plot_field_2d <-
  function(coastline,
           mesh,
           vector_field = FALSE,
           data,
           xlim = NULL,
           ylim = NULL,
           coastline_col = "white",
           coastline_border = "black",
           coastline_lwd = 1,
           coastline_lty = 1,
           mesh_border = "lightgrey",
           mesh_lwd = 0.5,
           mesh_lty = 1,
           ncols = 50000,
           col_fn = grDevices::heat.colors,
           colour_bar_add = TRUE,
           colour_bar_x = NULL,
           colour_bar_y = ylim,
           element_xy = NULL,
           arrow_angle = 30,
           arrow_scale = 0.01,
           arrow_length = 0.01,
           arrow_lwd = 0.5,
           arrow_lty = 1,
           xlab = "Longitude (dd)",
           ylab = "Latitude (dd)",
           zlab = "",
           main = "",
           xlab_line = 2.5,
           ylab_line = -3.5,
           zlab_line = 5,
           main_line = 1,
           axes = TRUE,
           nticks = 10,
           cex = 1.2,
           cex.axis = cex - 0.2
           ){



    ##############################################
    ##############################################
    #### Checks

    #### Check, depending on the input data, that they have specified vector field correctly:
    if(class(data) == "data.frame"){

      # Check vector field has been correctly specified
      if(vector_field){
        stop("You have supplied a data.frame but specifed vector_field = TRUE. Either supply a data.frame with vector_field = FALSE to plot a scalar field or supply a list with vector_field = TRUE to plot a vector field.")
      }
      # Check colnames have been correctly specified:
      if(!all(c("ID", "fvcom") %in% colnames(data))){
        stop("Please specify colnames(data) as ID and fvcom.")
      } # close if(!(colnames(data) ...

    } else if(class(data) == "list"){
      # Check vector field has been correctly specified
      if(!vector_field){
        stop("The data you have supplied is in the form of a list, implying that you want to plot a vector field, yet vector field = FALSE.")
      }
      # Check list elements have been correctly specified
      stopifnot(names(data)[1] == "udata" & names(data)[2] == "vdata")
      # Check the list has the correct length
      stopifnot(length(data) == 2)
      # Check the dataframe colnames have been correctly specified
      if(!any(unique(c(colnames(data[[1]]), colnames(data[[2]]))) %in% c("ID", "fvcom"))){
        stop("Please specify colnames(data) as ID and fvcom.")
      }
    }



    ##############################################
    ##############################################
    #### Data preparation for plotting

    #### For non vector fields, add the fvcom values to the mesh...
    # Later, we'll define colours based on these values
    if(!vector_field){
      # add the values to the mesh, by matching mesh IDs and data IDs
      mesh$fvcom <- data$fvcom[match(as.character(mesh$ID), as.character(data$ID))]

    #### Else, for vector fields...
    } else{

      #### Extract u and v data from the list provided
      udata <- data$udata
      vdata <- data$vdata

      #### Add data to the mesh:
      mesh$udata <- udata$fvcom[match(as.character(mesh$ID), as.character(udata$ID))]
      mesh$vdata <- vdata$fvcom[match(as.character(mesh$ID), as.character(vdata$ID))]

      #### Compute magnitude of vectors, which we'll use to colour the lines.
      # (in a called "fvcom" for consistency between scalar and vector fields)
      mesh$fvcom <- sqrt(mesh$udata^2 + mesh$vdata^2)

      #### Compute coordinates (if required) for arrow start points:
      if(is.null(element_xy)) element_xy <- sp::coordinates(mesh)

    }


    #### Define colour scheme:
    # Define a uniform, pretty, sequence of values across the range of the variable
    # ... with the number of colours (smoothness) user-defined:
    pretty_vals <- pretty(range(mesh$fvcom, na.rm = TRUE), n = ncols)
    # Determine the position of each of the values of the variable
    # ... within this pretty sequence using the findInterval function
    mesh$order <- findInterval(mesh$fvcom, pretty_vals)
    # Define a sequence of colours that is the same length as our regular
    # ... sequence of values for the second variable using the user-defined colour function:
    cols <- col_fn(length(pretty_vals))
    # Add to data/mesh based on order
    mesh$colour <- cols[mesh$order]


    ##############################################
    ##############################################
    #### Plot the map

    #### Define limits if required
    if(is.null(xlim)) xlim <- raster::extent(coastline)[1:2]
    if(is.null(ylim)) ylim <- raster::extent(coastline)[3:4]

    #### Plot the coastline
    raster::plot(coastline,
                 col = coastline_col,
                 border = coastline_border,
                 lwd = coastline_lwd,
                 lty = coastline_lty,
                 xlim = xlim,
                 ylim = ylim)


    ##############################################
    #### Add the environmental conditions

    #### For non vector fields, we'll add a coloured mesh:
    # If its not a vector field
    if(!vector_field){
      # add the mesh, with each cell having the appropriate colour,
      # ... with the graphical parameters of the mesh itself specified by the user
      raster::plot(mesh,
                   col = mesh$colour,
                   border = mesh_border,
                   lwd = mesh_lwd,
                   lty = mesh_lty,
                   add = TRUE)

    #### Else, for vector fields, we'll add the mesh and then coloured arrows,
    # ... with the colour defined by the magnitude of the vector components:
    } else{

      #### Add the mesh
      raster::lines(mesh, col = mesh_border, lwd = mesh_lwd, lty = mesh_lty)

      #### Add arrows delineating the direction of mass flow, coloured by magnitude (mesh$fvcom):
      quiver(x = element_xy[, 1],
             y = element_xy[, 2],
             u = mesh$udata,
             v = mesh$vdata,
             angle = arrow_angle,
             length = arrow_length,
             scale = arrow_scale,
             col = mesh$colour,
             lwd = arrow_lwd,
             lty = arrow_lty)

      } # close else{

      #### Add a colour bar, if specified:
      if(colour_bar_add){
        if(is.null(colour_bar_x)) colour_bar_x <- c(xlim[2] + c(0.01, 0.02))
        if(is.null(colour_bar_y)) colour_bar_y <- ylim
        # Add the colour bar at user defined positions, using the colour_bar() function
        TeachingDemos::subplot(x = colour_bar_x,
                       y = colour_bar_y,
                       fun =
                         colour_bar(
                           uniform_values = pretty_vals,
                           cols = cols,
                           legend_labels = pretty(range(pretty_vals), 10),
                           variable_name = zlab,
                           variable_name_line = zlab_line,
                           cex = cex,
                           cex.axis = cex.axis)
                       )
    } # close if(colour_bar_add){



    ##############################################
    #### Complete the map

    #### add back the coastline for neatness
    raster::plot(coastline,
                 col = coastline_col,
                 border = coastline_border,
                 lwd = coastline_lwd,
                 lty = coastline_lty,
                 add = TRUE)

    #### Add titles and axes if requested
    # Note that these can be added outside the function for full customisation
    # ... but for iterative purposes its helpful to be able to add them inside the function too.

    # Define titles
    # These can be suppressed with ""
    graphics::mtext(side = 3, main, cex = cex, line = main_line, font = 2)
    graphics::mtext(side = 1, xlab, cex = cex, line = xlab_line)
    graphics::mtext(side = 2, ylab, cex = cex, line = ylab_line)

    # Define axes
    if(axes){
      # Extract limits
      xlim1 <- xlim[1]
      xlim2 <- xlim[2]
      ylim1 <- ylim[1]
      ylim2 <- ylim[2]
      # Define axes after creating plot
      # (define all four axes to form a box around the plot)
      seq_x <- pretty(c(xlim1, xlim2), n = nticks)
      seq_y <- pretty(c(ylim1, ylim2), n = nticks)
      # Ensure pretty sequences are within limits
      seq_x <- seq_x[which(seq_x >= xlim1 & seq_x <= xlim2)]
      seq_y <- seq_y[which(seq_y >= ylim1 & seq_y <= ylim2)]
      # add blank axes defining a box around the plot, suppressing tick marks
      graphics::axis(side = 1, xlim, labels = c("", ""), lwd.tick = 0, pos = ylim1)
      graphics::axis(side = 2, ylim, labels = c("", ""), lwd.tick = 0, pos = xlim1)
      graphics::axis(side = 3, xlim, labels = c("", ""), lwd.tick = 0, pos = ylim2)
      graphics::axis(side = 4, ylim, labels = c("", ""), lwd.tick = 0, pos = xlim2)
      # add pretty axes labels within this plot
      graphics::axis(side = 1, seq_x, pos = ylim1, cex.axis = cex.axis)
      graphics::axis(side = 2, seq_y, pos = xlim1, cex.axis = cex.axis,  las = 2)
      graphics::axis(side = 3, at = seq_x, labels = rep("", length(seq_x)), pos = ylim2)
      graphics::axis(side = 4, at = seq_y, labels = rep("", length(seq_y)), pos = xlim2)
    } # close if axes

  } # close function


#### End of code.
##############################################
##############################################
