#' @title Interactive 3d plots of landscapes or seascapes using \code{\link[plotly]{plot_ly}}
#'
#' @description This is a wrapper function for \code{\link[plotly]{plot_ly}} which streamlines the production of 3d, interactive raster plots. Key features include (a) automated handling of raster orientation and appropriate labelling, (b) internal aggregation of large rasters, if requested, (c) zooming around inputted points, if requested, (d) the addition of coastlines to 3d rasters, if requested, and (e) additional control over plot shape. Points (e.g. passive acoustic telemetry receivers) and lines (e.g. movement pathways) can be added across the landscape. Some other \code{\link[plotly]{plot_ly}} options are also supported. This function was motivated by the need to visualise rapidly a complex bathymetric landscape with passive acoustic telemetry receivers and reconstructed animal movement pathways over the seabed.
#'
#' @param r A \code{\link[raster]{raster}} to be plotted. The raster should be less than, approximately, 1800 x 1800 cells (otherwise, \code{\link[plotly]{plot_ly}} will not show the plot). Raster aggregation can be implemented within the function (see \code{aggregate}, below), although this may be slow for large rasters. Interactive plots of lower resolution rasters are more responsive.
#' @param aggregate (optional) A named list of arguments that is passed to \code{\link[raster]{aggregate}} to aggregate raster cells. If provided, the raster, \code{r}, is aggregated within the function prior to plotting. For large rasters, this is necessary to reduce raster dimension to a size which can be handled by \code{\link[plotly]{plot_ly}}, which creates the underlying plot. The raster can also be reduced in size by zooming around points provided (see \code{buffer}).
#' @param buffer (optional) A named list of arguments that is passed to \code{\link[rgeos]{gBuffer}} to add a buffer around inputted points,  provided via \code{add_markers}, below (e.g. \code{buffer = list(width = 1)}). If provided, only the raster cells which fall into this/these buffer(s) are shown.
#' @param add_surface (optional) A named list of arguments that is passed to \code{\link[plotly]{add_surface}} to customise the raster surface added to the plot.
#' @param add_markers (optional) A named list of arguments that is passed to \code{\link[plotly]{add_markers}} to add points to the plot.
#' @param thin_markers (optional) A logical input which defines whether or not plot all inputted markers (\code{thin_markers = FALSE}) or only those markers that fall within the domain of the raster (\code{thin_markers = TRUE}).
#' @param add_paths (optional) A named list of arguments that is passed to \code{\link[plotly]{add_paths}} to add lines to the plot.
#' @param coastline (optional) A \code{\link[sp]{SpatialPointsDataFrame-class}} object which defines the coastline (if applicable).
#' @param coastline_paths A named list of arguments that is passed to \code{\link[plotly]{add_paths}} to add the coastline as lines to the plot.
#' @param plane (optional) A number which defines the height of a horizontal, 2 dimensional plane that can be added to the plot to aid inference. Note that if \code{stretch} is provided (see below), the height of the plane is also adjusted by \code{stretch} internally.
#' @param plane_surface (optional) A named list of arguments that is passed to \code{\link[plotly]{add_surface}} to customise the plane added to the plot.
#' @param xlim (optional) A numeric vector of length 2 which defines x axis limits. If not provided, these are calculated internally.
#' @param ylim (optional) A numeric vector of length 2 which defines y axis limits. If not provided, these are calculated internally.
#' @param zlim (optional) A numeric vector of length 2 which defines z axis limits. If not provided, these are calculated internally.
#' @param font A named list of arguments that control plot font (see \code{\link[plotly]{layout}}).
#' @param xtitle (optional) A character string which defines the label for the x axis.
#' @param ytitle (optional) A character string which defines the label for the y axis.
#' @param ztitle (optional) A character string which defines the label for the z axis.
#' @param stretch (optional) A number which is used to vertically stretch the height of a landscape. This can be useful if \code{aspectmode = "data"} but the magnitude of change horizontally versus vertically is very different.
#' @param aspectmode A character which defines the shape of the plot: \code{"cube"} produces a cube; \code{"data"} provides a plot whether the size of the x, y and z axes is scaled according to the data.
#' @param eye (optional) A named list of arguments that control the camera perspective (see \code{\link[plotly]{layout}}).
#' @param verbose A logical input which defines whether or not to display messages regarding function progress. This can be useful if \code{aggregate} or \code{buffer} are provided: for large rasters and/or many points, these steps may be slow.
#'
#' @details The raster dimension should be less than approximately 1800 cells by 1800 cells. The coordinate system for the raster and, if applicable, markers and the buffer width, all need to be identical, whether this is implicit or specified explicitly.
#'
#' @return The function returns a \code{\link[plotly]{plotly}} plot.
#'
#' @examples
#'
#' #### Define some bathymetry data
#' r <- dat_gebco
#' r[r[] > 0] <- 0
#'
#' #### Example 1: Plot a landscape using default options
#' plot3dscape(r = r)
#'
#' #### Example 2: Adjusting axes via zlim, ylim, zlim and font arguments
#' plot3dscape(r = r,
#'                 xlim = c(-5.5, -5.3),
#'                 font = list("Times"))
#'
#' #### Example 3: Change the aspectmode to "data":
#' # The x, y and z values should be in the same units.
#' # Here, depth is in m, so we'll project the raster to UTM coordinates:
#' r <- raster::projectRaster(r, crs = sp::CRS("+proj=utm +zone=29 ellps=WGS84"))
#' plot3dscape(r = r, aspectmode = "data")
#' # For large areas, it can be helpful to vertically stretch the raster
#' plot3dscape(r = r, stretch = 50, aspectmode = "data")
#'
#' #### Example 4: Aggregrate the raster via aggregate
#' # This is necessary for large rasters for plotly to produce the plot.
#' plot3dscape(r = r,  stretch = 50, aspectmode = "data",
#'                 aggregate = list(fact = 2, fun = mean))
#'
#' #### Example 5: Add points via add_markers and zoom into landscape around points
#' # Add points
#' xyz <- matrix(raster::coordinates(r)[500:520, ], ncol = 2)
#' xyz <- data.frame(x = xyz[, 1], y = xyz[, 2], z = -500)
#' plot3dscape(r = r,  stretch = 50, aspectmode = "data",
#'                 add_markers = list(x = xyz$x, xyz$y, xyz$z))
#' # Control point characteristics
#' plot3dscape(r = r,  stretch = 50, aspectmode = "data",
#'                 add_markers = list(x = xyz$x, xyz$y, xyz$z,
#'                                    marker = list(color = "red")))
#' # Zoom into landscape around points via buffer argument
#' # Note that the units are in m here (since we're using the UTM coordinate system)
#' plot3dscape(r = r,  stretch = 50, aspectmode = "data",
#'                 add_markers = list(x = xyz$x, y = xyz$y, z = xyz$z,
#'                                    marker = list(color = "red")),
#'                 buffer = list(width = 1500))
#'
#' #### Example 6: Add lines via add lines
#' # Note that the legend for these lines is hidden automatically
#' plot3dscape(r = r,  stretch = 50, aspectmode = "data",
#'                 add_paths = list(x = xyz$x, y = xyz$y, z = xyz$z))
#'
#' #### Example 6: Incorporate coastline via add_coastline and coastline_paths
#' # r and coastline need to have same crs
#' coastline <- sp::spTransform(dat_coast_around_oban, raster::crs(r))
#' plot3dscape(r = r,  stretch = 50, aspectmode = "data",
#'                 coastline = coastline)
#' # Control coastline graphical parameters
#' # Note that the legend is hidden automatically
#' plot3dscape(r = r,  stretch = 50, aspectmode = "data",
#'                 coastline = coastline,
#'                 coastline_paths = list(line = list(color = "red")))
#'
#' #### Example 7: Add plane
#' # Note that the plane height is automatically adjusted by stretch
#' plot3dscape(r = r,  stretch = 50, aspectmode = "data",
#'                 plane = -50)
#' # Customise via plane_surface
#' # Note that the legend is hidden automatically
#' plot3dscape(r = r,  stretch = 50, aspectmode = "data",
#'                 plane = -50, plane_surface = list(colorscale = "blue", showscale = FALSE))
#'
#' @author Edward Lavender
#' @export
#'

###############################################
###############################################
#### plot3dscape()

plot3dscape <-
  function(r,
           aggregate = NULL,
           add_surface = list(colors = grDevices::heat.colors(100)),
           add_markers = NULL,
           thin_markers = FALSE,
           buffer = NULL,
           add_paths = NULL,
           coastline = NULL,
           coastline_paths = list(line = list(color = grDevices::rgb(0, 0, 0), width = 4)),
           plane = NULL,
           plane_surface = list(showscale = FALSE),
           xlim = NULL, ylim = NULL, zlim = NULL,
           font = list(family = "sans", size = 18, color = "black"),
           xtitle = "", ytitle = "", ztitle = "",
           stretch = 1,
           aspectmode = "cube",
           eye = list(),
           verbose = TRUE
  ){

    #### Define buffer around points and clip spatial data, if required
    # Implement this first so that downstream stages are faster
    if(!is.null(buffer) & !is.null(add_markers)){
      if(verbose) cat("Cropping spatial data around buffered points... \n")
      xy_markers <- cbind(add_markers$x, add_markers$y)
      sp_markers <- sp::SpatialPoints(xy_markers, proj4string = sp::CRS(as.character(raster::crs(r))))
      buffer$spgeom <- sp_markers
      buf_markers <- do.call(rgeos::gBuffer, buffer)
      ecrop <- raster::extent(buf_markers)
      r <- raster::crop(r, ecrop)
      if(!is.null(coastline)){
        coastline <- raster::crop(coastline, ecrop)
      }
    }

    #### Aggregate bathymetry, if required.
    # Aggregate bathymetry
    if(!is.null(aggregate)){
      if(verbose) cat("Aggregating raster...\n")
      aggregate$x <- r
      r <- do.call(raster::aggregate, aggregate)
    }
    # Print a warning if the dimensions of the raster are too big for plotly"
    d1 <- dim(r)[1]
    d2 <- dim(r)[2]
    dtot <- d1 * d2
    if(dtot > 1800 * 1800){
      warning(
        paste0(
          "The dimensions of the cropped bathymetry file to be plotted are: ",
          d1, " x ", d2, ". ",
          "This is greater than than the maximum recommended resolution (c. 1800 x 1800) and the function may crash. Try supplying a list of arguments to the aggregate argument to reduce raster resolution."
        ))
    }

    #### Define bathymetry matrix, adjusted by the stretch factor
    if(verbose) cat("Defining plot properties...\n")
    r <- r*stretch
    z <- raster::as.matrix(r)
    # define x and y coordinates for matrix:
    xy <- raster::coordinates(r)
    x <- as.numeric(levels(factor(xy[,1])))
    y <- as.numeric(levels(factor(xy[,2])))
    y <- rev(y)

    #### Define axes properties
    # Extract limits, if not provided
    if(is.null(xlim)){
      xlim <- prettyGraphics::pretty_seq(x, lim = NULL, pretty_args = list(n = 5))
    }
    if(is.null(ylim)){
      ylim <- prettyGraphics::pretty_seq(y, lim = NULL, pretty_args = list(n = 5))
    }
    if(is.null(zlim)){
      zlim <- prettyGraphics::pretty_seq(as.vector(z), lim = NULL, pretty_args = list(n = 5))
    }
    # Define axis tick marks
    x_lab_ls <- list(title = xtitle, titlefont = font, range = xlim)
    y_lab_ls <- list(title = ytitle, titlefont = font, range = ylim)
    z_lab_ls <- list(title = ztitle, titlefont = font, range = zlim)

    #### Basic plot
    # Plot
    if(verbose) cat("Producing plot...\n")
    p <- plotly::plot_ly(x =  x, y = y, z = z) %>%
      plotly::layout(
        scene = list(
          xaxis = x_lab_ls,
          yaxis = y_lab_ls,
          zaxis = z_lab_ls,
          aspectmode = aspectmode,
          camera = list(eye = eye)
        ))
    # Add surface
    add_surface$p <- p
    p <- do.call(plotly::add_surface, add_surface)

    #### Add markers
    if(!is.null(add_markers)){
      # By default, we will add markers if specified, unless the inputed list fails some checks, below.
      add_markers_pass <- TRUE
      # Check add_markers contains, at a minimum, the coordinates required to add markers:
      if(!all(c("x", "y", "z") %in% names(add_markers))){
        warning("add_markers must be a named list with x, y and z coordinates; add_markers input ignored.")
        add_markers_pass <- FALSE
      }
      # Select only those markers which fall within the raster's horizontal domain, if requested:
      if(thin_markers & add_markers_pass){
        e <- raster::extent(r)
        xyz_markers <- data.frame(x = add_markers$x, y = add_markers$y, z = add_markers$z)
        pos_keep <- which(xyz_markers$x >= e[1] &
                                  xyz_markers$x <= e[2] &
                                  xyz_markers$y >= e[3] &
                                  xyz_markers$y <= e[4])
        if(length(pos_keep) == 0){
          warning("No markers within raster domain.")
          add_markers_pass <- FALSE
        } else{
          xyz_markers <- xyz_markers[pos_keep, ]
          add_markers$x <- xyz_markers$x
          add_markers$y <- xyz_markers$y
          add_markers$z <- xyz_markers$z
        }
      }
      if(add_markers_pass){
        add_markers$p <- p
        p <- do.call(plotly::add_markers, add_markers)
      }

    }

    #### Add paths
    if(!is.null(add_paths)){
      add_paths$p <- p
      p <- do.call(plotly::add_paths, add_paths) %>% plotly::layout(showlegend = FALSE)
    }

    #### Add plane
    if(!is.null(plane)){
      plane_mat <- z
      plane_mat[] <- plane * stretch
      plane_surface$p <- p
      plane_surface$x <- x
      plane_surface$y <- y
      plane_surface$z <- plane_mat
      p <- do.call(plotly::add_surface, plane_surface) %>% plotly::layout(showlegend = FALSE)
    }

    #### Add coastline
    if(!is.null(coastline)){
      if(length(coastline) > 0){
        # For each element in coastline
        for(poly in 1:length(coastline)){
          # Directory of coordinates for all polygons
          dir <- coastline@polygons[[poly]]@Polygons
          # Extract coordinates for each polygon
          ls <- lapply(dir, function(d){
            # create a dataframe containing the coordinates of that polygon
            d <- data.frame(d@coords)
            colnames(d) <- c("x", "y")
            # add a z coordinate (to plot the lines at 0 depth)
            d$z <- 0
            return(d)
          })
          # for each element in our list, we'll update the plot
          for(i in 1:length(ls)){
            # extract x, y and z coordinates
            coastline_paths$p <- p
            coastline_paths$x  <- ls[[i]]$x
            coastline_paths$y  <- ls[[i]]$y
            coastline_paths$z  <- ls[[i]]$z
            p <- do.call(plotly::add_paths, coastline_paths) %>%
              plotly::layout(showlegend = FALSE)
          }
        }
      }
    }

    #### Return the completed plot.
    return(p)

  }


#### End of code.
###############################################
###############################################
