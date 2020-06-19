#' @title Identify the mesh cells enclosing coordinates
#' @description This function identifies the mesh cells which surround inputted coordinates. Coordinates can be inputted as \code{lat} and \code{long} vectors, a dataframe (\code{dat}) or a \code{\link[sp]{SpatialPoints-class}} object (\code{xysp}). The function can return a dataframe or vector of mesh IDs for each pair of coordinates or each unique pair of coordinates inputted.
#'
#' @param lat A numeric vector of latitudes (in decimal degrees). Alternatively, \code{dat} or \code{xysp} can be provided (see below).
#' @param long A numeric vector of longitudes (in decimal degrees). Alternatively, \code{dat} or \code{xysp} can be provided (see below).
#' @param dat A dataframe with columns 'lat' and 'long' (in decimal degrees) at which mesh IDs will be identified. Alternatively, \code{lat} and \code{long} or \code{xysp} can be provided.
#' @param xysp A \code{\link[sp]{SpatialPoints-class}} object which defines the positions at which mesh IDs are defined. Alternatively, \code{lat} and \code{long} or \code{dat} can be provided (see above).
#' @param mesh A mesh, created by \code{\link[WeStCOMSExploreR]{build_mesh}}, that surrounds nodes or elements.
#' @param proj A projection string of class CRS-class. The default is World Geodetic System 84 (WGS84). This should be the same as for \code{mesh} and inputted coordinates.
#' @param f A function to process mesh IDs after identification. For example, \code{function(x) as.numeric(as.character(x))} can be useful.
#' @param return A number (1, 2, 3 or 4) which specifies the format of the return output. \code{return = 1} returns a dataframe with columns 'lat', 'long' and 'mesh_ID', comprising only unique coordinates. (Note that this may contain duplicate mesh IDs because unique coordinates may lie within the same mesh cell.) \code{return = 2} only the mesh IDs corresponding to unique coordinates (e.g. to add to an existing dataframe). \code{return = 3} returns a dataframe, as above, but including all inputted coordinate pairs. (This will be the same as outputted with \code{return = 1} if their are no duplicate coordinates.) \code{return = 4} returns only the mesh IDs corresponding to these coordinates.
#'
#' @details Ensure that the coordinate system of inputted coordinates, the mesh and the projection are identical (whether directly specified, e.g. if coordinates are specified via a \code{\link[sp]{SpatialPoints-class}} object, or otherwise or not (e.g. if coordinates are supplied via \code{lat} and \code{long} vectors.
#'
#' @return The function returns a vector or a dataframe, depending on the input to \code{return} (see above).
#'
#' @examples
#'
#' #### Define some coordinates for which to identify the mesh cell ID
#' # In this hypothetical example, we'll choose coordinates for which
#' # ... we know the mesh ID in advance, so we can vertify the function
#' # ... returns the correct outputs:
#' set.seed(1)
#' xy <- sp::coordinates(dat_mesh_around_nodes)
#' select <- sample(1:nrow(xy), 10)
#' xy <- xy[select, ]
#' selected_nodes <- as.numeric(rownames(xy))
#' lat <- as.numeric(xy[, 2])
#' long <- as.numeric(xy[, 1])
#'
#' #### Set the mesh projection appropriately:
#' proj <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#' raster::crs(dat_mesh_around_nodes) <- proj
#'
#' #### Example 1: Find cells from lat/long input:
#' cells <- find_cells(lat = lat,
#'                     long = long,
#'                     mesh = dat_mesh_around_nodes,
#'                     f = function(x) as.numeric(as.character(x)),
#'                     return = 1)
#' cells
#' identical(cells$mesh_ID, selected_nodes)
#'
#' #### Example 2: Find cells from dat input:
#' cells <- find_cells(dat = data.frame(lat = lat, long = long),
#'                     mesh = dat_mesh_around_nodes,
#'                     f = function(x) as.numeric(as.character(x)),
#'                     return = 1)
#' identical(cells$mesh_ID, selected_nodes)
#'
#' #### Example 3: Find cells from SpatialPoints input:
#' cells <- find_cells(xysp = sp::SpatialPoints(xy, proj4string = proj),
#'                     mesh = dat_mesh_around_nodes,
#'                     f = function(x) as.numeric(as.character(x)),
#'                     return = 1)
#' identical(cells$mesh_ID, selected_nodes)
#'
#' #### Example 4: Find cells for unique coordinates or all coordinates in a dataframe:
#' # Imagine we have duplicate coordinates:
#' xy <- rbind(xy, xy)
#' rownames(xy) <- NULL
#' # Look over each return type (1:4) and compare outputs:
#' cells_ls <-
#'   lapply(1:4, function(i){
#'     cells <- find_cells(xysp = sp::SpatialPoints(xy, proj4string = proj),
#'                         long = long,
#'                         mesh = dat_mesh_around_nodes,
#'                         f = function(x) as.numeric(as.character(x)),
#'                         return = i)
#'     return(cells)
#'   })
#' utils::str(cells_ls)
#'
#' @author Edward Lavender
#' @export
#'

#############################################
#############################################
#### find_cells()

find_cells <-
  function(lat,
           long,
           dat = NULL,
           xysp = NULL,
           mesh,
           proj = sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
           f = NULL,
           return = 1){

    #### If the user has not provided spatial points, then we need to define/check dat
    if(is.null(xysp)){

      #### Define/check dat
      # If the user has not provided dat, we'll define dat using lat/long inputs.
      if(is.null(dat)){
        dat <- data.frame(lat = lat, long = long)
      # If dat has been provided, ensure this contains the correct columns:
      } else{
        stopifnot(all(c("lat", "long") %in% colnames(dat)))
      }

    } else{

      #### If xysp was provided, we need to define dat differently:
      # ... extract coordinates from xysp and make dat (needed for outputs)
      xy <- sp::coordinates(xysp) # returns a matrix with long = [, 1] and lat = [, 2]
      dat <- data.frame(lat = xy[, 2], long = xy[, 1])

    }

    #### Extract unique coordinates for speed when it comes to extracting coordinates
    dat_locs <- data.frame(f = unique(paste0(dat$lat, "_", dat$long)))
    dat_locs[, c("lat", "long")] <- as.numeric(stringr::str_split_fixed(dat_locs$f, "_", 2)[, 1:2])

    #### Define SpatialPoints
    dat_locs_sp <- sp::SpatialPoints(dat_locs[, c("long", "lat")], proj4string = proj)

    #### Define mesh IDs for each unique pair of coordinates
    dat_locs$mesh_ID <- unlist(sp::over(dat_locs_sp, mesh))

    #### Process mesh IDs if requested (e.g. as.numeric(as.character()) can be useful)
    if(!is.null(f)){
      dat_locs$mesh_ID <- f(dat_locs$mesh_ID)
    }

    #### Return outputs, either:
    # a) a dataframe comprising unique coordinates and IDs (e.g. dat_locs)
    # b) only the unique IDs (i.e., dat_locs$meshID)
    # c) the original dataframe + node IDs
    # d) only the mesh IDs corresponding to the original dataframe
    if(return == 1 | !(return %in% 1:4)){
      if(!(return %in% 1:4)) warning(paste("return", return, "unsupported. Supported options are 1:4. return = 1 used."))
      dat_locs <- dat_locs[, c("lat", "long", "mesh_ID")]
      return(dat_locs)
    } else if(return == 2){
      return(dat_locs$mesh_ID)
    } else if(return == 3 | return == 4){
      dat$mesh_ID <- dat_locs$mesh_ID[match(paste0(dat$lat, "_", dat$long), dat_locs$f)]
      if(return == 3) return(dat) else return(dat$mesh_ID)
    }

  }


#### End of code.
#############################################
#############################################
