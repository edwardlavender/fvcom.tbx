#' @title Find the coordinates of specified mesh cells
#' @description This function finds the coordinates of specified mesh cells. For nodes, these can be extracted from the \code{nodexy} dataframe or from the mesh. The latter method is not quite as accurate and is slower; this difference may become noticeable for large meshes. However, for elements, the extraction of coordinates from the mesh is the only implemented method.
#'
#' @param mesh_ID A vector of integers which define mesh IDs for which coordinates are designed.
#' @param mesh A mesh, created by \code{\link[WeStCOMSExploreR]{build_mesh}}, around nodes or elements.
#' @param nodexy (optional) A dataframe with columns which specify the coordinates and IDs of each cell in mesh around nodes in columns named 'node_id', 'x' and 'y' (see \code{\link[WeStCOMSExploreR]{dat_nodexy}}). If provided, coordinates are extracted from \code{nodexy} rather than \code{mesh}. This is recommended.
#'
#' @return The function returns a dataframe with mesh IDs ('mesh_ID') and corresponding coordinates ('x' and 'y').
#'
#' @examples
#'
#' #### Define mesh cells for which we want to extract coordinates
#' # In this hypothetical situation, we'll use mesh IDs for which we know their coordinates
#' # ... so that we can show that we can compare the two approaches:
#' set.seed(1)
#' select <- sample(1:nrow(dat_nodexy), 10)
#' dat <- dat_nodexy[select, ]
#'
#' #### Examples: find_xy using nodexy and mesh options respectively
#' dat_xy1 <- find_xy(dat$node_id, nodexy = dat_nodexy)
#' dat_xy2 <- find_xy(dat$node_id, mesh = dat_mesh_around_nodes)
#'
#' #### Coordinates from the nodexy method are exactly correct but coordinates from the mesh
#' # ... are not as accurate:
#' identical(dat, dat_xy1)
#' identical(dat, dat_xy2)
#' cbind(dat, dat_xy1, dat_xy2)
#'
#' #### The nodexy approach is also considerably faster. For these reasons, the nodexy
#' # ... approach is recommended for node cells where possible.
#' microbenchmark::microbenchmark(find_xy(dat$node_id, nodexy = dat_nodexy),
#'                                find_xy(dat$node_id, mesh = dat_mesh_around_nodes)
#' )
#'
#' @author Edward Lavender
#' @export
#'

##############################################
##############################################
#### find_xy()

find_xy <-
  function(mesh_ID, mesh, nodexy = NULL){

    #### If nodexy has been provided, we will use this to extract coordinates for speed.
    if(!is.null(nodexy)){
      # Check nodexy contains necessary columns
      stopifnot(all(c("node_id", "x", "y") %in% colnames(nodexy)))
      # Check the classes of inputted mesh_IDs and node_id in nodexy are identical
      if(class(mesh_ID) != class(nodexy$node_id)){
        warning("class(mesh_ID) != class(nodexy$node_id): both adjusted using function(x) as.numeric(as.character(x)).")
        mesh_ID <- as.numeric(as.character(mesh_ID))
        nodexy$mesh_id <- as.numeric(as.character(nodexy))
      }

      #### Extract the coordinates of mesh IDs from nodexy and return dataframe.
      dat_xy <- nodexy[nodexy$node_id %in% mesh_ID, ]

      #### Reorder dat_xy by node_id so that the mesh_IDs are returned in the same order
      dat_xy <- dat_xy[order(factor(as.character(dat_xy$node_id), levels = as.character(mesh_ID))), ]


      #### If nodexy has not been provided, we'll use the provided mesh
      } else{

        #### Extract the coordinates of the mesh and define dataframe
        # Note mesh is assumed to have an ID column.
        mesh_xy <- sp::coordinates(mesh)
        mesh_xy <- data.frame(mesh_ID = mesh$ID, x = mesh_xy[, 1], y = mesh_xy[, 2])
        mesh_xy$mesh_ID <- as.numeric(as.character(mesh_xy$mesh_ID))
        if(!(class(mesh_ID) %in% c("integer", "numeric"))){
          warning("class(mesh_ID) not integer or numeric. Class adjusted using function(x) as.numeric(as.character(x)).")
          mesh_ID <- as.numeric(as.character(mesh_ID))
        }

        #### Extract coordinates from mesh
        dat_xy <- mesh_xy[mesh_xy$mesh_ID %in% mesh_ID, ]

        #### Reorder dat_xy by node_ID so that the mesh_IDs are returned in the same order
        dat_xy <- dat_xy[order(factor(as.character(dat_xy$mesh_ID), levels = as.character(mesh_ID))), ]

      }

    #### Return dataframe including coordinates.
    return(dat_xy)

  }


#### End of code.
##############################################
##############################################
