#' @title Build unstructured meshes
#' @description This function is used to build an unstructured mesh (around nodes or elements) from node coordinates and connections as a SpatialPolygonsDataFrame in R. This function was designed with the WeStCOMS unstructured mesh in mind.
#'
#' @param nodexy A dataframe containing node ids and coordinates. The dataframe should have three columns: node_id, x and y. See \code{?WeStCOMSExploreR::dat_nodexy} for the dataset included in WeStCOMSExploreR as a guide.
#' @param trinodes A dataframe containing element ids and the surrounding nodes (i.e. which nodes are linked to which other nodes). The dataframe should have four columns: element_id, node1, node2 and node3. See \code{?WeStCOMSExploreR::dat_trinodes} for the dataset included in WeStCOMSExploreR as a guide.
#' @param mesh_type A character specifying the mesh type you want to build. There are two options: "node" or "element". \code{mesh_type = "node"} builds a mesh based on nodes (i.e. around elements). This is useful for plotting conditions resolved at elements. \code{mesh_type = "element"} builds a mesh around nodes based on elements. This is useful for plotting conditions resolved at nodes.
#' @param proj4string A projection string of class \code{\link[sp]{CRS-class}}. The World Geodetic System 84 (WGS84), i.e. \code{proj4string = sp::CRS(as.character("+init=epsg:4326"))}, is the default.
#' @param cl A cluster object created by \code{\link[parallel]{makeCluster}}. This is required if you want to run the algorithm in parallel, which can improve computation time (especially for large meshes). The default is NULL (i.e. the algorithm is run on a single processor: see examples). If supplied, the connection to the cluster is stopped within the function.
#' @param pass2varlist A list containing a character vector of names of objects to export, to be passed to the \code{varlist} argument of \code{\link[parallel]{clusterExport}}. This is required if \code{cl} is supplied and you specify some function arguments via objects, rather than directly. (See the use of \code{WeStCOMSExploreR::dat_nodexy} and \code{WeStCOMSExploreR::dat_trinodes} in the examples.) These objects must be located in the global environment.
#'
#' @return An SpatialPolygonsDataFrame (see \code{\link[sp]{SpatialPolygonsDataFrame-class}}). Each polygon has an ID corresponding to the ID of the node or element which is surrounds, as supplied by the nodexy or trinodes dataframe respectively.
#'
#' @examples
#'
#' # 1) Build a mesh around elements (based on nodes) on a single processor
#' # You will receive a warning when you run this:
#' # ...'In sp::Polygon(coords, hole) : less than 4 coordinates in polygon'.
#' # This can be safely ignored. This is because each polygon is a prism;
#' # ... i.e., only comprised of three coordinates.
#' mesh_around_elements <- build_mesh(nodexy = dat_nodexy,
#'                                    trinodes = dat_trinodes,
#'                                    mesh_type = "node",
#'                                    cl = NULL,
#'                                    pass2varlist = list(NULL))
#'
#' # 2) Build a mesh around nodes (based on elements) on a single processor
#' mesh_around_nodes <- build_mesh(nodexy = dat_nodexy,
#'                                 trinodes = dat_trinodes,
#'                                 mesh_type = "element",
#'                                 cl = NULL,
#'                                 pass2varlist = list(NULL))
#'
#' # 3) Build a mesh around elements (based on nodes) using parallel processing
#' \dontrun{
#' # Define cluster object:
#' cl <- parallel::makeCluster(2L)
#' # Run the build_mesh algorithm in parallel by supplying a cluster:
#' mesh_around_elements <- build_mesh(nodexy = dat_nodexy,
#'                                    trinodes = dat_trinodes,
#'                                    mesh_type = "node",
#'                                    cl = cl,
#'                                    pass2varlist = list("dat_nodexy", "dat_trinodes"))
#' # Note that the connection with the cluster is closed within the function
#' # ... so it is not necessary to use parallel::stopCluster(cl) here.
#' }
#'
#' @seealso \code{\link[sp]{SpatialPolygonsDataFrame-class}} for the output class;
#' \code{\link[WeStCOMSExploreR]{dat_nodexy}} for an example nodexy dataframe;
#' \code{\link[WeStCOMSExploreR]{dat_trinodes}} for an example trinodes dataframe;
#' \code{\link[parallel]{makeCluster}} and \code{\link[parallel]{clusterExport}} for more information on parallelisation.
#'
#' @author Edward Lavender
#'
#' @export
#'


###################################################
###################################################
#### build_mesh

# build_mesh
build_mesh <-
  function(
    nodexy,
    trinodes,
    mesh_type = "element",
    proj4string = sp::CRS(as.character("+init=epsg:4326")),
    cl = NULL,
    pass2varlist = list(NULL)
    ){



  ###################################################
  #### Check mesh_type is allowed:

  if(!(mesh_type %in% c("node", "element"))){
    stop("The inputted mesh_type is not supported. The only options are 'node' or 'element'.")
  }

  #### Message
  cat("Step 1/2: Building mesh as a list of SpatialPolygons... \n")

  #### Export necessary objects to cluster:
  if(!is.null(cl)){
    parallel::clusterExport(cl = cl, varlist = pass2varlist)
  }

  ###################################################
  #### A mesh around elements based on nodes

  #### mesh_type = "node"
  # The first type of mesh we can construct is a mesh around elements but based on nodes.
  # Specifically, the node mesh structure, with nodes defining prisms, defines a mesh
  # ... around elements. So this type of mesh is useful if we want to plot conditions
  # ... resolved at elements.
  if(mesh_type == "node"){

    # Define a list of SpatialPolygons, each one of which is a triangular prism
    # ... around a given element:
    mesh_spls <-
      # Loop over every element......
      # Use the pblapply() function here because the process may take a few minutes
      # ... or longer with larger meshes
      pbapply::pblapply(trinodes$element_id, function(i){
        # identify the tri nodes around that element:
        trio <- trinodes[trinodes$element_id == i, ]
        # obtain the coordinates of all three nodes:
        node1xy <- nodexy[nodexy$node_id == trio$node1, c("x", "y")]
        node2xy <- nodexy[nodexy$node_id == trio$node2, c("x", "y")]
        node3xy <- nodexy[nodexy$node_id == trio$node3, c("x", "y")]
        # Define a matrix
        xynode_mat <- as.matrix(rbind(node1xy, node2xy, node3xy), ncol = 2)
        # convert to polygons with a unique id BASED ON ELEMENT (i) using the proj4string provided by the user:
        poly <- Orcs::coords2Polygons(xynode_mat, proj4string = proj4string, ID = i)
        # return the polygon
        return(poly)
      },
      cl = cl
      ) # close function(i) and lapply

  } # close if(mesh_type = "node"){



  ###################################################
  #### A mesh around nodes based on elements

  #### mesh_type == "element"
  # The second type of mesh we can construct is based on elements.
  # Specifically, aroud each node, we define the MCP around all the elements which
  # ... surround that node. This is useful for plotting
  # ... environmental variables that are resolved at nodes, because
  # ... a node is at the centre of every grid cell.
  if(mesh_type == "element"){
    # Define a list of SpatialPolygons, each one of which will be a
    # ... grid cell surrounding a central node.
    mesh_spls <-
      # Loop over every UNIQUE node...
      pbapply::pblapply(nodexy$node_id, function(i){
        # Identify the node of interest
        node <- i
        # Define the elements in the trinodes dataframe
        # ... which contain this node:
        elements_with_node <-
          trinodes[
            unique(which(trinodes[, c("node1", "node2", "node3")] == node, arr.ind = TRUE)[, 1]), "element_id"]

        # If there are more than two elements in elements_with_node i.e., if the node in
        # ... question is part of more than two prisms, then we'll define the positions of the elements
        # ... in each prism in which the node is a part and then create a SpatialPolygon around those.
        # Note that it is important that the node is part of at least 3 prisms because otherwise
        # ... we'll only calculate the position of two (or fewer) elements and, when we connect these up,
        # ... we'll be left with a SpatialLines object not a SpatialPolygons object, which is necessary because,
        # ... below, we coerce SpatialPolygons into a SpatialPolygonsDataFrame for plotting.
        # Note that the node may only be connected to < 2 prisms if its on the boundary of the mesh.
        nprisms <- length(elements_with_node)
        if(nprisms > 2){

          # Identify all the nodes in the prisms that surround the node of interest:
          all_nodes <- trinodes[trinodes$element_id %in% elements_with_node, ]
          # Create a blank matrix in which we'll store the x and y coordinates of
          # ... each prism:
          element_xy <- matrix(NA, ncol = 2, nrow = nprisms)
          # Loop over every prism, i.e. collection of three nodes,
          # ... and calculate their average position (i.e. the position of the element)
          for(j in 1:nprisms){
            # focus in on the trio of nodes of interest:
            trio <- all_nodes[j, ]
            # obtain the coordinates of all three nodes
            node1xy <- nodexy[nodexy$node_id == trio$node1, c("x", "y")]
            node2xy <- nodexy[nodexy$node_id == trio$node2, c("x", "y")]
            node3xy <- nodexy[nodexy$node_id == trio$node3, c("x", "y")]
            # average x and y coordinates and add these averages to the correct
            # ... position in the element_xy matrix
            element_xy[j, 1] <- mean(c(node1xy$x, node2xy$x, node3xy$x))
            element_xy[j, 2] <- mean(c(node1xy$y, node2xy$y, node3xy$y))
          } # close for(j in 1:nprisms){

          # Convert the positions of the element to a SpatialPoints object
          # ... with a proj4string as defined by the user (the default is WGS84)
          element_xy_sp <- sp::SpatialPoints(element_xy, proj4string = proj4string)

          # define the mcp enclosing these elements, which will have the selected
          # ... node at its heart and provide a unique id for this polygon
          # ... BASED ON NODE...
          poly <- rgeos::gConvexHull(element_xy_sp, id = node)

          # return the polygon
          # return(poly)
        } # close if(nprisms > 2){

      },
      cl = cl
      ) # close function(i) and pblapply

    # Remove any NULL elements of this list i.e. cases where there were
    # ... fewer than three prisms around a node so we couldn't create a polgyon
    # ... around that node based on elements.
    # mesh_spls <- plyr::compact(mesh_spls)
    mesh_spls <- mesh_spls[which(!sapply(mesh_spls, is.null))]

  } # close if(mesh_type == "element"){


  ###################################################
  #### Create a SpatialPolygonsDataFrame from the list

  #### Close the connection with the cluster if this has been supplied:
  if(!is.null(cl)){
    parallel::stopCluster(cl)
  }

  #### Message
  cat("Step 2/2: Converting the mesh to a SpatialPolygonsDataFrame... \n")

  # We have a list of SpatialPolygons, each one either surrouding an element
  # ... or surrouding a node. We want to convert this list into a SpatialPolygonsDataFrame
  # ... to improve speed plotting.
  # This section is adapted from:
  # https://stackoverflow.com/questions/46890221/r-spatialpolygonsdataframe-from-a-list-of-spatialpolygons

  #### The first step is to make a single SpatialPolygon from a list of SpatialPolygons:
  # (1) Define SpatialPolygons from list of polygons
  mesh_sp <-
    sp::SpatialPolygons(
      lapply(mesh_spls, function(x) methods::slot(x, "polygons")[[1]]))

  #### Next, use the SpatialPolygon to create a SpatialPolygonsDataFrame
  # (2) Creating a dataframe of IDs from SpatialPolygons:
  mesh_df <- data.frame(ID = sapply(methods::slot(mesh_sp, "polygons"), function(x) methods::slot(x, "ID")))

  # (3) Make the IDs row names in the dataframe
  # (row.names of data and Polygons IDs need to match)
  row.names(mesh_df) <- sapply(methods::slot(mesh_sp, "polygons"), function(x) methods::slot(x, "ID"))

  # (4) Make the SpatialPolygonsDataFrame
  mesh_spdf <- sp::SpatialPolygonsDataFrame(mesh_sp, data = mesh_df, match.ID = TRUE)
  names(mesh_spdf) <- "ID"


  ###################################################
  #### Return mesh_spdf

  return(mesh_spdf)

} # close build_mesh function



#### End of code.
###################################################
###################################################
