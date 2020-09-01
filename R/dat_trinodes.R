#' @title The nodes which surround each element
#' @description A dataset containing the nodes which surround each element for a subset of the WeStCOMS mesh in an area around Oban (see \code{\link[fvcom.tbx]{dat_area_boundaries}}).
#'
#' @format A data frame with 474 rows and 4 variables:
#' \describe{
#'   \item{element_id}{A unique identifier of each element.}
#'   \item{node1, node2, node3}{A unique identifier for each of the three nodes which surround the element.}
#' }
#' @source Dmitry Aleynik
"dat_trinodes"
