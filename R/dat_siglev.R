#' @title The siglev constants for each Sigma layer
#' @description A dataset defining the siglev constants of each Sigma layer. These constants, combined with tidal elevation (m) and depth below mean sea level, can be used to compute the depths of Sigma layers at a given location and time.
#'
#' @format A data frame with 11 rows and 2 variables:
#' \describe{
#'   \item{layer}{A unique identifier of each layer. The first layer is the surface; layer 10 is the seabed and layer 11 is slightly below the seabed.}
#'   \item{siglev}{The siglev constant for each layer.}
#' }
#' @source Dmitry Aleynik
"dat_siglev"
