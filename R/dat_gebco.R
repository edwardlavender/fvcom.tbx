#' @title The bathymetry around oban
#' @description A dataset of the bathymetry (m) around Oban, provided by the General Bathymetric Chart of the Oceans (GEBCO).
#'
#' @format A \code{\link[raster]{raster}} with 36 rows, 36 columns and 1296 cells, with the following properties:
#' \describe{
#'   \item{dimensions}{36, 36, 1296  (nrow, ncol, ncell)}
#'   \item{resolution}{0.004166667, 0.004166667  (x, y)}
#'   \item{extent}{-5.545833, -5.395833, 56.34167, 56.49167  (xmin, xmax, ymin, ymax)}
#'   \item{crs}{+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 }
#' }
#'
#' @source GEBCO Compilation Group (2019) GEBCO 2019 Grid (doi:10.5285/836f016a-33be-6ddc-e053-6c86abc0788e)
"dat_gebco"
