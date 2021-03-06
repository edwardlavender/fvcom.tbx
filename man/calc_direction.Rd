% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_helpers.R
\name{calc_direction}
\alias{calc_direction}
\title{Calculate wind or current direction from u and v component vectors}
\usage{
calc_direction(l)
}
\arguments{
\item{l}{A list composed of two environmental arrays (u and v component arrays) for a particular day.}
}
\value{
The function returns an array comprising wind/current directions for each cell in the original array.
}
\description{
This function calculates wind or current direction from u and v component vector arrays.
}
\details{
Wind or current direction are expressed as the direction (degrees) of mass flow.
}
\examples{
#### Step 1: Read example u and v files into a list
# Define the path to the u and v files:
path <- system.file("WeStCOMS_files",
                    package = "fvcom.tbx", mustWork = TRUE)
path <- paste0(path, "/")
path_u <- paste0(path, "uwind_speed")
path_v <- paste0(path, "vwind_speed")
# Define the source files:
source_u <- list.files(path_u, full.names = TRUE)[1]
source_v <- list.files(path_v, full.names = TRUE)[1]
# Define a list comprising u and v arrays with one element for each day:
l <- lapply(list(source_u, source_v), function(source){
              R.matlab::readMat(source)$data
           })

#### Step 2: Compute wind direction
wind_direction <- calc_direction(l)
graphics::hist(wind_direction)

}
\seealso{
This function can be implemented within \code{\link[fvcom.tbx]{compute_field_from_fvcom}}.
}
\author{
Edward Lavender
}
