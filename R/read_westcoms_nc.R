nc <- "/Users/el72/Downloads/minch2_20160815_0006.nc"

# install.packages("ncdf")
# Warning in install.packages :
#  package ‘ncdf’ is not available (for R version 3.6.1)
# devtools::install_github("cran/ncdf")
# ERROR

nc
nc <- ncdf4::nc_open(nc, verbose = TRUE, write = FALSE)
nc <- raster::raster(nc)

