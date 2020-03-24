##############################################
##############################################
#### Prepare_WeStCOMS_Data

#### Read in data:
setwd("/Users/el72/Documents/PhD/Academic_PhD_Work/FVCOM/WeStCOMS R code/R Package/WeStCOMSExplorer/WeStCOMSExploreR/data-raw/")
# mesh dfs
dat_trinodes <- readRDS("dat_trinodes.rda")
dat_nodexy <- readRDS("dat_nodexy.rda")
# meshes
dat_mesh_around_elements <- readRDS("dat_mesh_around_elements.rda")
dat_mesh_around_nodes <- readRDS("dat_mesh_around_nodes.rda")
# area boundaries and coastline
dat_area_boundaries <- rlist::list.load("dat_area_boundaries.RData")
dat_coast_around_oban <- readRDS("dat_coast_around_oban.rda")
# sample WeStCOMS dataframes
dat_temp <- readRDS("dat_temp.rda")
dat_tidal_elevation <- readRDS("dat_tidal_elevation.rda")
dat_uwind_speed <- readRDS("dat_uwind_speed.rda")
dat_vwind_speed <- readRDS("dat_vwind_speed.rda")
# data to compute sigma layer depths
dat_siglev <- data.frame(layer = 1:11,
                         siglev = c(0.00000000,
                                    -0.02196982,
                                    -0.06761890,
                                    -0.15559244,
                                    -0.30293667,
                                    -0.50000000,
                                    -0.69706333,
                                    -0.84440756,
                                    -0.93238109,
                                    -0.97803020,
                                    -1.00000000)
                         )
dat_h <- readRDS("dat_h.rda")

#### Checks
# mesh dfs
head(dat_trinodes); nrow(dat_trinodes)
head(dat_nodexy); nrow(dat_nodexy)
# meshes
raster::plot(dat_mesh_around_elements)
raster::plot(dat_mesh_around_nodes)
# area boundaries and coastline
dat_area_boundaries
raster::plot(dat_coast_around_oban)
# sample WeStCOMS dataframes
head(dat_temp)
head(dat_tidal_elevation)
head(dat_uwind_speed)
head(dat_vwind_speed)

#### use usethis::use_data() to add the data to the package appropriately
# mesh dfs
usethis::use_data(dat_trinodes, overwrite = TRUE)
usethis::use_data(dat_nodexy, overwrite = TRUE)
# meshes
usethis::use_data(dat_mesh_around_elements, overwrite = TRUE)
usethis::use_data(dat_mesh_around_nodes, overwrite = TRUE)
# area boundaries and coastline
usethis::use_data(dat_area_boundaries, overwrite = TRUE)
usethis::use_data(dat_coast_around_oban, overwrite = TRUE)
# sample WeStCOMS dataframes
usethis::use_data(dat_temp, overwrite = TRUE)
usethis::use_data(dat_tidal_elevation, overwrite = TRUE)
usethis::use_data(dat_uwind_speed, overwrite = TRUE)
usethis::use_data(dat_vwind_speed, overwrite = TRUE)
# data to compute sigma layer depths
usethis::use_data(dat_h, overwrite = TRUE)
usethis::use_data(dat_siglev, overwrite = TRUE)

#### End of code.
##############################################
##############################################
