compute_field_photoperiod <-
  function(
    nodexy,
    date,
    interval = c("dawn", "dusk"),
    hours,
    tz = "UTC",
    verbose = TRUE
  ){

    #### Checks
    ## Check nodexy contains required names
    check_names(arg = "nodexy",
                input = nodexy,
                req = c("x", "y"),
                extract_names = colnames,
                type = all)

    #### Define a dataframe with the photoperiod on each date
    nodexy_nrw <- nrow(nodexy)
    loc_index <- 1:nodexy_nrw
    dat <- expand.grid(date, loc_index)
    colnames(dat) <- c("date", "loc")
    dat$lon <- nodexy$x[match(dat$loc, loc_index)]
    dat$lat <- nodexy$y[match(dat$loc, loc_index)]
    dat$loc <- NULL
    photoperiod_df <- suncalc::getSunlightTimes(data = dat,
                                                keep = interval,
                                                tz = tz)

    #### Calculate photoperiod in user-specified units :
    photoperiod_df$photoperiod <-
      as.numeric(difftime(photoperiod_df[, interval[2]],
                          photoperiod_df[, interval[2]],
                          units = units)
                 )

    #### Return data.frame
    return(photoperiod_df)

  }
