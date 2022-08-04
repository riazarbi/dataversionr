

retrieve_dv_diff <- function(destination, as_of, key_cols) {
  if(is.na(as_of)) {
    as_of <- lubridate::now()
  }

  as_of <- lubridate::with_tz(as_of, tzone = "UTC")

  if(!("POSIXct" %in% class(as_of))) {
    stop("parameter as_of must be of class POSIXct. An easy way to create such an object is with lubridate::as_datetime()")
  }

  diff_ds <- get_diffs(destination, collect = FALSE)
  diff_ds_filtered <- filter(diff_ds, .data$diff_timestamp <= as_of) %>%
    arrange(.data$diff_timestamp) %>% collect()



  if(nrow(diff_ds_filtered) == 0) {
    stop("No diffs older than the specified as_of date found.")
  }


  dv <- diff_ds_filtered %>%
    group_by(across(all_of(c(key_cols)))) %>%
    summarise(across(everything(), last), .groups = "keep") %>%
    ungroup() %>%
    filter(.data$operation != "deleted") %>%
    select(-.data$diff_timestamp,-.data$operation) %>%
    as.data.frame

  return(dv)

}
