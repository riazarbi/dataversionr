retrieve_dataset_diffs <- function(prefix,
                                   destination,
                                   timestamp_filter = NA,
                                   collect = TRUE,
                                   verbose = FALSE) {
  if(!collect) {
    stop("Running collect = FALSE is not yet implemented for this function.")
  }

  dataset_prefix <- destination$cd(prefix)

  key_cols_history <- retrieve_dataset_diff_stats(prefix,
                                                  destination,
                                                  first_principles = FALSE,
                                                  verbose = verbose) %>%
    pull(.data$key_cols) %>% unique

  key_vars <- unlist(strsplit(key_cols_history[length(key_cols_history)], "\\|"))


  if (is.na(timestamp_filter)) {
    if (verbose) {
      message("No timestamp set. Retrieving latest copy.")
    }
    open_dataset(dataset_prefix$path("history")) %>%
      collect %>%
      as_tibble

  } else {
    timestamp_filter <- with_tz(timestamp_filter, tzone = "UTC")
    if (verbose) {
      message(paste(
        "Filtering dataset to timestamp",
        timestamp_filter,
        "UTF"
      ))
    }


    open_dataset(dataset_prefix$path("history")) %>%
      filter(timestamp <= timestamp_filter) %>%
      collect() %>%
      as_tibble

  }
}

