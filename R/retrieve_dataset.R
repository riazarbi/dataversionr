retrieve_dataset <- function(prefix,
                             destination,
                             timestamp_filter = NA,
                             collect = TRUE,
                             verbose = FALSE) {


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
    ds <- open_dataset(dataset_prefix$path("latest"))
    if(collect) {
      ds <- ds %>% collect() %>% as_tibble()
      return(ds)
    } else {
      return(ds)
    }

  } else {
    timestamp_filter <- with_tz(timestamp_filter, tzone = "UTC")
    if (verbose) {
      message(paste(
        "Reconstructing dataset at timestamp",
        timestamp_filter,
        "UTF"
      ))
      if(!collect) {
        message(
          "collect parameter is ignored when timestamp parameter is set."
        )
      }
    }

    open_dataset(dataset_prefix$path("history")) %>%
      filter(.data$timestamp <= timestamp_filter) %>%
      collect() %>%
      arrange(.data$timestamp) %>%
      group_by(across(all_of(c(key_vars)))) %>%
      summarise(across(everything(), last), .groups = "keep") %>%
      ungroup() %>%
      filter(.data$operation != "delete") %>%
      select(-.data$timestamp,-.data$operation)

  }
}
