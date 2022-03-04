retrieve_dataset_diff_stats <- function(prefix,
                                        destination,
                                        first_principles = FALSE,
                                        verbose = FALSE) {


  dataset_prefix <- destination$cd(prefix)


  if (first_principles) {
    if (verbose) {
      message("Computing diff stats from first principles...")
    }
    open_dataset(dataset_prefix$path("history")) %>%
      collect() %>%
      group_by(.data$timestamp, .data$operation) %>%
      summarise(count = as.integer(n()), .groups = "keep") %>%
      ungroup() %>%
      pivot_wider(id_cols = .data$timestamp,
                  names_from = .data$operation,
                  values_from = count) %>%
      mutate(across(where(is.integer), ~ as.integer(replace_na(.x, 0))))
  } else {
    if (verbose) {
      message("Retrieving diff stats...")
    }
    open_dataset(dataset_prefix$path("diff_stats"), format = "csv") %>% collect()
  }
}
