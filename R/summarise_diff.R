summarise_diff <- function(destination) {

  destination <- make_SubTreeFileSystem(destination)

  diff_counts <-  destination %>%
    get_diffs(collect = FALSE) %>%
    group_by(diff_timestamp, operation) %>%
    summarise(count = n()) %>%
    collect()

  diff_stats <- diff_counts %>%
    tidyr::pivot_wider(names_from = operation, values_from = count) %>%
    dplyr::mutate() %>% ungroup() %>% as.data.frame()

  if(!"new" %in% names(diff_stats)) {
    diff_stats$new <- 0L
  }

  if(!"deleted" %in% names(diff_stats)) {
    diff_stats$deleted <- 0L
  }

  if(!"modified" %in% names(diff_stats)) {
    diff_stats$modified <- 0L
  }

  return(diff_stats)

}

