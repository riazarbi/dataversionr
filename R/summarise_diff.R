summarise_diff <- function(destination) {

  destination <- make_SubTreeFileSystem(destination)

  diff_counts <-  destination %>%
    get_diffs(collect = FALSE) %>%
    dplyr::group_by(.data$diff_timestamp, .data$operation) %>%
    dplyr::collect() %>%
    dplyr::summarise(count = dplyr::n())


  diff_stats <- diff_counts %>%
    tidyr::pivot_wider(names_from = .data$operation, values_from = .data$count) %>%
    dplyr::mutate() %>% dplyr::ungroup() %>% as.data.frame()

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

