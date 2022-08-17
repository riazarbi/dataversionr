#' Put diff stats
#'
#' @param destination a local directory path or an arrow SubTreeFileSystem
#'
#' @return TRUE
#' @importFrom arrow write_csv_arrow
#' @export
#'
#' @examples
put_diff_stats <- function(destination) {
  destination <- make_SubTreeFileSystem(destination)

  diff_counts_df <- summarise_diffs(destination)

  put_location <- fix_path("diff_stats.csv", destination)

  arrow::write_csv_arrow(diff_counts_df, put_location)
  return(TRUE)
}
