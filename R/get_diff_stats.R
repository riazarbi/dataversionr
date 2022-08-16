#' Get diff stats
#'
#' @param destination a local directory path or an arrow SubTreeFileSystem
#'
#' @return a data frame
#' @importFrom arrow read_csv_arrow
#' @export
#'
#' @examples
get_diff_stats <- function(destination) {

  destination <- make_SubTreeFileSystem(destination)

  get_location <- fix_path("diff_stats.csv", destination)

  as.data.frame(arrow::read_csv_arrow(get_location))

}
