#' Get diffs
#'
#' @param destination a local directory path or an arrow SubTreeFileSystem
#' @param collect should we collect the underlying arrow dataset or return just the connection?
#'
#' @return a data frame or an arrow dataset connection
#' @importFrom arrow open_dataset
#' @importFrom dplyr collect
#' @export
#'
#' @examples
get_diffs <- function(destination, collect = TRUE) {
  destination <- make_SubTreeFileSystem(destination)
  ds <- arrow::open_dataset(destination$path("diff"))
  if(collect) {
    dplyr::collect(ds)
  } else {
    ds
  }
}
