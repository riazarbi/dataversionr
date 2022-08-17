#' Get latest
#'
#' @param destination a local directory path or an arrow SubTreeFileSystem
#' @param collect should we return a data frame (TRUE) or an arrow dataset connection (FALSE)
#'
#' @return a data frame or an arrow dataset
#' @importFrom arrow open_dataset
#' @importFrom dplyr collect
#' @export
#'
#' @examples
get_latest <- function(destination, collect = TRUE) {
  latest_prefix <- fix_path("latest", destination)


  ds <- arrow::open_dataset(latest_prefix)

  if (collect) {
    ds <- dplyr::collect(ds)
  }

  return(ds)
}
