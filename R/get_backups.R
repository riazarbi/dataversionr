#' Get backups
#'
#' @param destination a local directory path or an arrow SubTreeFileSystem
#' @param collect should we collect the underlying arrow dataset or return just the connection?
#'
#' @return an arrow dataset
#' @importFrom arrow open_dataset
#' @importFrom dplyr collect
#' @export
#'
#' @examples
get_backups <- function(destination, collect = TRUE) {

  backup_prefix <- fix_path("backup", destination)

  ds <- arrow::open_dataset(backup_prefix)

  if(collect) {
    ds <- dplyr::collect(ds)
  }

  return(ds)
}
