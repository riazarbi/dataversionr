#' Make prefix
#'
#' @param destination a local directory path or an arrow SubTreeFileSystem
#'
#' @return silent
#' @export
#'
#' @examples
make_prefix <- function(destination) {
  destination <- make_SubTreeFileSystem(destination)
  if (destination$base_fs$type_name == "local") {
    destination$CreateDir("/", recursive = TRUE)
  }
}
