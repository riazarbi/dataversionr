#' Update dv
#'
#' @param df a data frame. Must be structurally identical to the dv.
#' @param destination a local directory path or an arrow SubTreeFileSystem
#'
#' @return TRUE
#' @importFrom diffdfs diffdfs
#' @export
#'
#' @examples
update_dv <- function(df, destination) {
  destination <- make_SubTreeFileSystem(destination)
  metadata <- get_metadata(destination)
  key_cols <- metadata$key_cols

  if (metadata$diffed) {
    diff <- diffdfs::diffdfs(df, get_latest(destination), key_cols)
  }

  if (nrow(diff) > 0) {
    commit_diff(diff, destination)
  } else {
    message("No changes detected. Exiting.")
    return(FALSE)
  }


  if (metadata$backup_count > 0L) {
    put_backup(df, destination)
    delete_backup(destination, metadata$backup_count)
  }

  put_latest(df, destination)

}
