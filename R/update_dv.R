update_dv <- function(df, destination) {
  destination <- make_SubTreeFileSystem(destination)
  metadata <- get_metadata(destination)
  key_cols <- metadata$key_cols

  if(metadata$diffed) {
    diff <- diffdfs::diffdfs(df, get_latest(destination), key_cols)
    commit_diff(diff, destination)
  }

  if(metadata$backup_count > 0L) {
    put_backup(df, destination)
    delete_backup(destination, metadata$backup_count)
  }

  put_latest(df, destination)

}
