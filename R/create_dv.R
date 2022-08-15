

create_dv <- function(df, destination, key_cols = NA, diffed = TRUE, backup_count = 0L) {

  destination <- make_SubTreeFileSystem(destination)
  make_prefix(destination)
  if(length(destination$ls()) !=0) {
    stop("destination is not empty.")
  }

  if(all(is.na(key_cols))) {
    key_cols <- colnames(df)
  }

  metadata <- generate_metadata(df,
   destination,
   key_cols = key_cols,
   diffed = diffed,
   backup_count = backup_count
  )

  put_metadata(metadata, destination)

  if(diffed) {
    diff <- diffdfs::diffdfs(df, NA, key_cols = key_cols)
    commit_diff(diff, destination)
  }

  put_latest(df, destination)

  if(backup_count > 0L) {
    put_backup(df, destination)
  }

  }
