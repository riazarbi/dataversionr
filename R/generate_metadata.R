generate_metadata <- function(df,
                              destination,
                              key_cols = NA,
                              diffed = TRUE,
                              backup_count = 0L
                              ) {
  # versioning
  schema_ver = "20220727"

  # tests
  destination <- make_SubTreeFileSystem(destination)

  if(!is.data.frame(df)) {
    stop("parameter df is not a dataframe.")
  }

  if(all(is.na(key_cols))) {
    key_cols <- colnames(df)
  }

  if(!(all(key_cols %in% colnames(df)))) {
    stop("key_cols are not present in colnames(df)")
  }

  if(!(is.integer(backup_count))) {
    stop("parameter backup_count must be an integer. You can set it to 1L to just keep the latest version.")
  }

  if(!(is.logical(diffed))) {
    stop("parameter diffed must be TRUE/FALSE.")
  }

  if(diffed) {
    message("Checking that new_df can be diffed...")
    if(is.data.frame(diffdfs::diffdfs(df, key_cols = key_cols))){
      message("Diff test passed.")
    } else {
      stop("Diff test failed. Make sure your dataframe can be diffed or set diffed = FALSE")
    }
  }

  # metadata builder
  base_path = destination$base_path

  metadata <- list()
  metadata$schema_ver = schema_ver
  metadata$base_path = base_path
  metadata$key_cols = sort(key_cols)
  metadata$diffed = diffed
  metadata$backup_count = backup_count
  return(metadata)
}
