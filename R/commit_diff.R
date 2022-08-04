commit_diff <- function(diff_df,
                        destination,
                        verbose = FALSE) {

  destination <- make_SubTreeFileSystem(destination)

  timestamp <- lubridate::now(tzone = "UTC")

  diff_path <- paste0("diff/", as.numeric(timestamp), ".parquet")

  diff_df <- diff_df %>%
    dplyr::mutate(diff_timestamp = timestamp) %>%
    dplyr::select(diff_timestamp, everything())


  if (verbose) {
    message("Committing diff to dataset...")
  }
  if ("LocalFileSystem" %in% class(destination$base_fs)) {
    make_prefix(destination$base_fs$cd(paste0(destination$base_path, "diff")))
  }

  put_location <- fix_path(diff_path, destination)

  write_parquet(diff_df,
                put_location)

  if (verbose) {
    message("Verifying diff can be retrieved from dataset...")
  }

  retrieved_df <- read_parquet(put_location)

  if (verbose) {
    message("A parquet file can be read from the target path...")
  }

  # Table$() method is WORKAROUND FOR https://issues.apache.org/jira/browse/ARROW-16010
  read_check <-
    all(retrieved_df == Table$create(diff_df)$to_data_frame())
  if (verbose & read_check) {
    message("Remote diff is identical to local diff.")
  }

  if (!read_check) {
    stop(
      "Remote diff is not identical to local diff. \n
             You should make sure the dataset hasn't been corrupted."
    )
  }
  return(TRUE)
}