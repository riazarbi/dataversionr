put_backup <- function(new_df, destination) {

  latest_prefix <- fix_path("backup", destination)

  make_prefix(latest_prefix)

  timestamp <- lubridate::now(tzone = "UTC")

  file_path <- paste0(as.numeric(timestamp), ".parquet")

  put_location <- fix_path(file_path, latest_prefix)

  backup_df <- dplyr::mutate(new_df, backup_timestamp = timestamp)

  arrow::write_parquet(backup_df,
                put_location)
  return(TRUE)
}
