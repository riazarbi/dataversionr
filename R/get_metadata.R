

get_metadata <- function(destination, verbose = FALSE) {
  get_path <- fix_path("metadata.csv", destination, verbose = verbose)
  df <- arrow::read_csv_arrow(get_path,
                       col_types = "cccli",
                       col_names = c("schema_ver",
                                     "base_path",
                                     "key_cols",
                                     "diffed",
                                     "backup_count"),
                       skip = 1)
  metadata_concat <- as.list(df)
  metadata_map <- purrr::map(metadata_concat, ~ ifelse(is.character(.x), strsplit(.x, split = "[|]"), .x))
  metadata <- purrr::map(metadata_map, ~unlist(.x))
  return(metadata)
}
