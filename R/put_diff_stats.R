put_diff_stats <- function(destination) {

  destination <- make_SubTreeFileSystem(destination)

  diff_counts_df <- summarise_diff(destination)

  put_location <- fix_path("diff_stats.csv", destination)

  arrow::write_csv_arrow(diff_counts_df, put_location)
  return(TRUE)
}
