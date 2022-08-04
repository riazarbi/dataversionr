get_diff_stats <- function(destination) {

  destination <- make_SubTreeFileSystem(destination)

  get_location <- fix_path("diff_stats.csv", destination)

  as.data.frame(arrow::read_csv_arrow(get_location))

}
