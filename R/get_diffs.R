get_diffs <- function(destination, collect = TRUE) {
  destination <- make_SubTreeFileSystem(destination)
  ds <- arrow::open_dataset(destination$path("diff"))
  if(collect) {
    dplyr::collect(ds)
  } else {
    ds
  }
}
