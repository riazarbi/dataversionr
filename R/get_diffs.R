get_diffs <- function(destination, collect = TRUE) {
  destination <- make_SubTreeFileSystem(destination)
  ds <- open_dataset(destination$path("diff"))
  if(collect) {
    collect(ds)
  } else {
    ds
  }
}
