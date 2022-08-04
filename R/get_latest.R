get_latest <- function(destination, collect = TRUE) {

  latest_prefix <- fix_path("latest", destination)


  ds <- arrow::open_dataset(latest_prefix)

  if(collect) {
    ds <- dplyr::collect(ds)
  }

  return(ds)
}
