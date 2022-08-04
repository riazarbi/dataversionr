get_latest <- function(destination, collect = TRUE) {

  latest_prefix <- fix_path("latest", destination)


  ds <- open_dataset(latest_prefix)

  if(collect) {
    ds <- collect(ds)
  }

  return(ds)
}
