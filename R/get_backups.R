get_backups <- function(destination, collect = TRUE) {

  backup_prefix <- fix_path("backup", destination)

  ds <- arrow::open_dataset(backup_prefix)

  if(collect) {
    ds <- dplyr::collect(ds)
  }

  return(ds)
}
