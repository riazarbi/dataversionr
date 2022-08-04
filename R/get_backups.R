get_backups <- function(destination, collect = TRUE) {

  backup_prefix <- fix_path("backup", destination)

  ds <- open_dataset(backup_prefix)

  if(collect) {
    ds <- collect(ds)
  }

  return(ds)
}
