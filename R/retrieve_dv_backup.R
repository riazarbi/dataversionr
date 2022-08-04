

retrieve_dv_backup <- function(destination, as_of) {
  if(is.na(as_of)) {
    as_of <- lubridate::now()
  }

  as_of <- lubridate::with_tz(as_of, tzone = "UTC")

  if(!("POSIXct" %in% class(as_of))) {
    stop("parameter as_of must be of class POSIXct. An easy way to create such an object is with lubridate::as_datetime()")
  }

  backup_prefix <- fix_path("backup", destination)
  backup_prefix <- make_SubTreeFileSystem(backup_prefix)

  backup_files <- backup_prefix$ls()

  if(length(backup_files) == 0) {
    stop("No backups found in the backup directory. ")
  }

  ordered_backups <- sort(map_chr(backup_files, ~tools::file_path_sans_ext(.x)), decreasing = TRUE)

  ordered_backups_dttm <- lubridate::as_datetime(as.numeric(ordered_backups))

  older_backups <- ordered_backups[ordered_backups_dttm <= as_of]

  if(length(older_backups) == 0) {
    stop("No backups older than the specified as_of date found.")
  }

  latest_backup_name <- paste0(older_backups[1], ".parquet")

  get_path <- fix_path(latest_backup_name, backup_prefix)

  dv <- read_parquet(get_path)

  return(dv)

  }
