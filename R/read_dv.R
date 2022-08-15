

read_dv <- function(destination,
                    as_of = NA,
                    source = "latest") {

    destination <- make_SubTreeFileSystem(destination)

    if(length(destination$ls()) == 0) {
    stop("destination is empty.")
    }

    meta <- get_metadata(destination)
    key_cols <- meta$key_cols

    if(source == "latest") {
      if(!is.na(as_of)) {
        stop("parameter latest only works with parameter as_of set to NA.")
      }
      get_latest(destination)
    } else if(source == "diffs") {
      read_dv_diff(destination,
                       as_of,
                       key_cols)
    } else if(source == "backup") {
      read_dv_backup(destination,
                     as_of)
    } else {
      stop(paste("source parameter should be one of latest, diffs, or backup."))
    }



}
