#' Delete backup
#'
#' @param destination a local directory path or an arrow SubTreeFileSystem
#' @param after_position how many backups should we leave in the dv?
#' @param verbose TRUE /FALSE should the function be chatty?
#'
#' @return TRUE
#' @importFrom purrr map_chr
#' @importFrom tools file_path_sans_ext
#' @export
#'
#' @examples
delete_backup <-
  function(destination, after_position, verbose = FALSE) {
    if (!(is.integer(after_position) & after_position > 0L)) {
      stop("position parameter must be an integer larger than 0.")
    }

    prefix_path <- fix_path("backup", destination)
    prefix_path <-
      make_SubTreeFileSystem(prefix_path, verbose = verbose)

    backup_files <- prefix_path$ls()
    ordered_backups <-
      sort(purrr::map_chr(backup_files, ~ tools::file_path_sans_ext(.x)),
           decreasing = TRUE)
    num_ordered_backups <- length(ordered_backups)
    start_flag <- after_position + 1
    if (start_flag > num_ordered_backups) {
      return(TRUE)
    } else {
      backups_flagged <-
        ordered_backups[start_flag:length(ordered_backups)]
      for (bak in backups_flagged) {
        prefix_path$DeleteFile(paste0(bak, ".parquet"))
      }
      return(TRUE)
    }
  }
