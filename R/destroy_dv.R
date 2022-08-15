

destroy_dv <- function(destination, prompt = TRUE) {
  destination <- make_SubTreeFileSystem(destination)
  metadata <- get_metadata(destination)
  metadata_check <- is.list(metadata) && length(metadata) == 5
  if(metadata_check) {
    remove_prefix(destination, prompt = prompt)
  } else {
    stop("metadata check failed. Are you sure this is a dv? You'll have to manually destroy it via another method.")
  }
}
