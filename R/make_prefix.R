make_prefix <- function(destination) {
    destination <- make_SubTreeFileSystem(destination)
  if (destination$base_fs$type_name == "local") {
    destination$CreateDir("/", recursive = TRUE)
  }
}
