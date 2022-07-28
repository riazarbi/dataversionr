make_prefix_path <- function(destination) {
  verify_arrow_filesystem(destination)
  if (destination$base_fs$type_name == "local") {
    destination$CreateDir("/", recursive = TRUE)
  }
}
