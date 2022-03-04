make_prefix_path <- function(prefix, 
                              destination) {
  if (destination$base_fs$type_name == "local") {
    fsdr$CreateDir(prefix, recursive = TRUE)
  }
}
