
verify_arrow_filesystem <- function(destination, verbose = FALSE) {
  if(!(all(class(destination) == c("SubTreeFileSystem","FileSystem","ArrowObject","R6")))) {
    stop("destination parameter is not an arrow filesystem object.")
  } else if (verbose) {
    message("destination parameter is an arrow filesystem object.")
  }
}
