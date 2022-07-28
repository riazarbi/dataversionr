#' Title
#'
#' @param x a vector
#' @param required_class a function checking the class, eg. is.factor 
#' @importFrom lubridate is.POSIXt
#'
#'
#' @examples
#'\dontrun{
#' check_vec_class(iris$Species, is.factor)
#' }
check_vec_class <- function(x, required_class) {
  if(!(all(map_lgl(x, required_class)))) {
    stop(paste("The summary statistics column class mapping is failing.
      Hint: make sure all the columns you are evaluating are of the required type."))
  }
}

