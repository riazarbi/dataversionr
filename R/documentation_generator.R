#' Documentation generator
#'
#' @param df a dataframe to create documentation for
#' @param path string. a file path to save the documentation json file at.
#'
#' @return TRUE
#' @export
#' @importFrom jsonlite fromJSON toJSON
#' @import purrr
#'
#' @examples
#'\dontrun{
#' documentation_generator(mtcars, "mtdocumentation.json")
#'}
documentation_generator <- function(df, path) {
  data <-
    system.file("extdata", "template_documentation.json", package = "dataversionr")
  template <- fromJSON(data)
  var_names <- colnames(df)
  num_vars <- length(var_names)
  var_types <- unlist(map(df, function(x) paste(class(x), collapse = " ")))
  template$schema$fields <- map_dfr(seq_len(num_vars), ~template$schema$fields)
  template$schema$fields$name <- var_names
  template$schema$fields$type <- var_types
  generated_documentation <- toJSON(template, pretty = TRUE)
  write(generated_documentation, path)
  return(TRUE)
}
