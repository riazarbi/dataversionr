#' Transpose summary dataframe
#'
#' @param df a summary statistics dataframe
#'
#' @return a neatly transposed dataframe
#' @export
#' @importFrom tibble rownames_to_column
#' @importFrom lubridate now days
#'
#' @examples
transpose_summary_df <- function(df) {
  df %>% 
    t %>% 
    as.data.frame %>% 
    rownames_to_column(var = "variable")
}
