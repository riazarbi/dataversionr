#' Check key cols unique
#'
#' @param df a dataframe
#' @param key_cols vector of column names
#' @param verbose TRUE/FALSE should we print a message?
#'
#' @return TRUE if key cols have unique rows; FALSE if not
#' @export
#' @importFrom dplyr distinct
#'
#' @examples
#' irisint = iris
#' irisint$rownum = 1:nrow(irisint)
#' key_cols = c("rownum")
#' check_key_cols_unique(irisint, key_cols, TRUE)
#' check_key_cols_unique(irisint, "Species", TRUE)
check_key_cols_unique <- function(df, key_cols, verbose = FALSE) {
  if(verbose) {
    message("Checking that key column rows are unique")
  }
  if(nrow(distinct(.data = df, across(all_of(key_cols)))) != nrow(df))  {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

