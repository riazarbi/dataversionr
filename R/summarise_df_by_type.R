#' Title
#'
#' @param df a dataframe
#' @param cols string vector of column names
#' @param sum_funs a named vector of summary functions to apply to the columns
#'
#' @return a dataframe of summary statistics
#'
summarise_df_by_type <- function(df, cols, sum_funs) {
  
  # A little internal function
  rename_varcol <- function(df_list_element){
    colnames(df_list_element[[1]])[2] <- names(df_list_element)
    df_list_element[[1]]
  }
  ##
  
  if(length(cols) == 0) {
    return(NULL)
  } else {
    
    if(is.null(names(sum_funs))) {
      stop("The vector of functions sum_funs must contain named elements.")
    }
    names(cols) <- cols
    
    summary_list <- map(sum_funs, function(x) map_dfc(cols, function(y) x(df[[y]])) )
    summary_list_transposed <- map(summary_list, transpose_summary_df)
    summary_list_named <- map(seq_along(summary_list_transposed), function(x) rename_varcol(summary_list_transposed[x]))  
    summary_list_named %>% reduce(full_join, by = "variable")
  }
}
