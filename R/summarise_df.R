#' Summarise a dataframe
#'
#' @param df a dataframe
#'
#' @return a dataframe with salient summarty statistics
#' @export
#' @importFrom dplyr select full_join mutate case_when
#'
#' @examples
#' summarise_df(iris)
summarise_df <- function(df) {

  # Define column groups
  cols <- df %>% colnames
  names(cols) <- cols

  cols_char <- df %>%
    dplyr::select(where(is.character)) %>%
    colnames
  names(cols_char) <- cols_char

  cols_num <- df %>%
    dplyr::select(where(is.numeric)) %>%
    colnames
  names(cols_num) <- cols_num

  cols_fact <- df %>%
    dplyr::select(where(is.factor)) %>%
    colnames
  names(cols_fact) <- cols_fact

  cols_dttm <- df %>%
    dplyr::select(where(~ "POSIXt" %in% class(.x))) %>%
    colnames
  names(cols_dttm) <- cols_dttm


  # Create the column type summary group dfs
  # Numerics
  sum_funs <- c(mean = mean_stat, sd = sd_stat, p0 = p0, p25 = p25, p50 = p50, p75 = p75, p100 = p100)
  summary_num <- summarise_df_by_type(df, cols_num, sum_funs)

  # Datetimes
  sum_funs <- c(ts_end  = ts_end, ts_start = ts_start)
  summary_dttm <- summarise_df_by_type(df, cols_dttm, sum_funs)

  # Characters
  sum_funs <- c(min_char = min_char, max_char = max_char, n_whitespace = n_whitespace, n_empty_strings = n_empty_strings, n_unique = n_unique)
  summary_char <- summarise_df_by_type(df, cols_char, sum_funs)

  # Factors
  sum_funs <- c(is_ordered = is.ordered, nlevels = nlevels, top_counts = top_counts)
  summary_fact <- summarise_df_by_type(df, cols_fact, sum_funs)

  # All
  sum_funs <- c(col_type = col_type, n_missing = n_missing, complete_rate = complete_rate)
  summary_all <- summarise_df_by_type(df, cols, sum_funs)

  list(summary_all, summary_num, summary_dttm, summary_fact, summary_char) %>%
    compact %>%
    reduce(dplyr::full_join, by = "variable") %>%
    # make col_type non-R specific
    dplyr::mutate(col_type = case_when(col_type == "factor" ~ "categorical",
           TRUE ~ col_type))
}
