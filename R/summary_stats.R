## ALL COLS ##
# type
col_type <- function(x) {
  class(x)[1]
}


# n_missing
n_missing <- function(x) {
  sum(is.na(x) | is.null(x))
}


# n_complete
n_complete <- function(x) {
  sum(!is.na(x) & !is.null(x))
}


# complete_rate
complete_rate <- function(x) {
  1 - n_missing(x) / length(x)
}


## CHARACTER VECTORS ##
# n_whitespace
n_whitespace <- function(x) {
  check_vec_class(x, is.character)
  whitespace <- grepl("^\\s+$", x)
  sum(whitespace)
}


# character.min
min_char <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  }
  check_vec_class(x, is.character)
  characters <- nchar(x, allowNA = TRUE)
  min(characters, na.rm = TRUE)
}


# character.max
max_char <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  }
  check_vec_class(x, is.character)
  characters <- nchar(x, allowNA = TRUE)
  max(characters, na.rm = TRUE)
}


# character.empty
n_empty_strings <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  }
  check_vec_class(x, is.character)
  empty_strings <- x == ""
  sum(empty_strings, na.rm = TRUE)
}


# character.n_unique
n_unique <- function(x) {
  check_vec_class(x, is.character)
  un <- x[!is.na(x)]
  un <- unique(un)
  length(un)
}



#' Sorted count
#'
#' @param x a vector
#'
#' @return a string containing the counts of the 3 most numerous factor levels
#' @importFrom rlang set_names
#' @export
#'
#' @examples
#' sorted_count(iris$Species)
sorted_count <- function(x) {
  check_vec_class(x, is.factor)
  tab <- table(x, useNA = "no")
  names_tab <- names(tab)
  if (is.element("", names_tab)) {
    names_tab[names_tab == ""] <- "empty"
    warning(
      "Variable contains value(s) of \"\" that have been ",
      "converted to \"empty\"."
    )
  }
  out <- set_names(as.integer(tab), names_tab)
  sort(out, decreasing = TRUE)
}


# factor.top_counts
#' Title
#'
#' @param x a factor vector
#' @param max_char integer. number of characters to truncate the factors to
#' @param max_levels integer. the max number of levels to display
#'
#' @return a string with the shortened names of the top factors and the count of their occurrences.
#' @export
#'
#' @examples
#' top_counts(iris$Species, 4, 3)
top_counts <- function(x, max_char = 4, max_levels = 3) {
  check_vec_class(x, is.factor)

  counts <- sorted_count(x)
  if (length(counts) > max_levels) {
    top <- counts[seq_len(max_levels)]
  } else {
    top <- counts
  }
  top_names <- substr(names(top), 1, max_char)
  paste0(top_names, ": ", top, collapse = ", ")
}




## NUMERICS ##
# numeric.mean
mean_stat <- function(x) {
  check_vec_class(x, is.numeric)

  mean(x, na.rm = TRUE)
}


# numeric.sd
sd_stat <- function(x) {
  check_vec_class(x, is.numeric)

  stats::sd(x, na.rm = TRUE)
}


# numeric.p0
p0 <- function(x) {
  check_vec_class(x, is.numeric)

  stats::quantile(x, probs = 0, na.rm = TRUE, names = FALSE)
}


# numeric.p25
p25 <- function(x) {
  check_vec_class(x, is.numeric)

  stats::quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
}


# numeric.p50
p50 <- function(x) {
  check_vec_class(x, is.numeric)

  stats::quantile(x, probs = 0.5, na.rm = TRUE, names = FALSE)
}


# numeric.p75
p75 <- function(x) {
  check_vec_class(x, is.numeric)

  stats::quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE)
}


# numeric.p100
p100 <- function(x) {
  check_vec_class(x, is.numeric)

  stats::quantile(x, probs = 1, na.rm = TRUE, names = FALSE)
}


## TIME SERIES ##
# ts_start
ts_start <- function(x) {
  check_vec_class(x, is.POSIXt)
  min(x, na.rm = TRUE)
}


# ts_end
ts_end <- function(x) {
  check_vec_class(x, is.POSIXt)
  max(x, na.rm = TRUE)
}
