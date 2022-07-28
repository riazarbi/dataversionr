# col_type
test_that("stat: col_type", {
  expect_equal(col_type(iriss$Sepal.Length),
               "numeric")})


# n_missing
test_that("stat: n_missing", {
  expect_equal(n_missing(iriss$Sepal.Length),
               5)})


# n_complete
test_that("stat: n_complete", {
  expect_equal(n_complete(iriss$Sepal.Length),
               145)})


# complete_rate
test_that("stat: complete_rate", {
  expect_equal(complete_rate(iriss$Sepal.Length),
               0.96666667)})


## CHARACTER VECTORS ##
# n_whitespace
test_that("stat: n_whitespace", {
  expect_equal(n_whitespace(iriss$char),
               1)})


# character.min
test_that("stat: min_char", {
  expect_equal(min_char(iriss$char),
               0)})
test_that("stat: min_char NA", {
  expect_equal(min_char(c(NA, NA)),
               NA)})



# character.max
test_that("stat: max_char", {
  expect_equal(max_char(iriss$char),
               2)})
test_that("stat: max_char NA", {
  expect_equal(max_char(c(NA, NA)),
               NA)})


# character.empty
test_that("stat: n_empty_strings", {
  expect_equal(n_empty_strings(iriss$char),
               1)})

test_that("stat: n_empty_strings NA", {
  expect_equal(n_empty_strings(c(NA,NA)),
               NA)})


# character.n_unique
test_that("stat: n_unique", {
  expect_equal(n_unique(iriss$char),
               4)})

# sorted_count
test_that("stat: sorted_count", {
  expect_equal(sorted_count(iriss$Species),
               c(versicolor = 50L, virginica = 50L, setosa = 45L))})



test_that("stat: sorted_count empty factor", {
  expect_warning(iris %>%
                   mutate(Species = ifelse(Species == "versicolor", "", Species)) %>%
                   dplyr::pull(Species) %>%
                   as.factor %>%
                   sorted_count)})


# top_counts
test_that("stat: top_counts", {
  expect_equal(top_counts(iriss$Species, max_levels = 2),
               "vers: 50, virg: 50")})


## NUMERICS ##
# numeric.mean
test_that("stat: mean_stat", {
  expect_equal(mean_stat(iriss$Sepal.Length),
               5.8827586)})



# numeric.sd
test_that("stat: sd_stat", {
  expect_equal(sd_stat(iriss$Sepal.Length),
               0.8128606)})


# numeric.p0
test_that("stat: p0", {
  expect_equal(p0(iriss$Sepal.Length),
               4.4)})

# numeric.p25
test_that("stat: p25", {
  expect_equal(p25(iriss$Sepal.Length),
               5.2)})


# numeric.p50
test_that("stat: p50", {
  expect_equal(p50(iriss$Sepal.Length),
               5.8)})


# numeric.p75
test_that("stat: p75", {
  expect_equal(p75(iriss$Sepal.Length),
               6.4)})


# numeric.p100
test_that("stat: p100", {
  expect_equal(p100(iriss$Sepal.Length),
               7.9)})


## TIME SERIES ##
# ts_start
test_that("stat: ts_start", {
  expect_true(ts_start(iriss$date) == as.POSIXct("2017-02-03"))})


# ts_end
test_that("stat: ts_end", {
  expect_true(ts_end(iriss$date) == as.POSIXct("2017-02-04 17:23:20 SAST"))})
