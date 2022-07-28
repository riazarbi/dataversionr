test_that("check_vec_class works", {
  expect_invisible(check_vec_class(iris$Species, is.factor))
})

test_that("check_vec_class fails", {
  expect_error(check_vec_class(iris$Species, is.numeric))
})
