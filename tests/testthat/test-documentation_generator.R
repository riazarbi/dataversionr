test_that("generator works", {
  expect_true(documentation_generator(iris, "/tmp/tmp.json"))
})

test_that("generator file size is as expected", {
  expect_true(file.info( "/tmp/tmp.json")$size == 2256)
})

