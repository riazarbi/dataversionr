test_that("verify arrow filesystem", {
  expect_invisible({verify_arrow_filesystem(arrow::LocalFileSystem$create()$cd("/tmp"))})
})

test_that("verify arrow filesystem", {
  expect_message({verify_arrow_filesystem(arrow::LocalFileSystem$create()$cd("/tmp"), verbose = TRUE)},
                 "destination parameter is an arrow filesystem object.")
})


test_that("verify arrow filesystem", {
  expect_error({verify_arrow_filesystem("/tmp")},
               "destination parameter is not an arrow filesystem object.")
})
