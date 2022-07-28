fsdr <- arrow::LocalFileSystem$create()$cd("/tmp/history/")

test_that("dir doesn't initially exist",
          {expect_false({dir.exists("/tmp/history/")})})

test_that("dir doesn't initially exist",
          {expect_invisible(
            {make_prefix_path(destination = fsdr)}
            )
            }
          )

test_that("dir doesn't initially exist",
          {expect_true({dir.exists("/tmp/history/")})})


unlink("/tmp/history", recursive = TRUE)
