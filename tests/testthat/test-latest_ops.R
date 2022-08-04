# setup ------------------------------------------------------------------------
local_prefix <- tempfile()

# tests ------------------------------------------------------------------------
test_that("local get latest",
          {expect_error({
            get_latest(local_prefix)
          }, "No such file or directory")})

test_that("local put latest",
          {expect_true({
            put_latest(new_df, local_prefix)
          })})

test_that("local get latest",
          {expect_equal({
            get_latest(local_prefix)
          }, new_df)})

test_that("s3 get latest",
          {expect_error({
            get_latest(s3dir)
          }, "No such file or directory")})


test_that("s3 put latest",
          {expect_true({
            put_latest(new_df, s3dir)
          })})

test_that("s3 get latest",
          {expect_equal({
            get_latest(s3dir)
          }, new_df)})

# teardown ---------------------------------------------------------------------
withr::defer({
  unlink(local_prefix);
  s3$DeleteDirContents("dataversionr-tests/")
})
