filename <- "file-test.csv"
df <- iris
write.csv(df, filename)

test_that("put prefix",
          {expect_true({
            aws.s3::put_object("file-test.csv",
                               "prefix/file-test.csv",
                               bucket,
                               region = "",
                               base_url = "127.0.0.1:9000",
                               use_https = FALSE)

          })})

# # REMOVE PREFIX ----
prefix <- "prefix/"

test_that("remove prefix",
          {expect_true({
            remove_prefix(prefix, s3dir)
            })})

test_that("verify object doesn't exist",
          {expect_false({

                        aws.s3::object_exists(paste0(prefix, filename),
                                  bucket,
                                  region = "",
                                  base_url = "127.0.0.1:9000",
                                  use_https = FALSE)

          },
          "Client error: (404) Not Found")}
)



test_that("remove root prefix fails",
          {expect_error(
            remove_prefix("", s3dir),
            'Prefix is not allowed to be "". That would wipe the root of the bucket or directory!'
            )}
)

test_that("remove root prefix fails",
          {expect_error(
            remove_prefix("/", s3dir),
            'Prefix is not allowed to be "". That would wipe the root of the bucket or directory!'
          )}
)

test_that("root s3 fs fails fails",
          {expect_error(
            remove_prefix(prefix, s3)
          )}
)


withr::defer(unlink(filename))

