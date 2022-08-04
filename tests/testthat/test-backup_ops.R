# setup ------------------------------------------------------------------------
# set up
local_prefix <- tempfile()

new_df <- data.frame(a = 2:6, b = letters[2:6])
new_df[3, 2] <- "g"

# tests ------------------------------------------------------------------------
# get and put
test_that("local get backup",
          {expect_error({
            get_backups(local_prefix)
          }, "No such file or directory")})

test_that("local put backup",
          {expect_true({
            put_backup(new_df, local_prefix)
          })})

test_that("local get backup",
          {expect_equal({
            get_backups(local_prefix) %>% select(-backup_timestamp)
          }, new_df)})


test_that("s3 get backup",
          {expect_error({
            get_backups(s3dir)
          }, "No such file or directory")})


test_that("s3 put backup",
          {expect_true({
            put_backup(new_df, s3dir)
          })})

test_that("s3 get backup",
          {expect_equal({
            get_backups(s3dir) %>% select(-backup_timestamp)
          }, new_df)})

# delete backups s3
for(i in seq_along(1:10)) {put_backup(new_df, s3dir)}

prior_backups <- s3dir$cd("backup")$ls()

test_that("s3 delete backup",
          {expect_true({
            delete_backup(s3dir, after_position = 4L)
          })})

s3_post_backups <- s3dir$cd("backup")$ls()

test_that("s3 delete backup verify",
          {expect_equal({sort(prior_backups, decreasing = TRUE)[1:4]},
                        sort(s3_post_backups, decreasing = TRUE))})


# delete backups local
for(i in seq_along(1:10)) {put_backup(new_df, local_prefix)}

prefix_path <- fix_path("backup", local_prefix)
prefix_path <- make_SubTreeFileSystem(prefix_path)

prior_backups <- prefix_path$ls()

test_that("local_prefix delete backup",
          {expect_true({
            delete_backup(local_prefix, after_position = 4L)
          })})

local_post_backups <- prefix_path$ls()

test_that("local_prefix delete backup verify",
          {expect_equal({sort(prior_backups, decreasing = TRUE)[1:4]},
                        sort(local_post_backups, decreasing = TRUE))})


# retrieve_backup
# let some time elapse
Sys.sleep(60)

# retrieve_backup s3
old_backups <- s3dir$cd("backup")$ls()
correct_backup_ts <- lubridate::as_datetime(as.numeric(tools::file_path_sans_ext(sort(old_backups, decreasing = TRUE))))[1]

for(i in seq_along(1:10)) {put_backup(new_df, s3dir)}

test_that("s3 retrieve_dv_backup",
          {expect_equal({
            retrieve_dv_backup(s3dir, as_of = lubridate::now() - seconds(30)) %>% pull(backup_timestamp) %>% sample(1)},
            correct_backup_ts)})

test_that("retrieve_dv_backup too far back",
          {expect_error({
            retrieve_dv_backup(s3dir, as_of = lubridate::now() - hours(30)) %>% pull(backup_timestamp) %>% sample(1)},
            "No backups older")})


# retrieve_backup local
old_backups <- prefix_path$ls()
correct_backup_ts <- lubridate::as_datetime(as.numeric(tools::file_path_sans_ext(sort(old_backups, decreasing = TRUE))))[1]

for(i in seq_along(1:10)) {put_backup(new_df, local_prefix)}

test_that("local retrieve_dv_backup",
          {expect_equal({
            retrieve_dv_backup(local_prefix, as_of = lubridate::now() - seconds(30)) %>% pull(backup_timestamp) %>% sample(1)},
            correct_backup_ts)})


# teardown ---------------------------------------------------------------------
withr::defer({
  unlink(local_prefix);
  s3$DeleteDirContents("dataversionr-tests/")
})


