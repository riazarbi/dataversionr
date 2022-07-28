# Create sample dataframe
numbers <- 1:6
letters2 <- c("s", "j", "f", "u", "l", "i")

new_df <- data.frame(a = numbers[2:6], b = letters2[2:6])
new_df[3, 2] <- "update"
old_df <- data.frame(a = numbers[1:5], b = letters2[1:5])

# Define a unique prefix for next few tests
prefix <- paste0(round(as.numeric(now()) * 10000), 0)

test_that(
 "update_dataset: Expect non existent bucket fails",
 {expect_error(
   update_dataset(new_df = old_df,
                  prefix = prefix,
                  destination = s3$cd("nope"),
                  key_cols = "a")
  ,
   "The specified bucket does not exist"
 )}
)


test_that(
  "update_dataset: Expect non existent bucket fails VERBOSE",
  {expect_error(expect_warning(
    update_dataset(new_df = old_df,
                   prefix = prefix,
                   destination = s3$cd("nope"),
                   key_cols = "a",
                   verbose = TRUE))
  ,
  "The specified bucket does not exist"
  )}
)


test_that(
  "update_dataset: Check initial minio push gives warning",
  {expect_warning(
    update_dataset(old_df,
                   prefix,
                   destination = s3dir,
                   key_cols = "a",
                   verbose = TRUE),
    "NO CACHED DATASET FOUND"
  )}
)


# Check NA diff handling
na_df <- old_df
na_df[1,2] <- NA

update_dataset(na_df,
              prefix,
              destination = s3dir,
              key_cols = "a",
              verbose = TRUE)


test_that("compute_df_diff_tables: Check diff when comparing two identical dfs WITH AN NA ENTRY",
          {
            expect_equal(
              compute_df_diff_tables(retrieve_dataset(prefix,
                                                      destination = s3dir),
                                     na_df,
                                     key_cols = "a"),
              list(
                new_rows = data.frame(a = as.integer(character()), b = character()),
                modified_rows = data.frame(a = as.integer(character()), b = character()),
                deleted_rows = data.frame(a = as.integer(character()), b = character())
              )
            )
          })


# Check datetime diff handling
prefix <- paste0(bucket, "/", round(as.numeric(now()) * 10000), 0)

n <-  1631494810.376999855041503906250000000000000000000000000000000000

df <- data.frame(x = "a",
                 n = n,
                 t = as.POSIXct(n, origin = "1970-01-01"))

suppressWarnings(update_dataset(df,
                                prefix,
                                destination = fsdir,
                                verbose = TRUE))

test_that("update_dataset: no key cols used",
          {expect_message(
            update_dataset(df,
                                 prefix,
                           destination = fsdir,
                                 verbose = TRUE),
            "No new data detected.")}
)
#
#
#
# # Test key_col behaviour
# prefix <- paste0(bucket, "/", round(as.numeric(now()) * 10000), 0)
#
# test_that("update_dataset: no key cols used",
#           {expect_true(
#             update_dataset(new_df,
#                                  prefix,
#                                  verbose = FALSE,
#                                  warn_documentation = FALSE)
#           )}
#
# )
#
# test_that("update_dataset: inconsistent key cols used",
#           {expect_error(
#             update_dataset(old_df,
#                                  prefix,
#                                  key_cols = ("a"),
#                                  verbose = FALSE,
#                                  warn_documentation = FALSE),
#             "Key_col validation failed"
#           )})
#
#
# prefix <- paste0(bucket, "/", round(as.numeric(now()) * 10000), 0)
#
# test_that(
#   "update_dataset: Check initial minio push success VERBOSE",
#   {expect_warning(
#     update_dataset(old_df, prefix, verbose = TRUE, warn_documentation = FALSE),
#     "NO CACHED DATASET FOUND"
#   )
#   }
# )
#
# old_df_checkpoint <- now()
#
# test_that(
#   "update_dataset: Check minio update success VERBOSE",
#   {expect_message(
#     update_dataset(new_df, prefix, verbose = TRUE, warn_documentation = FALSE),
#     "Writing new dataset"
#   )
#   }
# )
#
# prefix <- paste0(bucket, "/", round(as.numeric(now()) * 10000), 0)
#
# test_that(
#   "update_dataset: Check initial minio push success",
#   {expect_true(
#     update_dataset(old_df, prefix, key_cols = "a", verbose = FALSE,
#                          warn_documentation = FALSE,
#                          summarise = TRUE)
#   )}
# )
#
# old_df_checkpoint <- now()
#
# test_that(
#   "update_dataset: Check minio update success",
#   {expect_true(
#     update_dataset(new_df,
#                          prefix,
#                          key_cols = "a",
#                          verbose = FALSE,
#                          warn_documentation = FALSE)
#   )}
# )
#
# test_that(
#   "update_dataset: Check message when no update required",
#   {expect_message(
#     update_dataset(new_df,
#                          prefix,
#                          key_cols = "a",
#                          verbose = FALSE,
#                          warn_documentation = FALSE),
#     "No new data detected"
#   )}
# )
#
# test_that(
#   "update_dataset: Check error when inconsistent key_col used",
#   {expect_error(
#     update_dataset(old_df,
#                          prefix,
#                          key_cols = c("a", "b"),
#                          warn_documentation = FALSE),
#     "Key_col validation failed"
#   )}
# )
#
#
# # Retrieve dataset diff stats
# test_that(
#   "retrieve_dataset_diff_stats: Compute from first principles",
#   {expect_equal(
#     retrieve_dataset_diff_stats(prefix,
#                                 first_principles = TRUE) %>% select(-timestamp),
#     as_tibble(data.frame(
#       create = c(5, 1),
#       delete = c(0, 1),
#       update = c(0, 1)
#     ))
#   )}
# )
#
# test_that(
#   "retrieve_dataset_diff_stats: Compute from first principles VERBOSE",
#   {expect_message(
#     retrieve_dataset_diff_stats(prefix, first_principles = TRUE, verbose = TRUE),
#     "Computing diff stats from first principles"
#   )}
# )
#
# test_that(
#   "retrieve_dataset_diff_stats: Read pre computed stats VERBOSE",
#   {expect_equal(
#     retrieve_dataset_diff_stats(prefix, verbose = TRUE) %>% arrange(timestamp) %>% select(-timestamp),
#     as_tibble(data.frame(
#       create = c(5L, 1L),
#       update = c(0L, 1L),
#       delete = c(0L, 1L),
#       key_cols = "a",
#       minio_user = MINIO_KEY
#     ))
#   )}
# )
#
#
# test_that(
#   "retrieve_dataset_diff_stats: Read pre computed stats VERBOSE",
#   {expect_message(
#     retrieve_dataset_diff_stats(prefix, verbose = TRUE),
#     "Retrieving diff stats"
#   )}
# )
#
#
# # Retrieve dataset
# test_that(
#   "retrieve_dataset: no timestamp set",
#   {expect_equal(
#     retrieve_dataset(
#       prefix,
#       verbose = T
#     ),
#     as_tibble(new_df)
#   )})
#
# test_that(
#   "retrieve_dataset: Verify now() versioned timestamp returns same as latest",
#   {expect_equal(
#     retrieve_dataset(
#       prefix,
#       timestamp = now(),
#       verbose = T
#     ),
#     retrieve_dataset(prefix,  verbose = T)
#   )
#   }
# )
#
#
# test_that(
#   "retrieve_dataset: Verify versioned timestamp returns time-sensitive dataset",
#   {expect_equal(
#     retrieve_dataset(
#       prefix,
#       timestamp = old_df_checkpoint,
#       verbose = T
#     ),
#     as_tibble(old_df)
#   )}
# )
#
#
# expected_diffs <- as_tibble(data.frame(
#   a = c(4L, 1L, 5L, 3L, 2L, 6L, 1L, 4L),
#   b = c("u", "s", "l", "f", "j","i", "s", "update"),
#   operation = c("create", "create", "create", "create", "create", "create", "delete", "update")
# ))
#
# # Retrieve dataset diffs
# test_that(
#   "retrieve_dataset_diffs: Simple case",
#   {expect_true(
#     fsetequal(
#       data.table(expected_diffs),
#       data.table(retrieve_dataset_diffs(prefix) %>% select(-timestamp)))
#   )}
# )
#
# test_that(
#   "retrieve_dataset_diffs: Simple case VERBOSE",
#   {expect_message(
#     retrieve_dataset_diffs(prefix, verbose = T) %>% select(-timestamp),
#     "No timestamp set. Retrieving latest copy"
#   )}
# )
#
# expected_diffs <- as_tibble(data.frame(
#   a = c(4L, 1L, 5L, 3L, 2L),
#   b = c("u", "s", "l", "f", "j"),
#   operation = c("create", "create", "create", "create", "create")
# ))
#
#
# test_that(
#   "retrieve_dataset_diffs: Time sensitive case VERBOSE",
#   {expect_true(
#     fsetequal(
#       data.table(retrieve_dataset_diffs(prefix, timestamp_filter = old_df_checkpoint, verbose = TRUE) %>% select(-timestamp)),
#       data.table(expected_diffs))
#   )}
# )
#
#
# # Clean up
# library(aws.s3)
# # Bucket contents in dataframe
# bucket_contents <-
#   get_bucket_df(bucket)
#
# # List of files to delete
# files_to_delete <-
#   bucket_contents %>%
#   select(Key) %>%
#   pull()
#
# # Loop through file list
# for(file in files_to_delete){
#   delete_object(file,
#                 bucket)
# }
