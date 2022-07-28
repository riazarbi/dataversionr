# Create sample dataframe
new_df <- data.frame(a = 2:6, b = letters[2:6])
new_df[3, 2] <- "g"
old_df <- data.frame(a = 1:5, b = letters[1:5])

# Test diff function
test_that("compute_df_diff_tables: Check diff with key cols works", {
  expect_equal(
    compute_df_diff_tables(new_df, old_df, key_cols = "a"),
    list(
      new_rows = data.frame(a = 6L, b = "f"),
      modified_rows = data.frame(a = 4L, b = "g"),
      deleted_rows = data.frame(a = 1L, b = "a")
    )
  )
})

test_that("compute_df_diff_tables: Check diff with key cols works VERBOSE", {
  expect_message(
    compute_df_diff_tables(new_df, old_df, key_cols = "a", verbose = TRUE),
    "Computing diff dataframe"
  )
})


test_that("compute_df_diff_tables: Check diff without key cols works", {
  expect_equal(
    compute_df_diff_tables(new_df, old_df, key_cols = NA),
    list(
      new_rows = data.frame(a = c(4L, 6L), b = c("g", "f")),
      modified_rows = data.frame(a = as.integer(character()), b = character()),
      deleted_rows = data.frame(a = c(1L, 4L), b = c("a", "d"))
    )
  )
})

test_that("compute_df_diff_tables: Check diff without no old dataset passed VERBOSE",
          {
            expect_message(
              compute_df_diff_tables(new_df, NA, verbose = TRUE),
              "Old dataframe argument is NA"
            )
          })

test_that("compute_df_diff_tables: Check different cols results in error",
          {
            expect_error(
              compute_df_diff_tables(new_df, old_df %>% dplyr::mutate(a = as.character(a))),
              "Newly retrieved table does not have the same column structure as the stored version"
            )
          })


test_that("compute_df_diff_tables: Check diff without no old dataset passed",
          {
            expect_equal(
              compute_df_diff_tables(new_df, NA),
              list(
                new_rows = new_df,
                modified_rows = data.frame(a = as.integer(character()), b = character()),
                deleted_rows = data.frame(a = as.integer(character()), b = character())
              )
            )
          })

test_that("compute_df_diff_tables: Check diff without no old dataset passed VERBOSE",
          {
            expect_message(
              compute_df_diff_tables(new_df, NA, verbose = T),
              "Old dataframe argument is NA"
            )
          })



test_that("compute_df_diff_tables: Check diff when comparing two identical dfs",
          {
            expect_equal(
              compute_df_diff_tables(new_df, new_df),
              list(
                new_rows = data.frame(a = as.integer(character()), b = character()),
                modified_rows = data.frame(a = as.integer(character()), b = character()),
                deleted_rows = data.frame(a = as.integer(character()), b = character())
              )
            )
          })

test_that("compute_df_diff_tables: Check bad first argument errors out",
          {
            expect_error(
              compute_df_diff_tables("bad", new_df),
              "First argument is not a dataframe")
          })

dup_new_df <- new_df
dup_new_df[1,1] <- 3L

test_that("compute_df_diff_tables: Check new_df non unique key cols error out",
          {
            expect_error(
              compute_df_diff_tables(dup_new_df, NA, key_cols = "a"),
              "The new_df key columns do not contain unique rows.")
          })

test_that("compute_df_diff_tables: Check old_df non unique key cols error out",
          {
            expect_error(
              compute_df_diff_tables(new_df, dup_new_df, key_cols = "a"),
              "The old_df key columns do not contain unique rows.")
          })


# Test illegal usage
# Expect error
test_that("compute_df_diff_tables: Check that not passing in a new df results in an error",
          {
            expect_error(compute_df_diff_tables(NA, old_df),
                         "First argument is not a dataframe")
          })

test_that("compute_df_diff_tables: Check that passing bad data to old_df results in error",
          {
            expect_error(compute_df_diff_tables(new_df, "bad"),
                         "Second argument is not a dataframe")
          })
