update_dataset <- function(new_df,
                           prefix,
                           destination,
                           key_cols = NA,
                           validate_key_cols = TRUE,
                           verbose = FALSE) {

  # Internal function
  put_diff <- function(df,
                       prefix,
                       destination,
                       operation,
                       write_timestamp,
                       verbose = FALSE) {


    dataset_prefix <- destination$cd(prefix)
    make_prefix_path(destination = dataset_prefix$path("history"))


    if (nrow(df) > 0) {
      if (verbose) {
        message(paste("Operation", operation, "has", nrow(df), "diffs..."))
      }

      df <- df %>%
        mutate(timestamp = with_tz(write_timestamp, tzone = "UTC"),
               operation = operation) %>%
        select(timestamp, everything(), operation)

      write_parquet(df,
                    dataset_prefix$OpenOutputStream(
                      paste0(
                        "history/",
                        as.numeric(write_timestamp),
                        "_",
                        operation,
                        ".parquet"
                      )
                    ))
    } else {
      if (verbose) {
        message(paste("Operation", operation, "has no diffs..."))
      }
    }
  }


  dataset_prefix <- destination$cd(prefix)

  # Read in the old table
  if (verbose) {
    message("Reading cached dataset...")
  }

  old_df <- tryCatch({
      collect(open_dataset(dataset_prefix$path("latest")))
  }, error = function(error_condition) {
    NA
  })

  if ("logical" %in% class(old_df)) {
    if (verbose) {
      warning("NO CACHED DATASET FOUND...",
              immediate. = TRUE)
    }
  }

  # WORKAROUND FOR https://issues.apache.org/jira/browse/ARROW-16010
  new_df <- Table$create(new_df)$to_data_frame()
  # END WORKAROUND

  # Calculate diff tables
  diff_tables <-
    compute_df_diff_tables(new_df, old_df, key_cols = key_cols, verbose = verbose)

  # Count number of changes
  diff_counts <- unlist(lapply(diff_tables, nrow))
  number_of_changes <- sum(diff_counts)

  # Define a little function for use inside this function
  put_diff <- function(df, dataset_prefix, operation, timestamp, verbose) {
    if (nrow(df) > 0) {
      if (verbose) {
        message(paste("Operation", operation, "has", nrow(df), "diffs..."))
      }

      df <- df %>%
        mutate(timestamp = lubridate::with_tz(write_timestamp, tzone = "UTC"),
               operation = operation) %>%
        select(timestamp, everything(), operation)
      write_parquet(df,
                    dataset_prefix$path(
                      paste0(
                        "history/",
                        as.numeric(write_timestamp),
                        "_",
                        operation,
                        ".parquet"
                      )
                    ))
    } else {
      if (verbose) {
        message(paste("Operation", operation, "has no diffs..."))
      }
    }
  }


  # Actual write operation logic
  # First, just exit if there are no diffs.
  if (number_of_changes == 0) {
    message("No new data detected. Exiting without write.")
  }
  # If there are actually diffs
  else {
    # Create directories if it's a local filesystem
    dataset_paths <- c("history", "latest", "diff_stats")
    for (path in dataset_paths) {
      make_prefix_path(destination = dataset_prefix$path(path))
    }

    # Determine a write timestamp
    write_timestamp <- lubridate::now()

    # Create a full-row composite key if no key has been specified
    if (is.na(key_cols[1])) {
      key_cols <- colnames(new_df)
    }


    if (verbose) {
      message("Writing diff statistics...")
    }

    # If specified, validate that the keys used are the same as the keys used in the past
    if(validate_key_cols) {
      # Skip validation if there is no cached dataset
      if ("logical" %in% class(old_df)) {
        if (verbose) {
          message("Validating key_cols will be skipped because there is no cached dataset.")
        }
        # If there is a cached dataset, pull the diff stats
      } else {
        if (verbose) {
          message("Validating key_cols are the same for incoming data...")
        }
        # Pull history of key cols in diff stats
        key_cols_history <- retrieve_dataset_diff_stats(prefix,
                                                        destination,
                                                        first_principles = FALSE,
                                                        verbose = verbose) %>% pull(key_cols)

        # Get a list of unique key cols. There should only be one!
        old_key_cols <- unique(key_cols_history)

        if(old_key_cols != paste(key_cols, collapse = "|")) {
          message <- paste0("Key_col validation failed. Old key_cols are ",
                            old_key_cols,
                            ". New key_cols are ",
                            paste(key_cols, collapse = "|"))
          stop(message)
        } else {
          if(verbose) {message("key_col validation passed.")}
        }

      }
    }

    diff_counts_df <- diff_counts %>% t %>%
      as.data.frame %>%
      mutate(`timestamp` = as.character(lubridate::with_tz(write_timestamp, tzone = "UTC")),
             key_cols = paste(key_cols, collapse = "|")) %>%
      rename(create = .data$new_rows,
             update = .data$modified_rows,
             delete = .data$deleted_rows) %>%
      select(.data$timestamp, everything())

    if (verbose) {
      message("Writing diff stats...")
    }
    write_csv_arrow(diff_counts_df,
                    dataset_prefix$OpenOutputStream(paste0(
                      "diff_stats/",
                      as.numeric(write_timestamp),
                      ".csv"
                    )))

    if (verbose) {
      message("Writing diffs...")
    }
    put_diff(diff_tables$new_rows, dataset_prefix, "create", write_timestamp, verbose = verbose)
    put_diff(diff_tables$modified_rows, dataset_prefix, "update", write_timestamp, verbose = verbose)
    put_diff(diff_tables$deleted_rows, dataset_prefix, "delete", write_timestamp, verbose = verbose)

    if (verbose) {
      message("Writing new dataset...")
    }
    write_parquet(new_df,
                  dataset_prefix$OpenOutputStream(
                      "latest/data-01.parquet"
                    )
                  )

    return(TRUE)
  }
}
