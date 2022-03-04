put_diff <- function(df, 
                     prefix,
                     destination,
                     operation, 
                     write_timestamp,
                     verbose = FALSE) {
  
  make_prefix_path(prefix = paste0(prefix, "/history"),
                    destination = destination)
  
  dataset_prefix <- destination$cd(prefix)
  
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
