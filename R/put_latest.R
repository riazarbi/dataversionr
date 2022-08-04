put_latest <- function(new_df, destination) {

  latest_prefix <- fix_path("latest", destination)

  make_prefix(latest_prefix)

  put_location <- fix_path("data.parquet", latest_prefix)

  arrow::write_parquet(new_df,
                put_location)
  return(TRUE)
}
