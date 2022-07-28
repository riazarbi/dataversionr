#' Retrieve dataset statistics (if the exists)
#'
#' @param prefix string. The dataset location.
#' @param timestamp_filter datetime. The timestamp of the time where you want to seent he summary stats for. If NA (default), returns the latest summary stats.
#' @param collect TRUE/FALSE. Do you want an arrow dataset connection, or a collected dataframe?
#' @param minio_key string. S3 access key. Defaults to Sys.getenv("AWS_ACCESS_KEY_ID").
#' @param minio_secret string. S3 secret key. Defaults to Sys.getenv("AWS_SECRET_ACCESS_KEY")
#' @param minio_url string. S3 url. Defaults to "lake.capetown.gov.za"
#' @param verbose TRUE/FALSE. Should the operations be chatty.
#'
#' @return a dataframe or arrow dataset connection
#' @importFrom aws.s3 object_exists
#' @export
#'
#' @examples
#'\dontrun{
#' #'# Set AWS secrets
#' Sys.setenv(
#'   "AWS_ACCESS_KEY_ID" = MINIO_KEY,
#'  "AWS_SECRET_ACCESS_KEY" = MINIO_SECRET
#' )
#'
#' retrieve_dataset_stats("test_dataset",
#'                  lubridate::now() - lubridate::days(90))
#'                  }
retrieve_dataset_stats <- function(prefix,
                                   timestamp_filter = NA,
                                   collect = TRUE,
                                   minio_key = Sys.getenv("AWS_ACCESS_KEY_ID"),
                                   minio_secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
                                   minio_url = "lake.capetown.gov.za",
                                   verbose = FALSE) {
  bucket <- str_split(prefix, "/", 2)[[1]][1]
  bucket_free_prefix <- str_remove(prefix, bucket)
  
  region_endpoint <-
    stringr::str_split_fixed(minio_url, pattern = "\\.", n = 2)
  region <- region_endpoint[[1]]
  endpoint <- region_endpoint[[2]]
  
  existence_check <-
    aws.s3::object_exists(
      paste0(
        bucket_free_prefix,
        "/summary_statistics/latest/data-01.parquet"
      ),
      bucket,
      key = minio_key,
      secret = minio_secret,
      base_url = endpoint,
      region = region
    )
  
  if (existence_check) {
    retrieve_dataset(
      paste0(prefix, "/summary_statistics"),
      timestamp_filter = timestamp_filter,
      collect = collect,
      minio_key = minio_key,
      minio_secret = minio_secret,
      minio_url = minio_url,
      verbose = verbose
    )
  }
  else {
    stop(
      "Summary statistics don't seem to exist. Are you sure you are writing the dataset with the option summarise = TRUE?"
    )
  }
  
}
