#' Title
#'
#' @param documentation_file string. the place you want to save the documentation file. 
#' @param prefix string. the bucket plus prefix of the location of the dataset.
#' @param minio_key string. S3 access key. Defaults to Sys.getenv("AWS_ACCESS_KEY_ID").
#' @param minio_secret string. S3 secret key. Defaults to Sys.getenv("AWS_SECRET_ACCESS_KEY")
#' @param minio_url string. S3 url. Defaults to "lake.capetown.gov.za"
#' @param verbose TRUE/FALSE. Should the operations be chatty.
#'
#' @return TRUE
#' @importFrom aws.s3 put_object
#' @export
#'
#' @examples
#'\dontrun{
#' # Set AWS secrets
#' Sys.setenv(
#'   "AWS_ACCESS_KEY_ID" = MINIO_KEY,
#'  "AWS_SECRET_ACCESS_KEY" = MINIO_SECRET
#' )
#'
#'update_dataset_documentation(prefix)
#'                     }
update_dataset_documentation <- function(documentation_file = "documentation.json",
                                 prefix,
                                 minio_key = Sys.getenv("AWS_ACCESS_KEY_ID"),
                                 minio_secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
                                 minio_url = "lake.capetown.gov.za",
                                 verbose = FALSE) {
  if(file.exists(documentation_file)) {
    if(verbose) {
      message("Local file exists...")
    }
  } else{
    stop(paste("Local file", documentation_file, "does not exist. Check your file path!"))
  }
  
  bucket <- str_split(prefix, "/", 2)[[1]][1]
  bucket_free_prefix <- str_remove(prefix, bucket)
  
  region_endpoint <-
    str_split_fixed(minio_url, pattern = "\\.", n = 2)
  region <- region_endpoint[[1]]
  endpoint <- region_endpoint[[2]]
  
  put_object(documentation_file,
        paste0(bucket_free_prefix,
               "/documentation.json"),
        bucket,
        key = minio_key,
        secret = minio_secret,
        base_url = endpoint,
        region = region
      )
  
  return(TRUE)
  
}
  