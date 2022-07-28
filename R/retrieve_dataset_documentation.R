#' Retrieve dataset documentation
#'
#' @param prefix string. s3 location of dataset, including bucket name
#' @param save_path string. Defaults to documentation.json. local path to save documentation file to.
#' @param open logocal. Defaults to FALSE. Should we open the file once we've retrieved it?
#' @param minio_key string. S3 access key. Defaults to Sys.getenv("AWS_ACCESS_KEY_ID").
#' @param minio_secret string. S3 secret key. Defaults to Sys.getenv("AWS_SECRET_ACCESS_KEY")
#' @param minio_url string. S3 url. Defaults to "lake.capetown.gov.za"
#' @param verbose TRUE/FALSE. Should the operations be chatty.
#'
#' @return TRUE
#' @export
#' @importFrom utils file.edit head
#' @importFrom aws.s3 save_object
#'
#' @examples
#'\dontrun{
#' #'# Set AWS secrets
#' Sys.setenv(
#'   "AWS_ACCESS_KEY_ID" = MINIO_KEY,
#'  "AWS_SECRET_ACCESS_KEY" = MINIO_SECRET
#' )
#'
#' retrieve_dataset_documentation("test_dataset")
#'                  }
retrieve_dataset_documentation <- function(prefix,
                                           save_path = "documentation.json",
                                           open = FALSE,
                                           minio_key = Sys.getenv("AWS_ACCESS_KEY_ID"),
                                           minio_secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
                                           minio_url = "lake.capetown.gov.za",
                                           verbose = FALSE) {
  if (file.exists(save_path)) {
    stop(
      "There is already a file in your save path. Move it or change your save path before proceeding."
    )
  }
  
  bucket <- str_split(prefix, "/", 2)[[1]][1]
  bucket_free_prefix <- str_remove(prefix, bucket)
  
  region_endpoint <-
    stringr::str_split_fixed(minio_url, pattern = "\\.", n = 2)
  region <- region_endpoint[[1]]
  endpoint <- region_endpoint[[2]]
  
  existence_check <-
    suppressMessages(
      object_exists(
        paste0(bucket_free_prefix,
               "/documentation.json"),
        bucket,
        key = minio_key,
        secret = minio_secret,
        base_url = endpoint,
        region = region
      )
    )
  
  if (existence_check) {
    if (verbose) {
      message("Remote documentation file found. Retrieving it now.")
    }
    retrieve <- save_object(
      paste0(bucket_free_prefix,
             "/documentation.json"),
      bucket,
      file = save_path,
      overwrite = TRUE,
      key = minio_key,
      secret = minio_secret,
      base_url = endpoint,
      region = region
    )
    retrieve <- retrieve == save_path
  }  else {
    if (verbose) {
      message("No remote documentation file found. Generating a new template.")
      message("Reading in dataset column names and types...")
    }
    dataset_to_document <-
      retrieve_dataset(prefix, collect = FALSE) %>% head %>% collect
    if (verbose) {
      message("Generating documentation template...")
    }
    documentation_generator(dataset_to_document, save_path)
    retrieve <- file.exists(save_path)
  }
  if (retrieve & verbose) {
    message(paste("Documentation file saved as", save_path,
                  "."))
  }
  if (open) {
    if (verbose) {
      message("Opening documentation file for editing.")
    }
    file.edit(save_path)
  }
  if (verbose) {
    message(
      "To update the dataset documentation file after editing, use the function update_dataset_documentation"
    )
  }
  return(retrieve)
}
