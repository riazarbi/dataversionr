# Set up minio for s3 testing ##################################################
Sys.setenv(
  MINIO_ROOT_USER = "minio",
  MINIO_ROOT_PASSWORD = "password",
  AWS_ACCESS_KEY_ID = "minio",
  AWS_SECRET_ACCESS_KEY = "password"
)

minio_data_dir <- tempfile()
dir.create(minio_data_dir, recursive = TRUE)

if (!(file.exists("/tmp/minio"))) {
  if(file.exists("minio")) {
    file.copy("minio", "/tmp/minio")
    sys::exec_internal("chmod", c("+x", "/tmp/minio"))
  } else {
  utils::download.file("https://dl.min.io/server/minio/release/linux-amd64/minio",
                       destfile = "/tmp/minio")
  sys::exec_internal("chmod", c("+x", "/tmp/minio"))
  }
}

minio_up <- tryCatch({
  class(aws.s3::s3HTTP(
    region = "",
    base_url = "127.0.0.1:9000",
    use_https = FALSE
  )) == "list"
},
error = function(e)
  FALSE)

while (!minio_up) {
  if (exists("minio_process")) {
    tools::pskill(minio_process)
  }
  minio_process <- sys::exec_background("/tmp/minio",
                                        c("server", minio_data_dir, "--console-address", ":9001"))
  Sys.sleep(2)
  minio_up <- tryCatch({
    class(aws.s3::s3HTTP(
      region = "",
      base_url = "127.0.0.1:9000",
      use_https = FALSE
    )) == "list"
  },
  error = function(e)
    FALSE)
}


bucket <- "dataversionr-tests"
aws.s3::put_bucket(bucket = bucket,
                   region = "",
                   base_url = "127.0.0.1:9000",
                   use_https = FALSE)


# Set base dir
s3 <- arrow::S3FileSystem$create(
  access_key = Sys.getenv("MINIO_ROOT_USER"),
  secret_key = Sys.getenv("MINIO_ROOT_SECRET"),
  scheme = "http",
  endpoint_override = "localhost:9000"
)

s3dir <- s3$cd(bucket)

# Create file dir
fs_data_dir <- "/tmp/dataversionr-tests"
fs <- arrow::LocalFileSystem$create()
fsdir <- fs$cd(fs_data_dir)
make_prefix_path(fsdir)

# Tear down minio after testing ################################################
withr::defer({
  while (is.na(sys::exec_status(minio_process, wait = FALSE))) {
    tools::pskill(minio_process)
  }
}, teardown_env())


withr::defer({
  unlink(minio_data_dir, recursive = TRUE);
  unlink(fs_data_dir, recursive = TRUE)
  }, teardown_env())


withr::defer({
  Sys.unsetenv(c("MINIO_ROOT_USER",
                            "MINIO_ROOT_PASSWORD",
                            "AWS_ACCESS_KEY_ID",
                            "AWS_SECRET_ACCESS_KEY"))
  }, teardown_env())



aws.s3::bucketlist(
           key = "minio",
           secret = "password",
           base_url = "localhost:9000",
           region = "",
           use_https = "false"
)

View(aws.s3::get_bucket_df(
  bucket,
  key = "minio",
  secret = "password",
  base_url = "localhost:9000",
  region = "",
  use_https = "false"
))

remove_prefix("dataversionr-tests", s3dir)
