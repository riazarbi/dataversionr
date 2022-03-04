source("set_env.R")

library(arrow)
library(dplyr)
library(janitor)
library(data.table)
library(lubridate)
library(tidyr)

files.sources = list.files("R", full.names = T)
sapply(files.sources, source)


# Set base dir
s3 <- S3FileSystem$create(
  access_key = Sys.getenv("S3_KEY"),
  secret_key = Sys.getenv("S3_SECRET"),
  region = "eu-central-1"
)
s3dr <- s3$cd("datasci-quant")

local_dir <- paste0(getwd(),"/datasets")
fs <- LocalFileSystem$create()
fs$CreateDir(local_dir, recursive = TRUE)
fsdr <- fs$cd(local_dir)

class(fsdr)
class(fsdr)
class(s3dr)
class(s3)
class(fs)

s3dr$base_fs$type_name
fsdr$base_fs$type_name

iriss <- iris %>% mutate(id = 1:nrow(.))

prefix <- "iriss"
iriss2 <- rbind(iris, iris) %>% mutate(id = 1:nrow(.))



update_dataset(new_df = iriss,
               prefix = prefix,
               destination = fsdr,
               key_cols = NA,
               validate_key_cols = TRUE,
               verbose = TRUE) 


update_dataset(new_df = iriss2,
               prefix = prefix,
               destination = fsdr,
               key_cols = NA,
               validate_key_cols = TRUE,
               verbose = TRUE) 

retrieve_dataset(prefix,
                       destination = fsdr,
                       timestamp_filter = NA,
                       collect = TRUE,
                       verbose = FALSE) 


retrieve_dataset_diffs(prefix,
                       destination = fsdr,
                                   timestamp_filter = NA,
                                   collect = TRUE,
                                   verbose = FALSE) 

retrieve_dataset_diff_stats(prefix = prefix,
                            destination = fsdr,
                            first_principles = TRUE,
                            verbose = TRUE)



remove_prefix("iriss",
              fsdr)
  

update_dataset(new_df = iriss,
               prefix = prefix,
               destination = s3dr,
               key_cols = NA,
               validate_key_cols = TRUE,
               verbose = TRUE) 


update_dataset(new_df = iriss2,
               prefix = prefix,
               destination = s3dr,
               key_cols = NA,
               validate_key_cols = TRUE,
               verbose = TRUE) 

retrieve_dataset(prefix,
                 destination = s3dr,
                 timestamp_filter = NA,
                 collect = TRUE,
                 verbose = FALSE) 


retrieve_dataset_diffs(prefix,
                       destination = s3dr,
                       timestamp_filter = NA,
                       collect = TRUE,
                       verbose = FALSE) 

retrieve_dataset_diff_stats(prefix = prefix,
                            destination = s3dr,
                            first_principles = TRUE,
                            verbose = TRUE)

remove_prefix("iriss",
              s3dr)
