# # Create sample dataframe
# numbers <- 1:6
# letters2 <- c("s", "j", "f", "u", "l", "i")
#
# new_df <- data.frame(a = numbers[2:6], b = letters2[2:6])
# new_df[3, 2] <- "update"
# old_df <- data.frame(a = numbers[1:5], b = letters2[1:5])
#
# # Define a unique prefix for next few tests
# prefix <- paste0(round(as.numeric(now()) * 10000), 0)
#
#
# # put_latest
# put_latest(new_df, s3dir)
#
# # retrieve_dataset
# retrieve_dataset(s3dir)
#
# # commit_dataset
#
# Thoughts
# Maybe we should call all the internal ops put and get
# Put latest
# Put metadata
# Put diff
# Put latest_temporal
# Maybe combine the two above? Because you want only 1 place a timestamp is generated to avoid colissions
#
# And then I don't like reusing the word dataset
#
# What could we call it? We are building datasets (in the arrow sense)
# We are connecting to a
# create_dv : use args to create metadata
# metadata format? json or yaml. What do we include?
#  package version
#  key_cols
#  diffed bool
#  versioned bool or int for number of copies to keep
#  base_path
#
# read_dv: read in dataset
# update_dv:
# destroy_dv: delete dv
# metadata_ver:
# base_path:
# dv_params:
#   key_cols
#   diffed
#   versioned
#
#
# /metadata
# /latest/
# /versions/
# /diff/
#
