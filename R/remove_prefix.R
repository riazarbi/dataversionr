remove_prefix <- function(destination, prompt = TRUE) {

  destination <- make_SubTreeFileSystem(destination)

  if(!("SubTreeFileSystem" %in% class(destination))) {
    stop('class(destination) must include the type "SubTreeFileSystem". This avoids wiping entire filesystems!')
  }

  prefix_depth <- length(strsplit(destination$base_path, split = "/")[[1]])

  if (s3dir$base_fs$type_name == "s3" & prefix_depth == 1) {
    stop('Deleting buckets not supported.')
  }

  if(prompt) {
    msg <- paste0("This command will delete ",
                 destination$base_path,
                 ".\nContinue?\n")
    proceed <- askYesNo(msg, default = FALSE,
             prompts = getOption("askYesNo", gettext(c("Y", "N", "Cancel"))))
  } else {
    proceed <- TRUE
  }

  if(proceed) {
    destination$DeleteDir("/")
    return(TRUE)
  } else {
    return(FALSE)
  }
}

