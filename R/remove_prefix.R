remove_prefix <- function(prefix,
                          destination) {
  if(prefix == "" | prefix == "/") {
    stop('Prefix is not allowed to be "". That would wipe the root of the bucket or directory!')
  }

  if(!("SubTreeFileSystem" %in% class(destination))) {
    stop('class(destination) must include the type "SubTreeFileSystem". This avoids wiping entire filesystems!')
  }

  destination$DeleteDir(prefix)
  return(TRUE)
}

