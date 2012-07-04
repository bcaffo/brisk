##creates a list of images to be loaded into an analytical matrix
##
##fileNames <- full paths to files
## 
## Brian Caffo July 2012
createImageList <- function(fileNames, ids = NULL, ...){
  if (!is.character(fileNames)) stop("fileNames must be character")
  fileCheckStatus <- sapply(fileNames, function(x) file.exists(x))
  if (any(!fileCheckStatus))stop(paste("File does not exist on disk : ", fileNames[!fileCheckStatus], ", "))
  
  if (is.null(ids)) {
    warning("Blank ids field, giving ids 1, 2, 3, ...")
    ids <- 1 : length(fileNames)
  }
  if (!is.character(ids)) ids <- as.character(ids)
  if (length(ids) != length(fileNames)) stop("Length of the ids and length of filenames are not equal")
  if (anyDuplicated(ids) > 0) warning("Duplicate ids")
  rval <- lapply(fileNames, getImageInfo, ...)
  names(rval) <- ids
  return(rval)
}


