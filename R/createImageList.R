##creates a list of images to be loaded into an analytical matrix
##
##fileNames <- full paths to files
##imageIDs unique image identifies
## Brian Caffo July 2012
createImageList <- function(fileNames, imageIDs, subjectIDs = NA, ...){
  if (!is.character(fileNames)) stop("fileNames must be character")
  fileCheckStatus <- sapply(fileNames, function(x) file.exists(x))
  if (any(!fileCheckStatus))stop(paste("File does not exist on disk : ", fileNames[!fileCheckStatus], ", "))

  rval <- lapply(fileNames, getImageInfo, ...)
  
  if (!is.na(subjectIDs)) {
    if (!is.character(subjectIDs)) subjectIDs <- as.character(subjectIDs)
    if (length(subjectIDs) != length(fileNames)) stop("Length of the subjectIDs and length of filenames are not equal")
    for (i in 1 : length(rval)) rval[[i]]$subjectId <- subjectIDs[i]  
  }
  if (!is.character(imageIDs)) imageIDs <- as.character(imageIDs)
  if (length(imageIDs) != length(fileNames)) stop("Length of imageIDs and length of filenames are not equal")
  ##imageIDs have to be unique
  if (anyDuplicated(imageIDs)) stop
  for (i in 1 : length(rval)) rval[[i]]$imageId <- imageIDs[i]  

  return(rval)
}


