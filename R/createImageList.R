##creates a list of images to be loaded into an analytical matrix
##
##fileNames <- full paths to files
## 
## Brian Caffo July 2012
createImageList <- function(fileNames, ids = NULL, run = NULL, session = NULL, ...){
  if (!is.character(fileNames)) stop("fileNames must be character")
  fileCheckStatus <- sapply(fileNames, function(x) file.exists(x))
  if (any(!fileCheckStatus))stop(paste("File does not exist on disk : ", fileNames[!fileCheckStatus], ", "))
  
  if (is.null(ids)) {
    warning("Blank ids field, giving ids 1, 2, 3, ...")
    ids <- paste("id", 1 : length(fileNames), sep = "")
  }
  if (!is.character(ids)) ids <- as.character(ids)
  if (is.null(run)) {
    warning("Blank run variable, putting 0")
    run <- rep(0, length(fileNames))
  }
  if (is.null(session)){
    warning("Blank session variable, putting 0")
    session <- rep(0, length(fileNames))
  }
  ##assume if only one value is given that it's the same for everyone
  if (length(run) == 1) run <- rep(run, length(fileNames))
  #same for session
  if (length(session) == 1) session <- rep(session, length(fileNames))
    
  if (length(ids) != length(fileNames)) stop("Length of the ids and length of filenames are not equal")
  if (anyDuplicated(ids) > 0) warning("Duplicate ids")
  if (length(run) != length(fileNames)) stop("Length of run and the length of filenames are not equal")
  if (length(session) != length(fileNames)) stop("Session length and filenames do not have same length")
  rval <- lapply(fileNames, getImageInfo, ...)
  for (i in 1 : length(rval)) {
    rval[[i]]$id <- ids[i]
    rval[[i]]$run <- run[i]
    rval[[i]]$session <- session[i]
    
  }
  return(rval)
}


