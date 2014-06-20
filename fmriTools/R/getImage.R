##attempts to load in the data
getImage <- function(filename, type = NULL){
  if (!is.character(filename)) stop("filename is not character")
  if (!file.exists(filename)) stop(paste("File ", filename, " does not exist"))  
  ##unspecified file type, try to get it from the extension
  if (is.null(type)){
    if (length(grep("\\.", basename(filename))) == 0) stop("Unknown file extension")
    type <- tail(unlist(strsplit(filename, "\\.")), 1)
  }
  if (type == "nii") {
    img <- f.read.nifti.volume(filename)
  }
  else if (type == "hdr"){
    if (length(grep("\\.", basename(filename))) == 0) stop(".hdr file without extension, don't know where to look for img file")
    imgFile <- sub("hdr$", "img", filename)
    img <- f.read.analyze.volume(imgFile)
  }
  else if (type == "img"){
    if (length(grep("\\.", basename(filename))) == 0) stop(".img file without extension, don't know where to look for hdr file")
    hdrFile <- sub("img$", "hdr", filename)
    img <- f.read.analyze.volume(hdrFile)
  }
  else if (type == "gz") stop("right now nii.gz has to be gunzipped first. working on that.")
  else stop("Unknown file type, sorry working on that")

  return(img)
}
