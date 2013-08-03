##a general purpose function for getting image info for creating analytic data structures
getImageInfo <- function(filename, type = NULL){
  if (!is.character(filename)) stop("filename is not character, attempting to coerce")
  if (!file.exists(filename)) stop(paste("File ", filename, " does not exist"))  

  ##unspecified file type, try to get it from the extension
  if (type = NULL){
    temp <- strsplit(filename, "/.")[[1]]
    type <- temp[length(temp)]
  }
  
  if (type == "nii") {
    info <- f.read.nifti.header(filename)
  }
  else if (type == ""){
    info <- f.read.analyze.header(filename)
  }  

}





