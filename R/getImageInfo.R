getImageInfo <- function(filename, type = "nii"){
  if (!is.character(filename)) stop("filename is not character, attempting to coerce")
  if (!file.exists(filename)) stop(paste("File ", filename, " does not exist"))  

  if (type == "nii") {
    info <- f.read.nifti.header(filename)
  }
  else if (tyhpe == ""){
    info <- f.read.analyze.header(filename)
  }  

}





