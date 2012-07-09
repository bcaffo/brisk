##a general purpose function for getting image info for creating analytic data structures
##returns a very minimal set of information
##The goal is to get a small set of information for loading and working with data
##the templateFile is the file that the data is registered to
##the pipeline is a name of a pipeline just to check later on to make sure all
##images were processed with the same pipeline
##
##Brian Caffo June 2012
getImageInfo <- function(filename, subjectID = NULL, imageID = NULL, type = NULL, pipeline = NULL, templateFile = NULL){
  if (!is.character(filename)) stop("filename is not character")
  if (!file.exists(filename)) stop(paste("File ", filename, " does not exist"))  
  if (!is.null(pipeline)) if (!is.character(pipeline)) stop("pipeline must be character")
  if (!is.null(templateFile)){
    if (!is.character(templateFile)) stop("If specified, templateFile must be a character")
    if (!file.exists(templateFile)) stop("templateFile does not exist")
    templateFile <- getAbsolutePath(dirname(templateFile))
  }
  ##unspecified file type, try to get it from the extension
  if (is.null(type)){
    if (length(grep("\\.", basename(filename))) == 0) stop("Unknown file extension")
    type <- tail(unlist(strsplit(filename, "\\.")), 1)
  }
  if (type == "nii") {
    info <- f.read.nifti.header(filename)
  }
  else if (type == "hdr"){
    info <- f.read.analyze.header(filename)
  }
  else if (type == "img"){
    if (length(grep("\\.", basename(filename))) == 0) stop(".img file without extension, don't know where to look for hdr file")
    hdrFile <- sub("img$", "hdr", filename)
    info <- f.read.analyze.header(hdrFile)
  }
  else if (type == "gz") stop("right now nii.gz has to be gunzipped first. working on that.")
  else stop("Unknown file type")
  rlist <- list(
    subjectID = subjectID,
    imageID = imageID, 
    file = basename(filename),
    location = getAbsolutePath(dirname(filename)),
    fileFullPath = getAbsolutePath(filename),
    dim = info$dim[2 : info$dim[1]],
    units = info$pixdim[2 : info$dim[1]],
    pipelineName = pipeline,
    templateFile = templateFile
  )
  
  return(rlist)
}





