##takes the output of the createImageList and creates an object with the template and the mask(s)
##for 3D data saves a vector and for 4D data saves a matrix
##Brian Caffo July 2012
##note right now it only supports the mask being a 0,1 image or vector of 0,1 images#
##imageList is the output of createImageList
##maskFile filename of mask, array with 0s and 1s, overides maskImg declaration
##maskImg is an array of TRUES and FALSES 
##saveRDA is a flag as to whether to write out a directory of rda files, one per subject
##rdaDRI is where saveRDA writes to
##keepData is a flag as to whether to keep all of the data
readSubjectImagingData <- function(imageList, 
                                   maskFile = NULL,
                                   maskImg = NULL, 
                                   checkPipeline = FALSE, 
                                   checkTemplate = FALSE,
                                   saveRDA = FALSE,
                                   rdaDIR = NULL,
                                   keepImage = FALSE,
                                   keepMaskedImage = TRUE){
  
  if ( keepMaskedImage & is.null(maskFile) & is.null(maskImg) ) stop("Must supply a mask if you want to keep masked data")
  n <- length(imageList)
  
  if (checkPipeline){
    pipelineNames <- sapply(imageList, function(x) x$pipelineName)
    print(pipelineNames)
    if (all(is.null(pipelineNames))) warning("Checking pipeline and no subjects have named pipelines")
    else if (any(is.null(pipelineNames))) stop("Some subjects with missing piplineNames")
    else if (length(unique(pipelineNames)) != 1) stop("Some subjects have different pipelineNames")
  }
  
  if (checkTemplate){
    templateNames <- sapply(imageList, function(x) x$templateFilename)
    if (all(is.null(templateNames))) warning("Checking template and no subjects have template file names")
    else if (any(is.null(templateNames))) stop("Some subjects with missing templates")
    else if (length(unique(templateNames)) != 1) stop("Some subjects have different templates")
  }
  
  if (!is.character(maskFile)) maskFile <- as.character(maskFile)  
  if (!file.exists(maskFile)) stop("Mask file doesn't exist")  
  maskImg <- getImage(maskFile)
  maskImg <- maskImg == 1  
  maskVector <- which(maskImg)

  if (!is.array(maskImg)) stop("maskImg must be an array")
  if (any(!(unique(maskImg) %in% c(TRUE, FALSE)))) stop("maskImg must be Boolean")

  maskDim <- dim(maskImg)
  
  
  for (i in 1 : n){ 
    
  }
   
}

