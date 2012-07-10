##takes the output of the createImageList and creates an object with the template and the mask(s)
##for 3D data saves a vector and for 4D data saves a matrix
##Brian Caffo July 2012
##note right now it only supports the mask being a 0,1 image or vector of 0,1 images#
##imageList is the output of createImageList

##maskFileList a vector of characters pointing to mask files. Can be of length 1 or n. If it's
##1 it's assumed to be common for all subjects.

##saveRDA is a flag as to whether to write out a directory of rda files, one per subject
##rdaDRI is where saveRDA writes to
##keepData is a flag as to whether to keep all of the data
readSubjectImagingData <- function(imageList, 
                                   maskFile = NULL,
                                   maskIMG = NULL,
                                   saveRDA = FALSE,
                                   rdaDIR = NULL,
                                   keepImage = FALSE,
                                   keepMaskedImage = TRUE,
                                   verbose = TRUE,
                                   checkPipeline = FALSE, 
                                   checkTemplate = FALSE){
  
  if (is.null(maskFile) & is.null(maskImg)) stop("one of a mask file or mask image must be retained")

  if ( !keepMaskedImage & !keepImage & !saveRDA ) warning("None of keepMaskImage, keepImage or saveRDA are TRUE, no loaded image data will be saved")

  n <- length(imageList)
  ##check the imageIDs which must be there
  imageIDs <- sapply(imageList, function(x), x$imageID)
  ##imageIDs have to be unique
  if (anyDuplicated(imageIDs)) stop
  
  if (checkPipeline){
    pipelineNames <- sapply(imageList, function(x) x$pipelineName)
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
  else {
    if (length(dim(maskImg)) == 4) maskImg <- maskImg[,,,1]
    maskImg <- getImage(maskFile)
    maskImg <- maskImg == 1  
  }
  
  if (!is.array(maskImg)) stop("maskImg must be an array")
  if (any(!(unique(maskImg) %in% c(TRUE, FALSE)))) stop("maskImg must be Boolean")
  
  maskDim <- dim(maskImg)
  maskVector <- which(maskImg)
  
  for (i in 1 : n){ 
    if (verbose) print(i)
    
    img <- getImage(imageList[[i]]$fileFullPath)
    if (keepImage) imageList[[i]]$image <- img      
    if (dim(img)[1 : length(maskDim)] != maskDim) stop("image and mask dimensions don't match")
    if (length(dim(img)) != 4) stop("function only works for 4D files, though it should read 3D files in as 4D with the final dimension of 1")
      
    ##now get the masked data 
    imgMatrix <- apply(img, 4, function(x) x[maskVector])
    
  }   
}

