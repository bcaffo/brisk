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
                                   maskFileList = NULL,
                                   saveRDA = FALSE,
                                   rdaDIR = NULL,
                                   keepImage = FALSE,
                                   keepMaskedImage = TRUE,
                                   verbose = TRUE,
                                   checkPipeline = FALSE, 
                                   checkTemplate = FALSE){
  
  if ( keepMaskedImage & is.null(maskFileList) ) stop("Must supply a mask if you want to keep masked data")

  if ( !keepMaskedImage & !keepImage & !saveRDA) warning("None of keepMaskImage, keepImage or saveRDA are TRUE, no loaded image data will be saved")

  n <- length(imageList)
  
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

  ##if you just gave one file, it's assumed that it's the mask for the whole group
  if (!is.null(maskFileList) & length(maskFileList) == 1) maskFileList <- rep(maskFileList, n)
    
  for (i in 1 : n){ 
    if (verbose) print(i)
    
    img <- getImage(imageList[[i]]$fileFullPath)
    if (keepImage) imageList[[i]]$image <- img
    
    if (!is.null(maskFileList)){
      maskFile <- maskFileList[i]
      if (!is.null(maskFile) & !is.character(maskFile)) maskFile <- as.character(maskFile)  
      if (!is.null(maskFile) & !file.exists(maskFile)) {
        stop("Mask file doesn't exist")  
      }
      else{
        maskImg <- getImage(maskFile)
        if (length(dim(maskImg)) == 4) maskImg <- maskImg[,,,1]
      }
      maskDim <- dim(maskImg)
      if (dim(img)[1 : length(maskDim)] != maskDim) stop("image and mask dimensions don't match")

      maskImg <- maskImg == 1  
      maskVector <- which(maskImg)
      if (length(dim(img)) != 4) stop("function only works for 4D files, though it should read 3D files in as 4D with the final dimension of 1")
      
      ##now get the masked data 
      imgMatrix <- apply(img, 4, function(x) x[maskVector])
    }
  }   
}

