##takes the output of the createImageList and creates an object with the template and the mask(s)
##for 3D data saves a vector and for 4D data saves a matrix
##Brian Caffo July 2012
##note right now it only supports the mask being a 0,1 image or vector of 0,1 images#
##imageList is the output of createImageList

##maskFile characters pointing to mask file.

##rdaDRI is where the rda is written to
##keepData is a flag as to whether to keep all of the data
readSubjectImagingData <- function(imageList, 
                                   rdaDIR,
                                   maskFile = NULL,
                                   keepImage = TRUE,
                                   keepMaskedImage = TRUE,
                                   verbose = TRUE,
                                   checkStuff = TRUE,
                                   overwrite = FALSE){
  
  if (is.null(maskFile) & !keepMaskedImage) stop("A mask file must be given if keepMaskedImage is true")

  if ( !keepMaskedImage & !keepImage ) stop("None of keepMaskImage, keepImage are TRUE, no loaded image data will be saved")

  if (is.null(rdaDIR)) stop("rdaDir must be specified")
  if (!file.exists(rdaDIR)) { 
    if (verbose) print("Making rdaDIR")
    dir.create(path = rdaDIR)
  }
  else if (length(list.files(rdaDIR)) > 0 & verbose) print("Existing files in rdaDIR")
    
  
  n <- length(imageList)
  
  ##check the imageIDs which must be there
  imageIDs <- sapply(imageList, function(x) x$imageID)
  ##imageIDs have to be unique
  if (anyDuplicated(imageIDs)) stop("Duplicate imageIDs")
  
  if (checkStuff){
    pipelineNames <- sapply(imageList, function(x) x$pipelineName)
    if (is.null(pipelineNames)) warning("Checking pipeline and no subjects have named pipelines")
    else if (length(unique(pipelineNames)) != 1) warning("Some subjects have different pipelineNames")
    templateNames <- sapply(imageList, function(x) x$templateFilename)
    if (is.null(templateNames)) warning("Checking template and no subjects have template file names")
    else if (length(unique(templateNames)) != 1) warning("Some subjects have different templates")
  }
  
  if (!is.character(maskFile)) maskFile <- as.character(maskFile)  
  if (!file.exists(maskFile)) stop("Mask file doesn't exist")  
  else {
    maskImg <- getImage(maskFile)
    if (length(dim(maskImg)) == 4) maskImg <- maskImg[,,,1]
    maskImg <- maskImg == 1  
    maskDim <- dim(maskImg)
    if (length(maskDim) != 3) stop("Mask must be a 3D image")
    maskVector <- which(maskImg)
  }
    
  
  for (i in 1 : n){ 
    if (verbose) print(i)
    
    img <- getImage(imageList[[i]]$fileFullPath)  
    
    if (all(dim(img)[1 : 3] != maskDim)) stop("image and mask dimensions don't match")
      
    ##now get the masked data 
    imgMatrix <- t(apply(img, 4, function(x) x[maskVector]))
    
    subjDIR <- paste(rdaDIR, "/", imageList[[i]]$imageID, "/", sep = "")
    if (!file.exists(subjDIR)) dir.create(subjDIR)
    
    if (keepImage) {
      imageLoc <- paste(subjDIR, "image.rda", sep = "")
      if (file.exists(imageLoc) & !overwrite) stop("Image file already exists and overwrite is FALSE")
      else save(img, maskVector, file = imageLoc, compress = TRUE)
    }
    if (keepMaskedImage) {
      maskedImageLoc <- paste(subjDIR, "maskImage.rda", sep = "/")
      if (file.exists(maskedImageLoc) & !overwrite) stop("Masked image file already exists and overwrite is FALSE")
      else save(imgMatrix, maskVector, file = maskedImageLoc, compress = TRUE)
    }
    
  } 
  metaInfoLoc <- paste(rdaDIR, "/imageList.rda", sep = "")

  attributes(imageList)$rdaDIR <- rdaDIR
  attributes(imageList)$keepImage <- keepImage
  attributes(imageList)$keepMaskedImage <- keepMaskedImage
  
  save(imageList, file = metaInfoLoc)

  return(imageList)
}

