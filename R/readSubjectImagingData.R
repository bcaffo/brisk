##takes the output of the createImageList and creates an object with the template and the mask(s)
##for 3D data saves a vector and for 4D data saves a matrix
##Brian Caffo July 2012
##note right now it only supports the mask being a 0,1 image or vector of 0,1 images#
##imageList is the output of createImageList

##maskFile characters pointing to mask file.

##saveRDA is a flag as to whether to write out a directory of rda files, one per subject
##rdaDRI is where saveRDA writes to
##keepData is a flag as to whether to keep all of the data
readSubjectImagingData <- function(imageList, 
                                   maskFile = NULL,
                                   saveRDA = FALSE,
                                   rdaDIR = NULL,
                                   keepImage = FALSE,
                                   keepMaskedImage = TRUE,
                                   verbose = TRUE,
                                   checkStuff = TRUE){
  
  if (is.null(maskFile) & !keepMaskedImage) stop("A mask file must be given if keepMaskedImage is true")

  if ( !keepMaskedImage & !keepImage & !saveRDA ) stop("None of keepMaskImage, keepImage or saveRDA are TRUE, no loaded image data will be saved")

  if (saveRDA){
    if (is.null(rdaDIR)) stop("rdaDir must be specified if saveRDA is TRUE")
    if (!file.exists(rdaDIR)) { 
      warning("Making rdaDIR")
      dir.create(path = rdaDIR)
    }
    else if (length(list.files(rdaDIR)) > 0) warning("existing files in rdaDIR")
    
  }
  n <- length(imageList)
  
  ##check the imageIDs which must be there
  imageIDs <- sapply(imageList, function(x) x$imageID)
  ##imageIDs have to be unique
  if (anyDuplicated(imageIDs)) stop("Duplicate imageIDs")
  
  if (checkStuff){
    pipelineNames <- sapply(imageList, function(x) x$pipelineName)
    if (is.null(pipelineNames)) warning("Checking pipeline and no subjects have named pipelines")
    else if (length(unique(pipelineNames)) != 1) warning("Some subjects have different pipelineNames")
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
  
  #if (!is.array(maskImg)) stop("maskImg must be an array")
  #if (any(!(unique(maskImg) %in% c(TRUE, FALSE)))) stop("maskImg must be Boolean")
  
  
  for (i in 1 : n){ 
    if (verbose) print(i)
    
    img <- getImage(imageList[[i]]$fileFullPath)  
    
    if (keepImage & !saveRDA) imageList[[i]]$image <- img      
    if (all(dim(img)[1 : 3] != maskDim)) stop("image and mask dimensions don't match")
      
    ##now get the masked data 
    imgMatrix <- t(apply(img, 4, function(x) x[maskVector]))
    
    if (keepMaskedImage & !saveRDA) imageList[[i]]$maskedImage <- imgMatrix

    if (saveRDA){
      subjDIR <- paste(rdaDIR, "/", imageList[[i]]$imageID, "/", sep = "")
      dir.create(subjDIR)
      if (keepImage) save(img, maskVector, file = paste(subjDIR, "image,rda", compress = TRUE, sep = ""))
      if (keepMaskedImage) save(imgMatrix, maskVector, file = paste(subjDIR, "maskImage.rda", sep = "/"), compress = TRUE)
    }
    
  } 
  if (saveRDA){
    save(imageList, file = paste(subjDIR, "/imageListHeader.rda",sep = ""))
  }
  return(imageList)
}

