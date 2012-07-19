##takes the output of the createImageList and creates an object with
##the template and the mask(s)
##for 3D data saves a vector and for 4D data saves a matrix
##Brian Caffo July 2012
##note right now it only supports the mask being a 0,1 image or vector of 0,1 images#
##imageList is the output of createImageList

##maskFile characters pointing to mask file.

##rdaDIR is where the rda is written to
##keepImage creates a copy of the image as an RDA file

readSubjectImagingData <- function(imageList, 
                                   rdaDIR,
                                   maskFile = NULL,
                                   keepImage = FALSE,
                                   verbose = TRUE,
                                   overwrite = FALSE){
  if (is.null(rdaDIR)) stop("rdaDir must be specified")
  if (!file.exists(rdaDIR)) { 
    if (verbose) print("Making rdaDIR")
    dir.create(path = rdaDIR)
  }
  else if (length(list.files(rdaDIR)) > 0 & verbose) {
    print("Existing files in rdaDIR")
  }    

  n <- length(imageList)

  ##check the imageIDs which must be there
  imageIDs <- sapply(imageList, function(x) x$imageID)
  ##imageIDs have to be unique
  if (anyDuplicated(imageIDs)) stop("Duplicate imageIDs")
  

  if (!is.null(maskFile)){
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
  }
    
  
  for (i in 1 : n){ 
    if (verbose) print(i)

    img <- getImage(imageList[[i]]$fileFullPath)  
    
    if (all(dim(img)[1 : 3] != maskDim))
      stop("image and mask dimensions don't match")

    
    ##now get the masked data
    if (length(dim(img)) == 4){
      imgMatrix <- t(apply(img, 4, function(x) x[maskVector]))
    }
    else if (length(dim(img)) == 3){
      imgMatrix <- as.vector(img[maskVector])
    }
    else stop("Masking only implemented for 3D and 4D images")
    
    
    subjDIR <- paste(rdaDIR, "/", imageList[[i]]$imageID, "/", sep = "")
    if (!file.exists(subjDIR)) dir.create(subjDIR)
    
    imageList[[i]]$subjDIR <- getAbsolutePath(subjDIR)
    
    if (keepImage) {
      imageLoc <- paste(subjDIR, "image.rda", sep = "")
      if (file.exists(imageLoc) & !overwrite)
        stop("Image file already exists and overwrite is FALSE")
      else save(img, maskVector, file = imageLoc, compress = TRUE)
      imageList[[i]]$rdaImageLoc <- imageLoc
    }
    if (!is.null(maskFile)) {
      maskedImageLoc <- paste(subjDIR, "maskImage.rda", sep = "/")
      if (file.exists(maskedImageLoc) & !overwrite) {
        stop("Masked image file already exists and overwrite is FALSE")
      }
      else save(imgMatrix, maskVector, file = maskedImageLoc, compress = TRUE)
      imageList[[i]]$maskFile <- maskFile
      imageList[[i]]$maskLength <- length(maskVector)
      imageList[[i]]$rdaMaskedImageLoc <- maskedImageLoc
    }
    
  } 
  imageListFile <- paste(rdaDIR, "/imageList.rda", sep = "")

  attributes(imageList)$rdaDIR <- rdaDIR
  attributes(imageList)$imageListFile <- imageListFile
  
  save(imageList, file = imageListFile)

  return(imageList)
}

