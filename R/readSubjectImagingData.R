##takes the output of the createImageList and creates an object with the template and the mask(s)
##for 3D data saves a vector and for 4D data saves a matrix
##Brian Caffo July 2012
readSubjectImagingData <- function(imageList, 
                                   mask, 
                                   template, 
                                   checkPipeline = FALSE, 
                                   checkTemplate = FALSE,
                                   saveRDA = FALSE,
                                   rdaDIR = NULL,
                                   keepData = TRUE){
  
  n <- length(imageList)
  if (checkPipeline){
    pipelineNames <- sapply(imageList, function(x) x$pipelineName)
    if (all(is.null(pipelineNames))) warning("Checking pipeline and no subjects have named pipelines")
    else if (any(is.null(pipelineNames))) stop("Some subjects with missing piplineNames")
    else if (length(unique(pipelineNames)) != n) stop("Some subjects have different pipelineNames")
  }
  if (checkTemplate){
    templateNames <- sapply(imageList, function(x) x$templateFilename)
    if (all(is.null(templateNames))) warning("Checking template and no subjects have template file names")
    else if (any(is.null(templateNames))) stop("Some subjects with missing templates")
    else if (length(unique(templateNames)) != n) stop("Some subjects have different templates")
  }
  
}

