##takes the output of the createImageList and creates an object with the template and the mask
##Brian Caffo July 2012
readSubjectImagingData <- function(imageList, 
                                   mask, 
                                   template, 
                                   checkPipeline = FALSE, 
                                   checkTemplate = FALSE){
  
  n <- length(imageList)
  if (checkPipeline){
    pipelineNames <- sapply(test, function(x) x$pipelineName)
    if (all(is.null(pipelineNames))) warning("Checking pipeline and no subjects have named pipelines")
    else if (any(is.null(pipelineNames))) stop("Some subjects with missing piplineNames")
    else if (length(unique(pipelineNames)) != n) stop("Some subjects have different pipelineNames")
  }
  
  
}

