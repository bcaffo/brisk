##adds subject level covariate data to a neuroimaging data set
##if it's not in the right order, it orders it according to the imageListIDs
addCovariateData <- function(covData, imageList){
  if (!is.data.frame(covDat)) stop("covData must be a dataframe")
  if (is.null(covDat$imageIDs)) stop("covData must have an imageID variable")
  if (is.null(attr(imageList, "imageIDs")))
    stop("imageList doesn't have image IDs")
  
  imageListIDs <- attr(imageList, "imageIDs")
  covDataIDs <- covData$imageIDs

  if (nrow(covData) != length(imageListIDs))
    stop("covData has different number of imageIDs than the imageList")

  if (any(sort(imageListIDs) != sort(covDataIDs)))
    stop("covData has different imageIDs than the imageList")

  ##this gets the covariate data in the same order as the
  ##imageListIDs
  test <- merge(data.frame(imageIDs = imageListIDs),
                covData,
                by = "imageIDs",
                sort = FALSE)

  attributes(imageList)$covData <- covData
  return(imageList)
}
