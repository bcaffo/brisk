##this function is just for testing out code
library(AnalyzeFMRI)
library(R.utils)
source("getImageInfo.R")
source("createImageList.R")
source("readSubjectImagingData.R")
getImageInfo("getImageInfo.R")
getImageInfo("../data/1.nii")
test <- createImageList(c("../data/1.nii", "../data/2.nii"))
test2 <- test
test2[1]$pipelineName <- "Test"
readSubjectImagingData(test, 1, 1)



