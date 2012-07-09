##this function is just for testing out code
library(AnalyzeFMRI)
library(R.utils)
source("getImageInfo.R")
source("createImageList.R")
source("readSubjectImagingData.R")
source("getImage.R")
getImageInfo("getImageInfo.R")
getImageInfo("../data/1.nii")
test2 <- getImage("../data/1.nii")
test2 <- getImage("../data/mask.nii")
test <- createImageList(c("../data/1.nii", "../data/2.nii"), 1 : 2, 1 : 2, pipeline = "NITRC", templateFile = "../data/template.nii")

readSubjectImagingData(test, maskFileList ="../data/mask.nii", checkPipeline = TRUE)

