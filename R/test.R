##this function is just for testing out code
library(AnalyzeFMRI)
library(R.utils)
source("getImageInfo.R")
source("createImageList.R")
getImageInfo("getImageInfo.R")
getImageInfo("../data/1.nii")
test <- createImageList(c("../data/1.nii", "../data/2.nii"), 1 : 2)




