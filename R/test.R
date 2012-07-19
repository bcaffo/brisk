##this function is just for testing out code
library(AnalyzeFMRI)
library(R.utils)
source("getImageInfo.R")
source("createImageList.R")
source("readSubjectImagingData.R")
source("getImage.R")
source("addCovariateData.R")

getImageInfo("getImageInfo.R")
getImageInfo("../data/1.nii")
test2 <- getImage("../data/1.nii")
test2 <- getImage("../data/mask.nii")
test <- createImageList(c("../data/1.nii", "../data/2.nii"),
                        imageIDs = c("subject1", "subject2"),
                        pipeline = "NITRC",
                        templateFile = "../data/template.nii")

covDat <- data.frame( imageIDs = c("subject1", "subject2"), age = c(10, 20))


test <- addCovariateData(covDat, test)

test3 <- readSubjectImagingData(test,
                                maskFile ="../data/mask.nii",
                                rdaDIR = "temp")
