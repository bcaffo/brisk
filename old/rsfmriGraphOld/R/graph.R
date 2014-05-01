library(fmri)
library(AnalyzeFMRI)
library(rgl)
library(misc3d)

img <- f.read.nifti.volume("ic-15.nii")[,,,1]
##threshold
timg <- (abs(img) > 2.5) + 0

mask <- f.read.nifti.volume("brainMask.nii")[,,,1]

source("downsample.R")

dtimg <- timg#downsample(timg)
dmask <- mask#downsample(mask)

params <- par3d()

#open3d(params)
contour3d(dmask, level= 1, smooth = 20, fill = TRUE, mask = array(TRUE, dim(dtimg)), alpha = .2, add = TRUE)
contour3d(dtimg, level= 1, smooth = 20, fill = TRUE, mask = array(TRUE, dim(dtimg)), alpha = .2, add = TRUE, color = "red")


browseURL(paste("file://",writeWebGL(dir=file.path("./", "webGL2"),  width=500), sep = ""))
