\name{roiStamperOuter}
\alias{roiStamperOuter}
\title{roiStamperOuter, stamps out an ROI image from a set of csv files}
\usage{
roiStamperOuter(file_list, roifile, type = "mean", bg.value = NA)
}
\arguments{
  \item{file_list}{vector of full paths to image files
  (either .nii or .nii.gz)}

  \item{roiFile}{full path to the roi image (.nii or
  .nii.gz) containing the}

  \item{type}{character type of ROI summary to create
  currently accepts mean median}
}
\description{
this function stamps out an ROI image an a sequence of fmri
images the ROI image can have any set of unique levels
demarking the levels
}
\details{
The function creates a list with two values output$l.res
are the extracted ROI summaries. The output$ulev are the
unique levels of the ROIs
}
\author{
Brian Caffo
}

