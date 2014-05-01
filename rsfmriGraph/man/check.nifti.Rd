\name{check.nifti}
\alias{check.nifti}
\title{Check if nifti image or read in}
\usage{
\method{check}{nifti}(x, reorient = FALSE)
}
\arguments{
  \item{x}{character path of image or an object of class
  nifti}

  \item{reorient}{(logical) passed to
  \code{\link{readNIfTI}} if the image is to be
  re-oriented}
}
\value{
nifti object
}
\description{
Simple check to see if input is character or of class nifti
}
\seealso{
\link{readNIfTI}
}

