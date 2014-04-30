\name{downsample}
\alias{downsample}
\title{Lame downsampling function}
\usage{
downsample(img, by = 2)
}
\arguments{
  \item{img}{a 3d array}

  \item{by}{how much to downsample by, 2 skips every other
  voxel, 3 does every third}
}
\description{
A really quick and lame downsampler that I wrote for
plotting just skips a bunch of voxels rather than averaging
or smoothing. This should only be used for exploratory
plotting
}
\author{
Brian Caffo
}

