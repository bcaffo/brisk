\name{rsfmriGraphUtilityFunctions}
\alias{cor2vec}
\alias{cor2vec,}
\alias{cov2cor,}
\alias{ind2sub}
\alias{rsfmriGraphUtilityFunctions}
\alias{vec2cor}
\title{utilty functions for rsfmriGraph}
\usage{
cor2vec(sigma)

vec2cor(vec, n)

ind2sub(dims, indexVal)
}
\arguments{
  \item{sigma}{a covariance or square matrix}

  \item{a}{vector to be put back unto a square matrix}

  \item{n}{number of columns or rows of the square matrix}

  \item{dims}{dimensions}

  \item{indexVal}{index value to be converted to
  subscripts}
}
\description{
A set of utility functions for the rsfmriGraph package
}
\details{
This is a set of utiltiy functions for rsfmriGraph
including functions to go back and forth between vectors
and covariance matrices covariances to correlations and
indices to subscripts. \code{cov2cor} a function that
computes the correlation from a cov matrix \code{cor2vec} a
function that vectorizes the covariance or correlation
omitting the diagonal \code{vec2cor} a function that
unvectorizes a correlation vector, note it assumes the
diagonal is one and that the vector input is the purely
upper triangular part \code{ind2sub} given index value
returns array coordinates for a 3d array
}
\author{
Brian Caffo
}
\keyword{cor}

