\name{rsfmriGraphUtilityFunctions}
\alias{cor2vec,}
\alias{cov2cor}
\alias{cov2cor,}
\alias{ind2sub}
\alias{rsfmriGraphUtilityFunctions}
\alias{vec2cor}
\title{utilty functions for rsfmriGraph}
\usage{
cov2cor(sigma)

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
indices to subscripts.
}
\author{
Brian Caffo
}
\keyword{cor}

