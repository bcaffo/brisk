\name{groupMatrixPermutationTest}
\alias{groupMatrixPermutationTest}
\title{Group Matrix Permutation Test}
\usage{
groupMatrixPermutationTest(group1MatrixData, group2MatrixData, paired = FALSE,
  cores = 1, permutations = 1000, stat = function(x, y) abs(mean(x) -
  mean(y)))
}
\arguments{
  \item{paired}{whether the observations are paired or not.
  If so, then the group data matrices have to be the same
  size}

  \item{cores}{the number of compute cores to be used (note
  this may not be faster)}

  \item{permutations}{the number of permutations}

  \item{stat}{the statistic (of the form \code{f(x, y)})
  that returns the statistic}
}
\description{
Performs permutation tests for paired and groupd data in
the form of a matrix groups must be aligned by columns.
Multiplicity accounted for by taking the max larger values
of the statistic points to the alternative. Parallelization
is available by setting cores > 1
}
\details{
The matrix has to be structured as features along the
columns and subjects along the rows
}
\author{
Brian Caffo
}

