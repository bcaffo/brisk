\name{corCreate}
\alias{corCreate}
\title{corCreate loops over a collection of csv files and creates correlations}
\usage{
corCreate(fileList, path = "./", loadFunction = read.csv,
  transpose = FALSE, what = cor, asMatrix = TRUE, columns = NULL,
  columnNames = NULL, cores = 1, ...)
}
\arguments{
  \item{filelist}{chararcter list of csv files}

  \item{loadFunction}{the function used to load in the
  files (defaults to read.csv)}

  \item{transpose}{whether or not to transpose the date
  before creating the correlations}

  \item{what}{function that is used to create matrix,
  obvious candidates are \code{\link{cor}}
  \code{{link(cov)}}}

  \item{asMatrix}{each correlation run through cor2vec and
  stacked up as a matrix? Otheriwse dump everything into a
  list}

  \item{columns}{use only specific columns of the csv
  files}

  \item{columnNames}{character vectror of names}

  \item{cores}{parallelize the computation, only needed if
  \code{what} is something computationally intensive,
  otherwise it will slow things down}
}
\value{
returns a matrix or list of output from what
}
\description{
Reads a collection of csv files and puts them into a list
of either correlations or covariances
}
\details{
This function loops over a collection of csv files and then
creates a matrix of vectorized covariance or correlation
functions or a list of them.
}
\author{
Brian Caffo
}
\seealso{
cor2vec
}
\keyword{cor}

