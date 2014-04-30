#' @title utilty functions for rsfmriGraph 
#' @description A set of utility functions for the rsfmriGraph package
#' @details This is a set of utiltiy functions for rsfmriGraph including
#' functions to go back and forth between vectors and covariance matrices
#' covariances to correlations and indices to subscripts.
#' 
#' @author Brian Caffo
#' @name rsfmriGraphUtilityFunctions
#' @rdname rsfmriGraphUtilityFunctions
#' 
#' @param sigma a covariance or square matrix 
#' @param a vector to be put back unto a square matrix
#' @param n number of columns or rows of the square matrix
#' @param dims dimensions
#' @param indexVal index value to be converted to subscripts
#' @keywords cor
#' @aliases cov2cor, cor2vec, ind2sub
NULL

##a function that computes the correlation from a cov matrix
#' @rdname rsfmriGraphUtilityFunctions 
#' @export
cov2cor <- function(sigma) diag(1 / sqrt(diag(sigma))) %*% sigma %*% diag(1 / sqrt(diag(sigma)))

##a function that vectorizes the covariance or correlation omitting the diagonal
cor2vec <- function(sigma) sigma[upper.tri(sigma, diag = FALSE)]
#' @rdname rsfmriGraphUtilityFunctions 

##a function that unvectorizes a correlation vector, note it assumes the diagonal is one and
##that the vector input is the purely upper triangular part
vec2cor <- function(vec, n){ rval <- diag(rep(1, n)); rval[upper.tri(rval)] <- vec; return(rval + t(rval) - diag(rep(1, n)))} 
#' @rdname rsfmriGraphUtilityFunctions 
##checking it
##temp <- controls[[1]]
##max(abs(temp - vec2cov(cov2vec(temp), roiCount)))


##given index value returns array coordinates for a 3d array
ind2sub <- function(dims, indexVal){
  I <- dims[1]
  J <- dims[2]
  K <- dims[3]
  v <- indexVal

  k <- (indexVal - 1) %/% (I * J) + 1
  temp <- (indexVal - 1) %% (I * J) 
  j <- temp %/% I + 1
  i <- temp %% I + 1
  cbind(i, j, k)
}
#' @rdname rsfmriGraphUtilityFunctions 
#' @examples
#' ##here's an example array with the indices valued
#' arrayDim <- 2 : 4
#' arrayIndices <- 1 : prod(arrayDim) 
#' ##show how they fill into the array
#' array(arrayIndices, arrayDim)
#' ##get the coordinates
#' arrayCoordinates <- ind2sub(2 : 4, 1 : 24)
#' ##show that they work
#' array(arrayCoordinates[,1], arrayDim)
#' array(arrayCoordinates[,2], arrayDim)
#' array(arrayCoordinates[,3], arrayDim)
#' @export

