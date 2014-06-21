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
##here's an example array with the indices valued
## arrayDim <- 2 : 4
## arrayIndices <- 1 : prod(arrayDim) 
## ##show how they fill into the array
## array(arrayIndices, arrayDim)

## ##get the coordinates
## arrayCoordinates <- ind2sub(2 : 4, 1 : 24)

## ##show that they work
## array(arrayCoordinates[,1], arrayDim)
## array(arrayCoordinates[,2], arrayDim)
## array(arrayCoordinates[,3], arrayDim)
