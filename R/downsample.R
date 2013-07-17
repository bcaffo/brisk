downsample <- function(img, by = 2){
  ivals <- seq(1, dim(img)[1], by = by)
  jvals <- seq(1, dim(img)[2], by = by)
  kvals <- seq(1, dim(img)[3], by = by)
  
  output <- array(0, c(length(ivals), length(jvals), length(kvals)))
  for (i in 1 : length(ivals)){
    for (j in 1 : length(jvals)){
      for (k in 1 : length(kvals)){
        output[i, j, k] <- img[ivals[i], jvals[j], kvals[k]]
      }
    }
  }
  return(output)
}
