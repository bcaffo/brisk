print.brisk <- function(x, ...){
  cat("The imageList contains", length(x), "images\n")
  if (is.null(attr(x, "covData"))) cat("No covariate data attached\n")
  else {
    covData <- attr(x, "covData")
    cat("The covariate data has", ncol(covData), "variables ")
    cat("and", nrow(covData), "rows\n")
  }
  if (is.null(attr(x, "rdaDIR"))) cat("The imaging data has not been reread to a BRISK directory\n")
  else {
    cat("The imaging data is stored in:", attr(x, "rdaDIR"), "\n")
    cat("The associated image list file is", attr(x, "imageListFile"), "\n")
  }
  cat("the image ids are:\n")
  print(attr(x, "imageIDs"))
            
}
