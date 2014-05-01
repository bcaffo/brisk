#' @title Check if nifti image or read in
#' @import oro.nifti
#' @description Simple check to see if input is character or of class nifti
#' @return nifti object
#' @seealso \link{readNIfTI}
#' @param x character path of image or 
#' an object of class nifti
#' @param reorient (logical) passed to \code{\link{readNIfTI}} if the image
#' is to be re-oriented
#' @export
check.nifti = function(x, reorient=FALSE){
  if (inherits(x, "character")) {
    img = readNIfTI(x, reorient=reorient)
  } else {
    if (inherits(x, "nifti")){
      img = x
    } else {
      stop("x has unknown class - not char or nifti")
    }
  }
  return(img)
}

