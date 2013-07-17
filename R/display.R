display <- function(file, type = "analyze", imageCenter = c(0, 0, 0), 
                    roi = TRUE,
                    positive = TRUE,
                    negative = TRUE,
                    plevels = 1,
                    nlevels = 1
                    ){
  if (type == "analyze") img2 <- extract.data(read.ANALYZE(file, setmask = FALSE))[,,,1]
  else if (type == "nifti") img2 <- extract.data(read.NIFTI(file, setmask = FALSE))[,,,1]
  else if (type == "img") img2 <- file
  else stop("unsupported file type")

  
  img <- extract.data(read.ANALYZE( "ROIimage", setmask = FALSE))[,,,1]
  roiDim <- dim(img)
  imageDim <- dim(img2)
  
  roiCenter <- c(46, 64, 37)
  resize <- array(0, roiDim);
  resize[(1 : imageDim[1]) + roiCenter[1] - imageCenter[1],
         (1 : imageDim[2]) + roiCenter[2] - imageCenter[2],
         (1 : imageDim[3]) + roiCenter[3] - imageCenter[3]] = img2;
  
  rm(img2)

  resize <- downsample(resize)
  img <- downsample(img)
  
  timg <- array(0, dim(img))
  timg[img > 0 & img < max(img)] <- 1

  if (roi) contour3d(timg, level= 1, smooth = 20, fill = TRUE, mask = array(TRUE, dim(img)), alpha = .2, add = FALSE)

  timg <- array(0, dim(resize))
  if (negative) {
    timg[resize < 0] <- -1 * resize[resize < 0]
    contour3d(timg,
              level= nlevels,
              color = heat.colors(length(nlevels)),
              smooth = 20,
              fill = TRUE,
              mask = array(TRUE, dim(timg)),
              alpha = .8,
              add = TRUE)
  }
  if (positive) {
    timg[resize > 0] <- resize[resize > 0]
    contour3d(timg,
              level= plevels,
              color = topo.colors(length(plevels)),
              smooth = 20,
              fill = TRUE,
              mask = array(TRUE, dim(timg)),
              alpha = .8,
              add = TRUE)
  }
  return(NULL)
}
