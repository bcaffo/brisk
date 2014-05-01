#' @title roiStamperOuter, stamps out an ROI image from a set of csv files
#' 
#' @author Brian Caffo
#' 
#' @param file_list vector of full paths to image files (either .nii or .nii.gz)
#' @param roiFile full path to the roi image (.nii or .nii.gz) containing the 
#' @param type character type of ROI summary to create currently accepts mean median
#' 
#' @description this function stamps out an ROI image an a sequence of fmri images
#' the ROI image can have any set of unique levels demarking the levels
#' 
#' @details The function creates a list with two values output$l.res are the extracted
#' ROI summaries. The output$ulev are the unique levels of the ROIs

roiStamperOuter <- function(file_list, roifile,  type = "mean", bg.value = NA){
  nfiles = length(file_list)
  roi = readNIfTI(roifile, reorient=FALSE)
  ulev = sort(unique(c(roi)))

  #     bg.value = NA
  ulev = ulev[ !(ulev %in% bg.value) ]
  roi.ind = lapply(ulev, function(lev){
    which(roi %in% lev, arr.ind=FALSE)
  })


  ifile = 1;
  cor.res = l.res = vector(mode="list", length = nfiles)
  #   pb = txtProgressBar(min=0, max=nfiles,initial=0)
  for (ifile in seq(nfiles)){
    print(ifile)
    img = readNIfTI(file_list[ifile], reorient=FALSE)

    ### V by T
    mat = matrix(img, nrow=prod(dim(img)[1:3]), ncol=ntim(img))
    ### double check
    x = img[,,,1]
    stopifnot(all(x == mat[,1]))
    # [1] TRUE

    ### resmat is T by R (n roi)
    resmat = sapply(roi.ind, function(indices){
      m = mat[indices,, drop=FALSE]
      res = switch(type,
               "median" = apply(m, 2, median),
               "mean"= colMeans(m),
      )
      })
    #     colnames(resmat) = ulev
    l.res[[ifile]] = resmat
  }
  names(l.res) = file_list
  return(list(l.res=l.res, ulev = ulev))
}

  

# some code to test it out
# 
fileList <- c(
  "/Users/Brian/Documents/GitHub/brisk/cigar/data/0010001.1.1.nyu.nii.gz",
 "/Users/Brian/Documents/GitHub/brisk/cigar/data/0010002.1.1.nyu.nii.gz",
  "/Users/Brian/Documents/GitHub/brisk/cigar/data/0010003.1.1.nyu.nii.gz"
)
roiFile <- "/Users/Brian/Documents/GitHub/brisk/cigar/data/M1_clusters_3mm.nii"

out <- roiStamperOuter(fileList, roiFile)



