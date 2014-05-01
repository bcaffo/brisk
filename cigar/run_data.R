
run.data = function(file_list, roifile, bg.value, type){
  
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
    #     setTxtProgressBar(pb, va)
    output$pbar = reactiveText({
      ifile
    })
    #     colnames(resmat) = ulev
    l.res[[ifile]] = resmat
  }
  names(l.res) = file_list
  return(list(l.res=l.res, ulev = ulev))
}
