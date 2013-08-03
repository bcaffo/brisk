##attaches subject-specific stuff to an imagelist. For example, in fmri it
##might contain a paradigm
attachSubjectInfo <- function(imageStuff, imageList, name){
  n <- length(imageList)
  if (!is.null(imageStuff)){
    if (!is.list(imageStuff)) stop("imageStuff must be a list")
    if (length(imageStuff) != n) stop("imageStuff must be of length n")
  }
  if(!is.character(name)) stop("name variable must be character")

  for (i in 1 : length(imageList)){
    eval(parse(text = paste("imageList[[i]]$", name, "<-imageStuff[[i]]"))) 
  }
  return(imageList)  
}
