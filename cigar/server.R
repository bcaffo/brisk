library(oro.nifti)
#library(genefilter)
library(miscTools)
library(ggplot2)
library(reshape2)
options(shiny.maxRequestSize=100*1024^2) 


get.niifile = function(iput){
  ext = gsub(".*\\.nii$", ".nii", iput$name)
  ext = gsub(".*\\.nii\\.gz$", ".nii.gz", ext)
  outfile = paste0(iput$datapath, ext)
  if (!file.exists(outfile)){
    file.rename(iput$datapath, outfile)
  }
  return(outfile)
}

get.file = function(iput){
  outfile = iput$datapath
  return(outfile)
}

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

shinyServer(function(input, output, session) {
  # output$filetable <- renderTable({
  extract.data = reactive({
  
    
    file_list = input$file_list
    print(file_list)
    roifile = input$roifile
    print(roifile)
    roifilename = input$roifile$name
    
    if (!is.null(roifile) & !is.null(file_list)){
      ## CIGAR
      file_list = read.csv(get.file(file_list), header=FALSE, 
                           stringsAsFactors=FALSE)[,1]
      print(input$file_list)
      stopifnot(all(file.exists(file_list)))
      
      ###
#       roifile = read.csv(get.file(input$roifile), header=FALSE, 
#                          stringsAsFactors=FALSE)[1,1]
#       roifile = read.csv(input$roifile, header=FALSE, 
#                          stringsAsFactors=FALSE)[1,1]      
      roifile = get.niifile(input$roifile)
      print("ROI File is")      
      print(roifile)
#       dir(basename(roifile))
#       print(file.exists(roifile))
      stopifnot(all(file.exists(roifile)))

      bg.value = input$bg.value
      type = input$type

      rr = run.data(file_list, roifile, bg.value, type)
      l.res = rr$l.res
      ulev = rr$ulev
    } else {
      l.res = list(NULL)
      ulev = NULL
    }
    return(list(l.res=l.res, 
                file_list=file_list, 
                ulev=ulev,
                roifilename = roifilename))
  })


	# Partial example

  output$outtab = renderTable({
    out = extract.data()
    l.res = out$l.res
    if (!is.null(l.res)){
      df = data.frame(l.res[[1]][1:5, 1:5])
    return(df)
    } else {
      return(data.frame("No Data"))
    }
  }, include.rownames=FALSE, sanitize.text.function=`(`)



  output$outplot = renderPlot({
    out = extract.data()
    l.res = out$l.res
    x = l.res[[1]]
    ulev = out$ulev
    if (!is.null(x)){
#       cat("l.res is")
#       print(l.res)
#       print(is.null(l.res))
      xx = melt(x)
      colnames(xx) = c("Scan", "roi", "value")
      xx$roi = factor(xx$roi)
      levs = sample(ulev, 10)
      xx = xx[ xx$roi %in% levs, ]
      g = ggplot(xx, aes(x=Scan, y=value, color = roi)) +
        geom_line(alpha = .5) + guides(color=FALSE) +
        ylab("Signal")
      print(g)
#       matplot(x)      
    } else {
      plot.new()
      text(0.5,0.5,"no data")
    }
  })


  output$dlrda <- downloadHandler(
    filename = function() {
      dt = format(Sys.time(), "%Y%m%d_%H%M")
      fname = input$fname
      if (fname == ""){
        fname = "output"
      }
      fname = paste0(fname, "_", dt, ".rda")
    },
    content = function(file) {
      out = extract.data()
      rundate = date()
      save(out, rundate, file=file)
    }
  )

})