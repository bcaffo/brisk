library(shiny)
shinyServer(
  function(input, output) {
    ###check to make sure that the files are there
    output$toPrint <- renderPrint({
      if (!is.null(input$file_list)){
        files <- as.vector(read.csv(input$file_list$datapath, header = FALSE)[,1])
        there <- sapply(files, file.exists)
        if (all(there)) cat("All files there!")
        else (print(files[!there]))
        #read.csv(input$file_list$datapath, header=FALSE)
      }
    })
  }
)

