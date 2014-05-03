require(shiny)
require(rsfmriGraph)

shinyServer(
  function(input, output) {
   output$downloadData <- downloadHandler(
    filename = function(){input$filename},
    content = function(file){
      load(input$rsOut$datapath)
      if (input$what == 1) what <- cor
      else if (input$what == 2) what <- cov
      dat <- corCreate(rsoOut=out, what = what)
      save(dat, file = file)
    }
   )
  }
)

