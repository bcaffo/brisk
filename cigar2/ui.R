library(shiny)
library(rsfmriGraph)

shinyUI(pageWithSidebar(
  headerPanel("CIGAR - Computational Interactive Graphical Analysis of Regions"),
  sidebarPanel(
    h2("Inputs"),
    selectInput('what', label= "What function would you like to collect?", 
                  choices = list("cor" = 1, "cov" = 2), selected = 1),
    textInput('filename', label = "Filename for output", value = "output.rda"),
    fileInput('rsOut', label = "Link to output from ROI Stamper GUI"),
    downloadButton('downloadData', "Download Results")
  ),
  mainPanel(
    textOutput("blah"),
    h1("Calculate cor/cov/icov for subjects and stack into a matrix")
  )
))
