library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("CIGAR - Computational Interactive Graphical Analysis of Regions"),
  sidebarPanel(
    h2("Inputs"),
    fileInput('file_list', label = "CSV file with full paths to filenames"),
    fileInput(inputId='roifile', label='nii or nii.gz ROI file')
  ),
  mainPanel(
    h1("ROI Stamper Outer"),
    h4("Status of image files"),
    textOutput("toPrint"),
    h4("Status of ROI file")
  )
))
