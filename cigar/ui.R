types = c("mean", "median")
bgvals = c(NA, 0, NaN)


shinyUI(pageWithSidebar(
  headerPanel("CIGAR" ),
  sidebarPanel(
    fileInput("file_list", "CSV of 4D nii files", 
              multiple = FALSE),
    fileInput("roifile", "ROI nii file", 
              multiple = TRUE),    
    selectInput("type", "Summary for ROI:",
                choices = types, selected=types[1]),
    selectInput("bg.value", "Background Value for ROI:",
                choices= bgvals, selected=bgvals[1]),
    textInput("fname", "Name for output (will be date_stamped)",
                value = ""),    
    downloadButton("dlrda", "Download rda of ROI timeseries")   
  ),
  mainPanel(
#     plotOutput("outplot"),
    textOutput("pbar"),
    tableOutput("outtab")
  )
))