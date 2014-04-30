library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("CIGAR - Computational Interactive Graphical Analysis of Regions"),
  sidebarPanel(
    h1(''),
    h2('h2 Text')
  ),
  mainPanel(
    h3('Main Panel text')
  )
))
