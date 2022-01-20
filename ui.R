library(shiny)
library(leaflet)

navbarPage("Locate and Donate", id="main",
           tabPanel("Map", leafletOutput("map", height=1000)),
           tabPanel("Data", DT::dataTableOutput("data")),
           tabPanel("Read Me",includeMarkdown("readme.md")))

