library(shiny)
library(shinyWidgets)
library(leaflet)
library(tidyverse)

navbarPage("Locate and Donate", id="main",
           tabPanel("Map", tags$style(type = "text/css", "html, body {width:100%;height:100%}"), 
                    leafletOutput("map", height=1000),
                    absolutePanel(top = 85, left = 90,
                                  pickerInput("donations", label = "Select a donation:",
                                              choices = list("All donations", 
                                                             "Blood",
                                                             "Clothes",
                                                             "Food",
                                                             "Recycle"),
                                              options = list(
                                                
                                                `live-search` = TRUE)
                                  )
                    ),
                    actionButton("NewLocation", "Add more")
           ),
           tabPanel("Data", DT::dataTableOutput("data")),
           tabPanel("Read Me",includeMarkdown("readme.md")))