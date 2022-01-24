library(shiny)
library(shinyWidgets)
library(leaflet)
library(tidyverse)

navbarPage("Locate and Donate", id="main",
           tabPanel("Map", tags$style(type = "text/css", "html, body {width:100%;height:100%}"), 
                    leafletOutput("map", height=1000),
                    
                    #added option to choose which category to display
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
                    )
           ),
           tabPanel("Data", DT::dataTableOutput("data"),
                    #added option to add more donation centers
                    actionButton("NewLocation", "Add more"),
           ),
           tabPanel("Read Me",includeMarkdown("readme.md")))