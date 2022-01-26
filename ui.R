library(shiny)
library(shinyWidgets)
library(leaflet)

navbarPage("Locate and Donate", id="main",
           #Map tab ------------------------------------------------------------
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
           #Data tab -----------------------------------------------------------
           tabPanel("Data", DT::dataTableOutput("data"),
           ),
           #New Location tab ---------------------------------------------------
           tabPanel("Add new Location",
                    tags$h3('Please enter the necessary information'),
                    textInput('Name', 'Name'),
                    textInput('Address', 'Address'),
                    numericInput('Latitude', 'Latitude', 0, min = -90,  max = 90, step = NA),
                    numericInput('Longitude', 'Longitude', 0, min = -180,  max = 180, step = NA),
                    textInput('Contact', 'Contact (Optional)'),
                    textInput('Email', 'Email (Optional)'),
                    textInput('Website', 'Website (Optional)'),
                    selectInput('Category', 'Category', choices = list("Blood", "Clothes", "Food", "Recycle"), selected = NULL, multiple = FALSE, selectize = TRUE),
                    actionButton('submit', 'Submit')
           ),
           #Read me tab --------------------------------------------------------
           tabPanel("Read Me",includeMarkdown("readme.md")))