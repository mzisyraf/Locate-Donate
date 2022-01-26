library(shiny)
library(dplyr)
library(leaflet)
library(DT)

shinyServer(function(input, output, session) {
  #Message when app is opened --------------------------------------------------
  showModal(
    modalDialog(
      title = "Welcome to Locate and Donate!",
      easyClose = TRUE,
      footer = modalButton("Happy Donating!"),
      tags$h3(
        "Click on the map to find the nearest donation center."
      )
    )
  )
  
  
  
  # Import Data and clean it ---------------------------------------------------
  lnd_data <- read.csv("LnD_Data.csv", stringsAsFactors = FALSE)
  lnd_data <- data.frame(lnd_data)
  lnd_data$Latitude <- as.numeric(lnd_data$Latitude)
  lnd_data$Longitude <- as.numeric(lnd_data$Longitude)
  
  # new column for the popup label ---------------------------------------------
  lnd_data2 <- mutate(lnd_data, cntnt=paste0('<strong>Name: </strong>',Name,
                                             '<br><strong>Address:</strong> ', Address,
                                             '<br><strong>Contact:</strong> ',Contact,
                                             '<br><strong>Email:</strong> ',Email,
                                             '<br><strong>Website:</strong> ',Website))
  
  # create a color palette for category type in the data file ------------------
  pal <- colorFactor(pal = c("#1b9e77", "#d95f02", "#7570b3", "#000000"), domain = c("Blood", "Clothes", "Food", "Recycle"))
  
  # create the leaflet map according to user input -----------------------------
  filteredData <- reactive({
    if (input$donations == 'All donations') {
      lnd_data2
    } else {
      filter(lnd_data2, Category == input$donations)
    }
  })
  
  # wait for user to click on map ----------------------------------------------
  observeEvent(input$map_click, {
    click <- input$map_click
    text<-paste("Latitude ", round(click$lat,2), "Longtitude ", round(click$lng,2))
    userlat <- click$lat
    userlng <- click$lng
    userlat_rad <- userlat/57.29577951
    userlng_rad <- userlng/57.29577951
    showNotification(
      print(paste0("Your current coordinate is:",
                   userlat,
                   ", ",
                   userlng))
    )
    proxy <- leafletProxy("map")
    
    ##calculate closest place to donate (by category) --------------------------
    
    ## by blood
    lnd_data_blood <- filter(lnd_data, Category == "Blood")
    mindist_blood <- 1000
    for (row in 1:nrow(lnd_data_blood)) {
      loclat_blood <- lnd_data_blood[row, "Latitude"]
      loclng_blood <- lnd_data_blood[row, "Longitude"]
      loclat_blood_rad <- loclat_blood/57.29577951
      loclng_blood_rad <- loclng_blood/57.29577951
      distance_blood <- 1.609344*(3963.0 * acos((sin(userlat_rad) * sin(loclat_blood_rad)) + cos(userlat_rad) * cos(loclat_blood_rad) * cos(loclng_blood_rad - userlng_rad)))
      if (distance_blood < mindist_blood){
        mindist_blood <- distance_blood
        place_name_blood <- lnd_data_blood[row, "Name"]
      }
      
    }
    
    ## by clothes
    lnd_data_clothes <- filter(lnd_data, Category == "Clothes")
    mindist_clothes <- 1000
    for (row in 1:nrow(lnd_data_clothes)) {
      loclat_clothes <- lnd_data_clothes[row, "Latitude"]
      loclng_clothes <- lnd_data_clothes[row, "Longitude"]
      loclat_clothes_rad <- loclat_clothes/57.29577951
      loclng_clothes_rad <- loclng_clothes/57.29577951
      distance_clothes <- 1.609344*(3963.0 * acos((sin(userlat_rad) * sin(loclat_clothes_rad)) + cos(userlat_rad) * cos(loclat_clothes_rad) * cos(loclng_clothes_rad - userlng_rad)))
      if (distance_clothes < mindist_clothes){
        mindist_clothes <- distance_clothes
        place_name_clothes <- lnd_data_clothes[row, "Name"]
      }
      
    }
    
    ## by food
    lnd_data_food <- filter(lnd_data, Category == "Food")
    mindist_food <- 1000
    for (row in 1:nrow(lnd_data_food)) {
      loclat_food <- lnd_data_food[row, "Latitude"]
      loclng_food <- lnd_data_food[row, "Longitude"]
      loclat_food_rad <- loclat_food/57.29577951
      loclng_food_rad <- loclng_food/57.29577951
      distance_food <- 1.609344*(3963.0 * acos((sin(userlat_rad) * sin(loclat_food_rad)) + cos(userlat_rad) * cos(loclat_food_rad) * cos(loclng_food_rad - userlng_rad)))
      if (distance_food < mindist_food){
        mindist_food <- distance_food
        place_name_food <- lnd_data_food[row, "Name"]
      }
      
    }
    
    ## by recycle center
    lnd_data_recycle <- filter(lnd_data, Category == "Recycle")
    mindist_recycle <- 1000
    for (row in 1:nrow(lnd_data_recycle)) {
      loclat_recycle <- lnd_data_recycle[row, "Latitude"]
      loclng_recycle <- lnd_data_recycle[row, "Longitude"]
      loclat_recycle_rad <- loclat_recycle/57.29577951
      loclng_recycle_rad <- loclng_recycle/57.29577951
      distance_recycle <- 1.609344*(3963.0 * acos((sin(userlat_rad) * sin(loclat_recycle_rad)) + cos(userlat_rad) * cos(loclat_recycle_rad) * cos(loclng_recycle_rad - userlng_rad)))
      if (distance_recycle < mindist_recycle){
        mindist_recycle <- distance_recycle
        place_name_recycle <- lnd_data_recycle[row, "Name"]
      }
      
    }
    
    #Popup Message on the closest donation center
    if (input$donations == 'All donations') {
      showModal(
        modalDialog(
          easyClose = TRUE,
          tags$h3(
            "The Closest place to"
          ),
          tags$h3(
              tags$strong(
                "DONATE BLOOD"
              ),
              tags$p(
                place_name_blood,"and is", format(round(mindist_blood, 2), nsmall = 2),"KM away"
              )
          ), 
          tags$h3(
            tags$strong(
              "DONATE CLOTHES"
            ),
            tags$p(
              place_name_clothes," and is ",format(round(mindist_clothes, 2), nsmall = 2),"KM away"
            )
          ),
          tags$h3(
            tags$strong(
              "DONATE FOOD"
            ),
            tags$p(
              place_name_food," and is ",format(round(mindist_food, 2), nsmall = 2),"KM away"
            )
          ),
          tags$h3(
            tags$strong(
              "RECYCLE"
            ),
            tags$p(
              place_name_recycle," and is ",format(round(mindist_recycle, 2), nsmall = 2),"KM away"
            )
          )
        )
      )
    }
    
    if (input$donations == 'Blood'){
      showModal(
        modalDialog(
          easyClose = TRUE,
          tags$h3(
            "The Closest place to"
          ),
          tags$h3(
            tags$strong(
              "DONATE BLOOD"
            ),
            tags$p(
              place_name_blood,"and is", format(round(mindist_blood, 2), nsmall = 2),"KM away"
            )
          )
        )
      )
    }
    
    if (input$donations == 'Clothes'){
      showModal(
        modalDialog(
          easyClose = TRUE,
          tags$h3(
            "The Closest place to"
          ),
          tags$h3(
            tags$strong(
              "DONATE CLOTHES"
            ),
            tags$p(
              place_name_clothes," and is ",format(round(mindist_clothes, 2), nsmall = 2),"KM away"
            )
          )
        )
      )
    }
    
    if (input$donations == 'Food'){
      showModal(
        modalDialog(
          easyClose = TRUE,
          tags$h3(
            "The Closest place to"
          ),
          tags$h3(
            tags$strong(
              "DONATE FOOD"
            ),
            tags$p(
              place_name_food," and is ",format(round(mindist_food, 2), nsmall = 2),"KM away"
            )
          )
        )
      )
    }
    
    if (input$donations == 'Recycle'){
      showModal(
        modalDialog(
          easyClose = TRUE,
          tags$h3(
            "The Closest place to"
          ),
          tags$h3(
            tags$strong(
              "RECYCLE"
            ),
            tags$p(
              place_name_recycle," and is ",format(round(mindist_recycle, 2), nsmall = 2),"KM away"
            )
          )
        )
      )
    }
    
    ## This displays the pin drop circle ---------------------------------------
    proxy %>% 
      clearGroup("new_point") %>%
      #clearMarkers(layerId=input$map_click$id) %>%
      #addPopups(click$lng, click$lat) %>%
      addCircles(click$lng, click$lat, radius=100, color="red", group = "new_point")
    
  })

  #render map ------------------------------------------------------------------
  output$map <- renderLeaflet({
    leaflet(filteredData()) %>% 
      addCircles(lng = ~Longitude, lat = ~Latitude) %>% 
      addTiles() %>%
      addCircleMarkers(lat =  ~Latitude, lng =~Longitude, 
                       radius = 10, popup = ~as.character(cntnt), 
                       color = ~pal(Category),
                       stroke = FALSE, fillOpacity = 0.9)%>%
      addLegend(pal=pal, values=lnd_data$Category,opacity=1, na.label = "Not Available")%>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="ME",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  })
  
  
  
  #create a data object to display data ----------------------------------------
  output$data <-DT::renderDataTable(datatable(
    lnd_data[,c(1,2,5,6,7:8)],filter = 'top',
    colnames = c("Name", "Address", "Contact", "Email", "Website", "Category" )
  ))
  
  # only store the information if the user clicks submit -----------------------
  observeEvent(input$submit, {
    #Added range for longitude and latitude cause Shiny numericInput does not respect range
    if(input$Name!="" && input$Address!="" && input$Latitude<=90 && input$Latitude>=-90 && input$Longitude<=180 && input$Longitude>=-180){
      #show added success ------------------------------------------------------
      showNotification("Location Added")
      
      #add new data ------------------------------------------------------------
      newrow = data.frame(Name = input$Name,
                          Address = input$Address,
                          Latitude = input$Latitude,
                          Longitude = input$Longitude,
                          Contact = input$Contact,
                          Email = input$Email,
                          Website = input$Website,
                          Category = input$Category) 
      lnd_data <<- rbind(lnd_data, newrow)
      
      #re-render the table -----------------------------------------------------
      output$data <-DT::renderDataTable(datatable(
        lnd_data[,c(1,2,5,6,7:8)],filter = 'top',
        colnames = c("Name", "Address", "Contact", "Email", "Website", "Category" )
      ))
      
      #update value of lnd_data2 -----------------------------------------------
      newrow2 <- mutate(newrow, cntnt=paste0('<strong>Name: </strong>',Name,
                                             '<br><strong>Address:</strong> ', Address,
                                             '<br><strong>Contact:</strong> ',Contact,
                                             '<br><strong>Email:</strong> ',Email,
                                             '<br><strong>Website:</strong> ',Website))
      
      lnd_data2 <<- rbind(lnd_data2, newrow2)
    }
  }, ignoreNULL = FALSE)
})
