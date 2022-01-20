library(shiny)

library(dplyr)

library(leaflet)

library(DT)

shinyServer(function(input, output) { 
  
  # Import Data and clean it
  
  lnd_data <- read.csv("LnD_Data.csv", stringsAsFactors = FALSE)
  lnd_data <- data.frame(lnd_data)
  lnd_data$Latitude <- as.numeric(lnd_data$Latitude)
  lnd_data$Longitude <- as.numeric(lnd_data$Longitude)
  
  
  
  # new column for the popup label
  
  lnd_data2 <- mutate(lnd_data, cntnt=paste0('<strong>Name: </strong>',Name,
                                          '<br><strong>Address:</strong> ', Address,
                                          '<br><strong>Contact:</strong> ',Contact,
                                          '<br><strong>Email:</strong> ',Email,
                                          '<br><strong>Website:</strong> ',Website))
  
  
  # create a color palette for category type in the data file
  
  pal <- colorFactor(pal = c("#1b9e77", "#d95f02", "#7570b3", "#000000"), domain = c("Blood", "Clothes", "Food", "Recycle"))
  
  # create the leaflet map  

  
  output$map <- renderLeaflet({
    leaflet(lnd_data) %>% 
      addCircles(lng = ~Longitude, lat = ~Latitude) %>% 
      addTiles() %>%
      addCircleMarkers(data = lnd_data2, lat =  ~Latitude, lng =~Longitude, 
                       radius = 5, popup = ~as.character(cntnt), 
                       color = ~pal(Category),
                       stroke = FALSE, fillOpacity = 0.8)%>%
      addLegend(pal=pal, values=lnd_data$Category,opacity=1, na.label = "Not Available")%>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="ME",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  })
  
  #create a data object to display data

  
  output$data <-DT::renderDataTable(datatable(
    lnd_data[,c(1,2,5,6,7:8)],filter = 'top',
    colnames = c("Name", "Address", "Contact", "Email", "Website", "Category" )
  ))
  
})