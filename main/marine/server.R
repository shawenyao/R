library(shiny)
library(leaflet)
library(tidyverse)
library(rio)
library(shiny.semantic)
library(plotly)

Sys.setlocale("LC_ALL","C")

function(input, output, session) {
  
  # search ship names by ship type
  search_ship_names <- reactive({
    ship_types %>% 
      filter(ship_type == input$ship_type) %>% 
      pull(SHIPNAME)
  })
  
  # update ship selector
  observe({
    updateSelectInput(
      session, 
      "ship_name", 
      label = "Which ship?",
      choices = search_ship_names()
    )
  })
  
  # create the map
  output$map <- renderLeaflet({
    leaflet(
      options = leafletOptions(minZoom = 3, maxZoom = 15)
    ) %>% 
      addTiles() %>% 
      fitBounds(
        lng1 = min(initial_coordinates$LON, na.rm = TRUE), 
        lat1 = min(initial_coordinates$LAT, na.rm = TRUE), 
        lng2 = max(initial_coordinates$LON, na.rm = TRUE), 
        lat2 = max(initial_coordinates$LAT, na.rm = TRUE)
      )
  })
  
  # wait for a while before triggering plot
  values <- reactiveValues(
    ship_type = "NA",
    ship_name = "NA"
  )
  observe({
    invalidateLater(2000, session)
    isolate(values$ship_type <- input$ship_type)
    isolate(values$ship_name <- input$ship_name)
  })
  
  # draw ship track
  observe({
    if(values$ship_type != "NA" & values$ship_name != "NA"){
      
      # find the specific ship
      track <- ships %>% 
        filter(
          ship_type == values$ship_type,
          SHIPNAME == values$ship_name
        )
      
      # find the case where the ship travels the longest distance
      if(nrow(track) == 1){
        track_longest <- rbind(track, track)
      }else{
        track_longest <- track %>% 
          slice(
            c(which(track$ind_longest) - 1, which(track$ind_longest))
          )
      }
      
      # plot on map
      leafletProxy("map", data = track) %>% 
        clearShapes() %>% 
        clearMarkers() %>% 
        clearPopups() %>% 
        flyToBounds(
          lng1 = min(track$LON, na.rm = TRUE), 
          lat1 = min(track$LAT, na.rm = TRUE), 
          lng2 = max(track$LON, na.rm = TRUE), 
          lat2 = max(track$LAT, na.rm = TRUE)
        ) %>% 
        
        # plot the full track
        addCircles(
          radius = 5,
          lng = ~LON,
          lat = ~LAT,
          opacity = 0.1,
          fillOpacity = 0.1
        ) %>% 
        addPolylines(
          lng = ~LON, 
          lat = ~LAT, 
          opacity = 0.1,
          fillOpacity = 0.1
        ) %>% 
        
        # plot the longest track
        addMarkers(
          data = track_longest,
          lng = ~LON,
          lat = ~LAT,
          label = ~as.character(DATETIME),
          labelOptions = labelOptions(
            noHide = FALSE, 
            direction = "top",
            opacity = 0.8,
            textsize = "20px",
            style = list(
              "box-shadow" = "3px 3px rgba(0,0,0,0.25)"
            )
          )
        ) %>%
        addPolylines(
          data = track_longest,
          lng = ~LON, 
          lat = ~LAT, 
          color = "dodgerblue",
          opacity = 0.9,
          fillOpacity = 0.9,
          label = as.character(paste0(round(track_longest$distance[2]), "m sailed")),
          labelOptions = labelOptions(
            noHide = TRUE, 
            direction = "bottom",
            opacity = 0.8,
            textsize = "20px",
            style = list(
              "box-shadow" = "3px 3px rgba(0,0,0,0.25)"
            )
          )
        )
    }
  })
  
  # distance by time plot
  output$distance <- renderPlotly({
    
    if(values$ship_type != "NA" & values$ship_name != "NA"){
      
      # find the specific ship
      track <- ships %>% 
        filter(
          ship_type == values$ship_type,
          SHIPNAME == values$ship_name
        )
      # print(values$ship_type);print(values$ship_name)
      
      ggplotly(
        ggplot(track, aes(x = DATETIME, y = distance)) +
          geom_line(size = 1, color = "dodgerblue") +
          labs(x = "Time", y = "Distance") +
          theme_minimal() +
          theme(text = element_text(size = 12))
      )
    }
  })
  
  # distance by time plot
  output$total_distance <- renderPlotly({
    
    if(values$ship_type != "NA" & values$ship_name != "NA"){
      
      # find the specific ship
      track <- ships %>% 
        filter(
          ship_type == values$ship_type,
          SHIPNAME == values$ship_name
        )
      # print(values$ship_type);print(values$ship_name)
      
      # View(track)
      ggplotly(
        ggplot(track, aes(x = DATETIME, y = cumsum(distance))) +
          geom_line(size = 1, color = "tomato") +
          labs(x = "Time", y = "Total Distance") +
          theme_minimal() +
          theme(text = element_text(size = 12))
      )
    }
  })
}
