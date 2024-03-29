library(shiny)
library(leaflet)
library(tidyverse)
library(rio)
library(shiny.semantic)
library(plotly)

# to use English as the locale
Sys.setlocale("LC_ALL","C")

function(input, output, session) {
  
  # search ship ids by ship type
  search_ship_names <- reactive({
    # given a `ship_type`, find the corresponding rows
    ship_of_a_type <- ship_types %>% 
      filter(ship_type == input$ship_type)
    
    # a named vector of `SHIP_ID`
    # with `display_name` as the names (for display purposes)
    ids <- ship_of_a_type$SHIP_ID
    names(ids) <- ship_of_a_type$display_name
    
    ids
  })
  
  # update ship id selector
  observe({
    updateSelectInput(
      session, 
      "ship_id", 
      label = "Which ship?",
      choices = search_ship_names()
    )
  })
  
  # create map
  output$map <- renderLeaflet({
    leaflet(
      options = leafletOptions(
        minZoom = 3, 
        maxZoom = 15, 
        zoomControl = FALSE
      )
    ) %>% 
      addTiles() %>% 
      # set initial map view based on arbitrary choice given in global.R
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
    ship_id = "NA"
  )
  observe({
    invalidateLater(100, session)
    isolate(values$ship_type <- input$ship_type)
    isolate(values$ship_id <- input$ship_id)
  })
  
  # draw ship track
  observe({
    if(values$ship_type != "NA" & values$ship_id != "NA"){
      
      # find the specific ship
      track <- ships %>% 
        filter(
          ship_type == values$ship_type,
          SHIP_ID == values$ship_id
        )
      
      if(nrow(track) == 0){
        return()
      }
      
      # find the case where the ship travels the longest distance
      if(nrow(track) == 1){
        # cases where there's only one observation given an id
        track_longest <- rbind(track, track)
      }else{
        track_longest <- track %>% 
          # the 2 rows where distance is the largest
          # i.e., the row where `ind_longest` == TRUE and the previous one
          slice(
            c(which(track$ind_longest) - 1, which(track$ind_longest))
          )
      }
      
      # plot on map
      leafletProxy("map", data = track) %>% 
        
        # clear everything from the last iteration
        clearShapes() %>% 
        clearMarkers() %>% 
        clearPopups() %>% 
        
        # set map view to fit the current track
        flyToBounds(
          lng1 = min(track$LON, na.rm = TRUE), 
          lat1 = min(track$LAT, na.rm = TRUE), 
          lng2 = max(track$LON, na.rm = TRUE), 
          lat2 = max(track$LAT, na.rm = TRUE)
        ) %>% 
        
        # plot the full track
        # 1) the dots
        addCircles(
          radius = 10,
          lng = ~LON,
          lat = ~LAT,
          opacity = 0.1,
          fillOpacity = 0.1
        ) %>% 
        # 2) the lines
        addPolylines(
          lng = ~LON, 
          lat = ~LAT, 
          opacity = 0.1,
          fillOpacity = 0.1
        ) %>% 
        
        # plot the longest track
        # 1) the dots
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
        # 2) the lines (with labels)
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
    if(values$ship_type != "NA" & values$ship_id != "NA"){
      
      # find the specific ship
      track <- ships %>% 
        filter(
          ship_type == values$ship_type,
          SHIP_ID == values$ship_id
        )
      
      if(nrow(track) == 0){
        return()
      }
      
      ggplotly(
        ggplot(track, aes(x = DATETIME, y = distance)) +
          geom_line(size = 1, color = "dodgerblue") +
          labs(x = "Time", y = "Total Distance") +
          theme_minimal() +
          theme(text = element_text(size = 12))
      )
    }
  })
  
  # total distance by time plot
  output$total_distance <- renderPlotly({
    if(values$ship_type != "NA" & values$ship_id != "NA"){
      
      # find the specific ship
      track <- ships %>% 
        filter(
          ship_type == values$ship_type,
          SHIP_ID == values$ship_id
        )
      
      if(nrow(track) == 0){
        return()
      }
      
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
