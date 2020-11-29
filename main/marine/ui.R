library(shiny)
library(leaflet)
library(tidyverse)
library(rio)
library(shiny.semantic)
library(plotly)

# to use English as the locale
Sys.setlocale("LC_ALL","C")

semanticPage(
  
  tags$head(
    includeCSS("styles.css"),
    tags$style(
      HTML('#controls {background-color: rgba(0,0,255,0.01);}
                 body {overflow-y: hidden;}')
    )
  ),
  
  # the map instance
  leafletOutput(
    "map", width="100%", height="100%"
  ),
  
  absolutePanel(
    id = "controls",
    class = "panel panel-default", 
    fixed = TRUE,
    draggable = TRUE, 
    top = 60, 
    left = 20,
    right = "auto", 
    bottom = "auto",
    width = 500, 
    height = "auto",
    style = "opacity: 0.85",
    
    # the dropdown menus
    div(
      id = "ship_controls",
      class = "ui raised segment",
      div(
        a(class = "ui green ribbon label", "Controls"),
        div(
          tags$i(
            class = "ui exclamation circle icon link",
          ), 
          align = "right",
          `data-tooltip` = "Display a ship's track",
          `data-position` = "bottom right"
        ),
        fluidRow(
          column(
            6,
            selectInput(
              inputId = "ship_type", 
              label = "What kind of ship?",
              choices = unique(ship_types$ship_type),
              selected = unique(initial_coordinates$ship_type)
             ),
          ),
          column(
            6,
            selectInput(
              inputId = "ship_id", 
              label = "Which ship?",
              # intended to be updated by the server side
              choices = NA
            )
          )
        )
      )
    ),
    
    # the distance plot
    div(
      id = "ship_distance",
      class = "ui raised segment",
      div(
        a(class="ui blue ribbon label", "Distance"),
        div(
          tags$i(
            class = "ui exclamation circle icon link",
          ), 
          align = "right",
          `data-tooltip` = "The incremental distance sailed over time",
          `data-position` = "bottom right"
        ),
        plotlyOutput("distance", height = 150)
      )
    ),
    
    # the total distance plot
    div(
      id = "ship_total_distance",
      class = "ui raised segment",
      div(
        a(class="ui red ribbon label", "Total Distance"),
        div(
          tags$i(
            class = "ui exclamation circle icon link",
          ), 
          align = "right",
          `data-tooltip` = "The total distance sailed over time",
          `data-position` = "bottom right"
        ),
        plotlyOutput("total_distance", height = 150)
      )
    )
  )
)
