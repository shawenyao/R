library(shiny)
library(shiny.semantic)

semanticPage(
  
  tags$head(
    # Include our custom CSS
    includeCSS("styles.css"),
    # includeScript("gomap.js"),
    tags$style(
      HTML('#controls {background-color: rgba(0,0,255,0.01);}
                 body {overflow-y: hidden;}')
    )
  ),
  
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
    
    div(
      id = "ship_controls",
      class = "ui raised segment",
      div(
        a(class="ui green ribbon label", "Controls"),
        fluidRow(
          column(
            6,
            selectInput(
              inputId = "ship_type", 
              label = "What kind of ship?",
              choices = unique(ship_types$ship_type)
            ),
          ),
          column(
            6,
            selectInput(
              inputId = "ship_name", 
              label = "Which ship?",
              choices = NA
            )
          )
        )
      )
    ),
    
    div(
      id = "ship_distance",
      class = "ui raised segment",
      div(
        a(class="ui blue ribbon label", "Distance"),
        plotlyOutput("distance", height = 300)
      )
    ),
    
    div(
      id = "ship_total_distance",
      class = "ui raised segment",
      div(
        a(class="ui red ribbon label", "Total Distance"),
        plotlyOutput("total_distance", height = 300)
      )
    )
  )
)
