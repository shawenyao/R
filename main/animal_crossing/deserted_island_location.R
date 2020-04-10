suppressWarnings(library(leaflet))
suppressWarnings(library(htmlwidgets))
suppressWarnings(library(beepr))
suppressWarnings(library(tidyverse))

setwd("C:/Users/Wenyao/Desktop/R/R/output/animal_crossing/")


#==== plot map ====
output <- leaflet(
  options = leafletOptions(minZoom = 2, maxZoom = 4)
) %>% 
  addProviderTiles(providers$Wikimedia) %>% 
  setView(lng = -95, lat = 43.95881, zoom = 2) %>% 
  setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%
  addRectangles(
    lng1 = -180, lat1 = 43.95881 - 0.5,
    lng2 = 180, lat2 = 43.95881 + 0.5,
    stroke = FALSE,
    fillColor = "blue",
    fillOpacity = 0.4
  )


#===== save =====
saveWidget(output, file = "my_island.html", selfcontained = TRUE)

# play sound when finished
beep(sound = 2)