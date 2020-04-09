suppressWarnings(library(leaflet))
suppressWarnings(library(htmlwidgets))
suppressWarnings(library(beepr))
suppressWarnings(library(tidyverse))

setwd("C:/Users/Wenyao/Desktop/R/R/output/animal_crossing/")


#==== plot map ====
output <- leaflet(
  options = leafletOptions(minZoom = 1, maxZoom = 6)
) %>% 
  addProviderTiles(providers$Wikimedia) %>% 
  setView(lng = 0, lat = 45, zoom = 2) %>% 
  setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%
  addRectangles(
    lng1 = -180, lat1 = 34,
    lng2 = 180, lat2 = 35,
    stroke = FALSE,
    fillColor = "blue",
    fillOpacity = 0.4
  )


#===== save =====
saveWidget(output, file = "my_island.html", selfcontained = TRUE)

# play sound when finished
beep(sound = 2)