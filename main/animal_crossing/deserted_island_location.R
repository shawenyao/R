suppressWarnings(library(leaflet))
suppressWarnings(library(htmlwidgets))
suppressWarnings(library(beepr))
suppressWarnings(library(tidyverse))

setwd("C:/Users/Wenyao/Desktop/R/R/")


#==== plot map ====
output <- leaflet(
  options = leafletOptions(minZoom = 3, maxZoom = 9)
) %>% 
  addProviderTiles(providers$Stamen.Toner) %>% 
  setView(lng = -95, lat = 38, zoom = 4) %>% 
  setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%
  addRectangles(
    lng1 = -118.456554, lat1 = 34.078039,
    lng2 = -118.436383, lat2 = 34.062717,
    fillColor = "transparent"
  )


#===== save =====
saveWidget(output, file = "map.html", selfcontained = TRUE)

# play sound when finished
beep(sound = 2)