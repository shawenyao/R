suppressWarnings(library(leaflet))
suppressWarnings(library(htmlwidgets))
suppressWarnings(library(beepr))
suppressWarnings(library(rio))
suppressWarnings(library(tidyverse))
suppressWarnings(library(zoo))

setwd("C:/Users/Wenyao/Desktop/R/R/output/coronavirus")
set.seed(350)

refresh_data <- TRUE


#===== load data =====
if(refresh_data){
  
  coronavirus_raw <- import("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  
  # format data
  coronavirus_input <- coronavirus_raw %>% 
    mutate(
      id = if_else(
        `Province/State` == "",
        `Country/Region`,
        paste0(`Province/State`, " @ ", `Country/Region`)
      ),
      display_name = if_else(
        `Province/State` == "",
        `Country/Region`,
        `Province/State`
      )
    ) %>% 
    select(-`Province/State`, -`Country/Region`) %>% 
    gather(date, cases, -id, -Lat, -Long, -display_name) %>% 
    mutate(
      date = as.Date(date, format = "%m/%d/%y")
    ) %>% 
    arrange(id, date) %>% 
    # forward filling
    group_by(id) %>% 
    mutate(
      cases = na.locf(cases)
    ) %>% 
    ungroup()
  
  export(coronavirus_input, "../../input/coronavirus/coronavirus.csv")
  
}

# read from pre-saved copy
coronavirus_input <- import("../../input/coronavirus/coronavirus.csv")

# smooth scale for better visualization effect
coronavirus <- coronavirus_input %>% 
  mutate(
    # cases_scaled = pnorm(cases, mean = mean(cases), sd = sd(cases) * 3),
    cases_scaled = cases,
    size = 10 + (cases_scaled - min(cases_scaled)) / (max(cases_scaled) - min(cases_scaled)) * 110
  )


#===== plot on map =====
# initialize
output <- leaflet(
  options = leafletOptions(minZoom = 3, maxZoom = 9)
) %>% 
  addTiles() %>% 
  setView(lng = 20, lat = 45, zoom = 4) %>% 
  setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90)

# plot group by date
all_dates <- unique(coronavirus$date)
for(a_date in all_dates){
  print(as.Date(a_date))
  output <- output %>%
    addCircleMarkers(
      data = coronavirus %>% 
        filter(date == as.Date(a_date)) %>% 
        filter(cases != 0),
      lng = ~Long, 
      lat = ~Lat,
      radius = ~size,
      stroke = FALSE, 
      color = "red",
      fillOpacity = 0.4,
      label = ~paste0(
        display_name, ": ", 
        cases, " ", 
        if_else(as.numeric(cases) == 1, "case", "cases"), 
        " confirmed"
      ),
      labelOptions = labelOptions(noHide = FALSE, textsize = "15px"),
      group = as.character(as.Date(a_date))
    )
  
  # hide everything except the first date's data (initially)
  if(a_date != all_dates[1]){
    output <- output %>% 
      hideGroup(as.character(as.Date(a_date)))
  }
}

# add control panel
output <- output %>% 
  addLayersControl(
    baseGroups = as.character(all_dates[length(all_dates):1]),
    options = layersControlOptions(collapsed = TRUE)
  ) 

# let the animation autoplay on load and loop
source("../../main/coronavirus/js_code.R", echo = TRUE)
output <- output %>% 
  onRender(
    jsCode = js_code
  )


#===== save =====
saveWidget(output, file = "map.html", selfcontained = TRUE)

# play sound when finished
beep(sound = 2)
