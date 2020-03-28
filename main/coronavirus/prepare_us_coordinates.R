suppressWarnings(library(rvest))
suppressWarnings(library(rio))
suppressWarnings(library(dplyr))

setwd("C:/Users/Wenyao/Desktop/R/R/input/coronavirus")

county_coordinates <- read_html("https://en.wikipedia.org/wiki/User:Michael_J/County_table") %>% 
  html_nodes("#mw-content-text > div > table") %>% 
  html_table()

output <- county_coordinates[[1]] %>% 
  transmute(
    state = State,
    county = `County [2]`,
    fips = FIPS,
    lat_county = Latitude %>% 
      gsub("°", "", .) %>% 
      gsub("–", "-", .) %>%  
      as.numeric(),
    long_county = Longitude %>% 
      gsub("°", "", .) %>% 
      gsub("–", "-", .) %>%  
      as.numeric()
  )

export(output, "us_county_coordinates.csv")
