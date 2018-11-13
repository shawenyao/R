library(tools)
library(rvest)

setwd("C:/Users/Wenyao/Desktop/R/R/")
source("./functions/functions_scrape_displate.R")

download_base_dir <- "C:/Users/Wenyao/Desktop/Displate/"

# "City Maps Vintage" series
download_collections(
  collection_url = "https://displate.com/lukeainsworth7/city-maps-vintage",
  collection_name = "City Maps Vintage", 
  total_page_number = 21,
  download_base_dir = download_base_dir
)

# "Old School Maps" series
download_collections(
  collection_url = "https://displate.com/iwoko/oldschool-maps", 
  collection_name = "Old School Maps", 
  total_page_number = 7,
  download_base_dir = download_base_dir
)

# "City Light Maps" series
download_collections(
  collection_url = "https://displate.com/retina/city-light-maps", 
  collection_name = "City Light Maps", 
  total_page_number = 1,
  download_base_dir = download_base_dir
)

# "Minimalist Travel Posters" series
download_collections(
  collection_url = "https://displate.com/henryrivers/minimalist-travel-posters", 
  collection_name = "Minimalist Travel Posters", 
  total_page_number = 2,
  download_base_dir = download_base_dir
)

# "Modern City Posters" series
download_collections(
  collection_url = "https://displate.com/inspirowl/modern-city-posters", 
  collection_name = "Modern City Posters", 
  total_page_number = 6,
  download_base_dir = download_base_dir
)
