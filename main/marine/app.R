library(shiny)
library(leaflet)
library(tidyverse)
library(rio)
library(shiny.semantic)
library(plotly)

# to use English as the locale
Sys.setlocale("LC_ALL","C")

# to be edited depending on project folder
root_dir <- "C:/Users/Wenyao/Desktop/R/R/main/marine"
setwd(root_dir)

runApp(
  appDir = getwd(),
  port = 80,
  launch.browser = FALSE,
  host = "127.0.0.1"
)
