library(shiny)
library(leaflet)
library(tidyverse)
library(rio)
library(shiny.semantic)
library(plotly)

Sys.setlocale("LC_ALL","C")

root_dir <- "C:/Users/Wenyao/Desktop/R/R/main/marine"
setwd(root_dir)

runApp(
  appDir = getwd(),
  port = 80,
  launch.browser = FALSE,
  host = "127.0.0.1"
)
