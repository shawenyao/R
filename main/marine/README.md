# Visualization of Marine Data: A Shiny App

![demo](demo.png)

## Summary
A Shiny app for visualization of the Marine data, where the users can:
* find ship by vessel type (e.g., passenger, fishing, etc.)
* track the ship's coordinates over time
* see the farthest distance a ship has sailed between two consecutive records

## Folder Structure
* data_prep.R: convert raw data into Shiny-friendly format
* global.R: global variables of the app
* app.R: app launcher
* ui.R: UI implementation
* server.R: server implementation
* test.R: test script

## Input Data
input/ships_04112020.zip can be found [here](https://drive.google.com/file/d/1IeaDpJNqfgUZzGdQmR6cz2H3EQ3_QfCV/view?usp=sharing)

## Deploy Guide
* Edit the `root_dir` variable in data_prep.R and run the script
* Edit the `root_dir` variable in app.R and run the script

## Demo
https://shawenyao.shinyapps.io/marine/
