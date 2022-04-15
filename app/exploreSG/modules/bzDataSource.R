library(shiny)
library(dplyr)

# module variables


# UI code
dataSource_UI <- function(id){
  ns <- NS(id) # generate namespace function
  tagList( # UI elements
    checkboxInput(
      inputId = ns("data_haw"),
      label = "Hawkers"
    ),
    checkboxInput(
      inputId = ns("data_hot"),
      label = "Hotels"
    ),
    checkboxInput(
      inputId = ns("data_mrt"),
      label = "MRT / LRT"
    ),
    checkboxInput(
      inputId = ns("data_res"),
      label = "Restaurants"
    ),
    checkboxInput(
      inputId = ns("data_tax"),
      label = "Taxi Stands"
    ),
    checkboxInput(
      inputId = ns("data_tou"),
      label = "Tourism"
    ),
    checkboxInput(
      inputId = ns("data_wea"),
      label = "Weather"
    )
  )
}

# server code
dataSource_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){ # logic
      
    }
  )
}