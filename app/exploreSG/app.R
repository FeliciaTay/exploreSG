library(shiny)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(shinyTime)
library(ggmap)
library(jsonlite)
library(fontawesome)
library(dplyr)
library(httr)
library(geosphere)
library(sp)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("What's nearby?", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 radioButtons(inputId = "choose",label="What to look for:",
                              choices = c("Hotels","MRT stations","Parking lots")),
                 textInput("loc_input", label = h3("Where are you near?"))
               ),
               mainPanel(
                 leafletOutput(outputId = "nearby")
               )
             ),
    ),
    tabPanel("Live data", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 actionButton("refresh", label = "Refresh")
               ),
               mainPanel(
                 textOutput("text_output")
               )
             )
    )
  ),
)

server <- function(input, output) {
  temp_icon <- makeAwesomeIcon(text=fa("circle"), iconColor="white", markerColor="blue")
  
  plotWithDesc <- function(data) {
    test_map <- leaflet() %>% addTiles() 
    for (i in 1:nrow(data)) {
      curr <- data[i,]
      long <- as.numeric(curr[1])
      lati <- as.numeric(curr[2])
      desc <- as.character(curr[3])
      test_map <- addAwesomeMarkers(test_map, lat=lati,lng=long, popup=desc, icon=temp_icon)
    }
    test_map
  }
  
  plot <- function(data) {
    test_map <- leaflet() %>% addTiles() 
    for (i in 1:nrow(data)) {
      curr <- data[i,]
      long <- as.numeric(curr[1])
      lati <- as.numeric(curr[2])
      test_map <- addAwesomeMarkers(test_map, lat=lati,lng=long, icon=temp_icon)
    }
    test_map
  }
  
  findAverageCoordOfSearch <- function() {
    url<-"https://developers.onemap.sg/commonapi/search"
    query <- list('searchVal' = as.character(input$loc_input), 'returnGeom' = 'Y', 'getAddrDetails' = 'N', 'pageNum' = '1')
    res <- GET(url, query = query, verbose())
    list <- content(res)$results
    # content(res)$results[[1]]
    # contents: SEARCHVAL, X, Y, LATITUDE, LONGTITUDE, LONGTITUDE
    if (length(content(res)$results) == 0) {
      c(103.851959, 1.290270)
    } else if (length(content(res)$results) < 3) { # less than 3 search results
      c(as.numeric(list[[1]]$LONGTITUDE), as.numeric(list$results[[1]]$LATITUDE))
    } else {
      avg_long <- mean(as.numeric(list[[1]]$LONGTITUDE), as.numeric(list[[2]]$LONGTITUDE), as.numeric(list[[3]]$LONGTITUDE))
      avg_lat <- mean(as.numeric(list[[1]]$LATITUDE), as.numeric(list[[2]]$LATITUDE), as.numeric(list[[3]]$LATITUDE))
      c(avg_long, avg_lat)
    }
  }
  
  findDist <- function(X1, Y1, X2, Y2) {
    distm(c(X1, Y1), c(X2, Y2), fun = distHaversine)
  }
  
  output$text_output <- renderText({ 
    "You have selected this"
  })
  
  output$nearby <-renderLeaflet(
    {
      avg_long <- findAverageCoordOfSearch()[1]
      avg_lat <- findAverageCoordOfSearch()[2]
      if (input$choose == "Hotels") {
        hotels <- read.csv("hotel-locations.csv")
        hotels.data <- hotels %>% select("X", "Y", "Name", "TOTALROOMS", "ADDRESS")
        hotels.data <- head(hotels.data %>% rowwise() %>% mutate(dist = findDist(X, Y, avg_long, avg_lat)) %>% arrange(dist), 5)
        plotWithDesc(hotels.data)
      } else if (input$choose == "MRT stations") {
        mrt <- read.csv("mrt_lrt_data.csv")
        mrt <- head(mrt %>% rowwise() %>% mutate(dist = findDist(lng, lat, avg_long, avg_lat)) %>% arrange(dist), 5) %>% mutate(desc = paste(station_name, type))
        mrt.data <- mrt %>% select(lng, lat, desc)
        plotWithDesc(mrt.data)
      } else {
        taxi.stops.data <- read.csv("lta-taxi-stop-kml.csv")
        taxi.stops.data <- taxi.stops.data[,1:2]
        taxi.stops.data <- head(taxi.stops.data %>% rowwise() %>% mutate(dist = findDist(X, Y, avg_long, avg_lat)) %>% arrange(dist), 5)
        plot(taxi.stops.data)
      }
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
