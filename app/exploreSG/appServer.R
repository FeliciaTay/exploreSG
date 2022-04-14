source("lib.R")
source("funcs.R")
source("datascript/weather.R")
source("appUItabRest.R")

appServer <- function(input, output) {
  temp_icon <- makeAwesomeIcon(text=fa("circle"), iconColor="white", markerColor="blue")
  
  output$tabPanelHtml <- renderUI({HTML(getUITabHtml(input))})
  output$refresh <- renderUI({HTML(refreshUITabHtml(input))})
  
  output$title <- renderUI({HTML("Welcome to ExploreSG!")})
  
  output$desc <- renderUI({ 
    HTML(paste("<br>", "What's nearby? tab: You can input a location using the text input box and you will be able to toggle between finding hotels, MRT stations, taxi stands or hawker centres nearby. You can find the 2hr weather forecast in that area to help you better plan your iternary:)",
               "<br>", "! Please only input places in Singapore. The default location the map will show is at the centre of Singapore.",
               "<br>", "<br>", "Data tab:"))
  })
  
  output$weather <- renderUI({ 
    coords = findAverageCoordOfSearch(input)
    avg_long = coords[1]
    avg_lat = coords[2]
    weather = getWeatherForecast() %>% mutate(dist = findDist(lng, lat, avg_long, avg_lat)) %>% arrange(dist)
    top = head(weather, 1)
    HTML(paste(paste("Area:", "<u>", top$area, "</u>"), paste("The 2-hour weather forecast for this area:", "<u>", top$forecast, "</u>"), sep="<br/>"))
  })
  
  output$nearby <-renderLeaflet(
    {
      coords = findAverageCoordOfSearch(input)
      avg_long = coords[1]
      avg_lat = coords[2]
      if (input$choose == "Hotels") {
        hotels <- read.csv("data/hotels.csv")
        hotels.data <- hotels %>% select("lng", "lat", "Name", "TOTALROOMS", "ADDRESS")
        hotels.data <- head(hotels.data %>% rowwise() %>% mutate(dist = findDist(lng, lat, avg_long, avg_lat)) %>% arrange(dist), 5)
        plotMap(hotels.data, temp_icon, withDesc = TRUE)
      } else if (input$choose == "MRT stations") {
        mrt <- read.csv("data/mrts.csv")
        mrt <- head(mrt %>% rowwise() %>% mutate(dist = findDist(lng, lat, avg_long, avg_lat)) %>% arrange(dist), 5) %>% mutate(desc = paste(station_name, type))
        mrt.data <- mrt %>% select(lng, lat, desc)
        plotMap(mrt.data, temp_icon, withDesc = TRUE)
      } else if (input$choose == "Taxi stands") {
        taxi.stops.data <- read.csv("data/taxis.csv")
        taxi.stops.data <- taxi.stops.data[,1:2]
        taxi.stops.data <- head(taxi.stops.data %>% rowwise() %>% mutate(dist = findDist(lng, lat, avg_long, avg_lat)) %>% arrange(dist), 5)
        plotMap(taxi.stops.data, temp_icon, withDesc = FALSE)
      } else {
        hawker <- read.csv("data/hawkers.csv")
        hawker <- head(hawker %>% rowwise() %>% mutate(dist = findDist(lng, lat, avg_long, avg_lat)) %>% arrange(dist), 5)
        hawker.plot <- hawker %>% select(lng, lat, name_of_centre)
        plotMap(hawker.plot, temp_icon, withDesc = TRUE)
      }
    }
  )
  
  output$tourist <-renderPlot(
    {
      tourist <- "data/TOURISM.kml"
      touristData <- st_read(tourist)
      tourist_3857 <- st_transform(touristData, 3857)
      # Define a function to fix the bbox to be in EPSG:3857
      ggmap_bbox <- function(map) {
        if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
        
        # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, and set the names to what sf::st_bbox expects:
        map_bbox <- setNames(unlist(attr(map, "bb")), 
                             c("ymin", "xmin", "ymax", "xmax"))
        
        # Convert the bbox to an sf polygon, transform it to 3857, and convert back to a bbox (convoluted, but it works)  
        bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
        
        # Overwrite the bbox of the ggmap object with the transformed coordinates 
        attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
        attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
        attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
        attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
        map
      }
      map <- get_map("singapore", maptype = "roadmap", zoom = 11, source = "google", color = "bw")
      # Use the function:
      map <- ggmap_bbox(map)
      ggmap(map) + 
        coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
        #transportation
        geom_sf(data = tourist_3857, col = "#80DA83", lwd = 0.6,inherit.aes = FALSE, show.legend = FALSE)
    }
  )
}