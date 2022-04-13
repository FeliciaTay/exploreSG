source("lib.R")

appServer <- function(input, output) {
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
  
  processLongAndLat <- function(place) {
    url<-"https://developers.onemap.sg/commonapi/search"
    query <- list('searchVal' = as.character(place), 'returnGeom' = 'Y', 'getAddrDetails' = 'N', 'pageNum' = '1')
    res <- GET(url, query = query, verbose())
    list <- content(res)$results
    if (length(content(res)$results) == 0) { 
      c(0,0)
    } else {
      c(as.numeric(list[[1]]$LONGTITUDE), as.numeric(list[[1]]$LATITUDE)) # use the top result;s long and lat
    }
  }
  
  output$text_output <- renderText({ 
    "You have selected this"
  })
  
  output$title <- renderUI({ 
    HTML("Welcome to ExploreSG!")
  })
  
  output$desc <- renderUI({ 
    HTML(paste("<br>", "What's nearby? tab: You can input a location using the text input box and you will be able to toggle between finding hotels, MRT stations, taxi stands or hawker centres nearby. You can find the 2hr weather forecast in that area to help you better plan your iternary:)",
               "<br>", "! Please only input places in Singapore. The default location the map will show is at the centre of Singapore.",
               "<br>", "<br>", "Data tab:"))
  })
  
  output$weather <- renderUI({ 
    date <- as.character(Sys.Date())
    time <- strftime(Sys.time(), "%T")
    url <- paste0("https://api.data.gov.sg/v1/environment/2-hour-weather-forecast?date_time=")
    url <- paste0(url,date,"T",time)
    data <- fromJSON(url)
    weather.area.raw.data <- as.data.frame(data$items$forecasts)
    # fit in longitude, lat from another file and merge
    data.long.lat <- read.csv("data/weather.csv")
    weather.area.raw.data <- merge(weather.area.raw.data, data.long.lat, all.x=T, by="area")
    avg_long <- findAverageCoordOfSearch()[1]
    avg_lat <- findAverageCoordOfSearch()[2]
    top <- head(weather.area.raw.data %>% rowwise() %>% mutate(dist = findDist(lng, lat, avg_long, avg_lat)) %>% arrange(dist), 1)
    HTML(paste(paste("Area:", "<u>", top$area, "</u>"), paste("The 2-hour weather forecast for this area:", "<u>", top$forecast, "</u>"), sep="<br/>"))
  })
  
  output$nearby <-renderLeaflet(
    {
      avg_long <- findAverageCoordOfSearch()[1]
      avg_lat <- findAverageCoordOfSearch()[2]
      if (input$choose == "Hotels") {
        hotels <- read.csv("data/hotel-locations.csv")
        hotels.data <- hotels %>% select("lng", "lat", "Name", "TOTALROOMS", "ADDRESS")
        hotels.data <- head(hotels.data %>% rowwise() %>% mutate(dist = findDist(lng, lat, avg_long, avg_lat)) %>% arrange(dist), 5)
        plotWithDesc(hotels.data)
      } else if (input$choose == "MRT stations") {
        mrt <- read.csv("data/mrt_lrt_data.csv")
        mrt <- head(mrt %>% rowwise() %>% mutate(dist = findDist(lng, lat, avg_long, avg_lat)) %>% arrange(dist), 5) %>% mutate(desc = paste(station_name, type))
        mrt.data <- mrt %>% select(lng, lat, desc)
        plotWithDesc(mrt.data)
      } else if (input$choose == "Taxi stands") {
        taxi.stops.data <- read.csv("data/lta-taxi-stop-kml.csv")
        taxi.stops.data <- taxi.stops.data[,1:2]
        taxi.stops.data <- head(taxi.stops.data %>% rowwise() %>% mutate(dist = findDist(lng, lat, avg_long, avg_lat)) %>% arrange(dist), 5)
        plot(taxi.stops.data)
      } else {
        hawker <- read.csv("data/hawker_data_final.csv")
        hawker <- head(hawker %>% rowwise() %>% mutate(dist = findDist(lng, lat, avg_long, avg_lat)) %>% arrange(dist), 5)
        colnames(hawker)[1] <- "name_of_centre"
        hawker.plot <- hawker %>% select(Longitude, Latitude, name_of_centre)
        plotWithDesc(hawker.plot)
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