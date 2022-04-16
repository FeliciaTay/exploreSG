source("bzmap.R")

# context #####################################################################
files = c("data/hawkers.csv", "data/hotels.csv", "data/mrts.csv", "data/restaurants.csv", "data/taxis.csv", "data/tourism.csv")
labels = c("Hawker Centres", "Hotels", "MRT / LRTs", "Restaurants", "Taxi Stands", "Tourism Info")
fields = c("name_of_centre", "Name", "station_name", "name", "TYPE_CD_DE", "Name")
drops = c("X", "x", "y", "dist", "plg", "plt", "lng", "lat", "success")

# UI ##########################################################################
shinyApp(
  ui = fluidPage(
    bzmap_UI("bzmap", files, labels, fields, drops)
  ),
  server = function(input, output, session) {
    bzmap_server("bzmap", files, labels, fields, drops)
  }
)