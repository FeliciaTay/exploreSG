source("bzmap.R")

# context #####################################################################
files = c("data/hawkers.csv", "data/hotels.csv", "data/mrts.csv", "data/restaurants.csv", "data/taxis.csv", "data/tourism.csv")
labels = c("Hawker Centres", "Hotels", "MRT / LRTs", "Restaurants", "Taxi Stands", "Tourism Info")
fields = c("name_of_centre", "Name", "station_name", "name", "TYPE_CD_DE", "Name")

# UI ##########################################################################
shinyApp(
  ui = fluidPage(
    bzmap_UI("bzmap", files, labels, fields)
  ),
  server = function(input, output, session) {
    bzmap_server("bzmap", files, labels, fields)
  }
)