source("nearby.R")
source("tourists.R")
source("parks.R")
source("info.R")

# nearby- context #####################################################################
files = c("data/hawkers.csv", "data/hotels.csv", "data/mrts.csv", "data/restaurants.csv", "data/taxis.csv", "data/tourism.csv")
labels = c("Hawker Centres", "Hotels", "MRT / LRTs", "Restaurants", "Taxi Stands", "Tourism Attractions")
fields = c("name_of_centre", "Name", "station_name", "name", "TYPE_CD_DE", "Name")
drops = c("X", "x", "y", "dist", "plg", "plt", "lng", "lat", "success", "href", 
          "altitudeMode", "X_ADDR", "Y_ADDR", "NAME2", "FMEL_UPD_D", "SHAPE", 
          "Description3", "DESCRIPTION2", "ADDRESSPOSTALCODE", "URL.Path", 
          "INC_CRC", "Image.Text", "Image.By", "Menus", "TYPE_CD", "description")

# UI ##########################################################################
shinyApp(
  ui = fluidPage(
    titlePanel(title = htmltools::span(img(src = "icon.png",height=50), "ExploreSG"), windowTitle = 'ExploreSG'),
    tabsetPanel(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
      ),
      tabPanel("What's nearby?", nearby_UI("nearby", files, labels, fields, drops)),
      tabPanel("Tourist attractions", tourists_UI("tourists")),
      tabPanel("Parks and Nature Reserves", parks_UI("parks")),
      tabPanel("Information about our app!", info_UI("info"))
    )
  ),
  server = function(input, output, session) {
    nearby_server("nearby", files, labels, fields, drops)
    
    tourists_server("tourists")
    
    parks_server("parks")
    
    info_server("info")
  }
)