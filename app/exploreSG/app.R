source("bzmap.R")

# context #####################################################################
files = c("data/hawkers.csv", "data/hotels.csv", "data/mrts.csv", "data/restaurants.csv", "data/taxis.csv", "data/tourism.csv")
labels = c("Hawker Centres", "Hotels", "MRT / LRTs", "Restaurants", "Taxi Stands", "Tourism Info")
fields = c("name_of_centre", "Name", "station_name", "name", "TYPE_CD_DE", "Name")
drops = c("X", "x", "y", "dist", "plg", "plt", "lng", "lat", "success")

# UI ##########################################################################
shinyApp(
  ui = fluidPage(
    tabsetPanel(
      tabPanel("bztab", bzmap_UI("bzmap", files, labels, fields, drops)),
      tabPanel("fttab", htmlOutput("html1")),
      tabPanel("About & Help", htmlOutput("html2"))
    )
  ),
  server = function(input, output, session) {
    bzmap_server("bzmap", files, labels, fields, drops)
    output$html1 <- renderUI({HTML("<h1>TAB</h1><span>test span</span><p>this is help doc</p>")})
    output$html2 <- renderUI({HTML("<h1>About Us</h1><span>We are human</span><p>We are great!</p>")})
  }
)