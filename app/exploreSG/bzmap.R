library(fontawesome)
source("bzmap_funcs.R")
# region subs (no return value) ###############################################

# edits a leaflet map in plade
editMap <- function(context, data, appIcon, withDesc = FALSE) {
  for (i in 1:nrow(data)) {
    curr = data[i,]
    if(is.numeric(curr[4])){
      long = as.numeric(curr[1])
      lati = as.numeric(curr[2])
      if(!withDesc) context$map = addAwesomeMarkers(context$map, lat=lati,lng=long, icon=appIcon)
      else{
        desc = as.character(curr[3])
        context$map = addAwesomeMarkers(context$map, lat=lati,lng=long, popup=desc, icon=appIcon)
      }
    }
  }
}

observable_txtSearch <- function(input, output, session, context){
  x <- input$txtSearch
  context$lstLoc <- searchLoc(x)
  name = context$lstLoc$name # breaks reference
  updateSelectInput(session, "drpSelect",
                    label = paste0("Available options based on \"", x, "\":"),
                    choices = name
  )
}

observable_drpSelect <- function(input, output, session, context){
  x <- input$drpSelect
  lst = context$lstLoc
  match = filter(lst, name == UQ(x))
  context$curr_loc <- match
  if(match$name == "No Result") match[1:nrow(match),2:ncol(match)] = NA
  output$tableSelected <- renderTable(t(match), rownames = TRUE, colnames = FALSE)
}

observable_chkChange <- function(input, output, session, context){
  context$checked <- input$chkData
  # reset map
  context$map <- getSGLeafletMap(context)
  if (!exists("curr_loc", envir = context, inherits = FALSE)){
    output$map <- renderLeaflet({context$map})
    return(NULL)
  }
  checkedVec = context$checked
  if (is.null(checkedVec)){
    output$map <- renderLeaflet({context$map})
    return(NULL)
  }
  # prepare variables
  clng = context$curr_loc$lng
  clat = context$curr_loc$lat
  context$map %>% setView(lng = clng, lat = clat, zoom = 12)
  ico = makeAwesomeIcon(text=fa("circle"), iconColor="white", markerColor="blue")
  if ("Hawker Centres" %in% checkedVec) {
    context$map = addCircleMarkers(context$map, data = context$data.res)
  }
  if ("Hotels" %in% checkedVec) {
    context$map = addCircleMarkers(context$map, data = context$data.res)
  }
  if ("MRT / LRTs" %in% checkedVec) {
    context$map = addCircleMarkers(context$map, data = context$data.res)
  }
  if ("Taxi Stands" %in% checkedVec) {
    context$map = addCircleMarkers(context$map, data = context$data.res)
  }
  if ("Restaurants" %in% checkedVec) {
    context$map = addCircleMarkers(context$map, data = context$data.res)
  }
  if ("Tourism Info" %in% checkedVec) {
    context$map = addCircleMarkers(context$map, data = context$data.res)
  }
  if ("Weather" %in% checkedVec) {
    context$map = addCircleMarkers(context$map, data = context$data.res)
  }
  output$map <- renderLeaflet({context$map})
}

# region ui ###################################################################
bzmap_UI <- function(id){
  ns <- NS(id) # generate namespace function
  tagList( # UI elements
    titlePanel("Welcome to SG!"),
    sidebarLayout(
      sidebarPanel(
        textInput(ns("txtSearch"), "Type here to search your location:"),
        selectInput(ns("drpSelect"), "Available options based on \"\":",searchLoc()$name),
        textOutput(ns("txtTableSelectedTitle")),
        wellPanel(
          tableOutput(ns("tableSelected")),
        ),
        wellPanel(
          checkboxGroupInput(ns("chkData"), "Which data sources to show?", 
            c("Hawker Centres", "Hotels", "MRT / LRTs", "Restaurants", "Taxi Stands", "Tourism Info", "Weather"))
        )
      ),
      mainPanel(
        leafletOutput(outputId = ns("map")),
        htmlOutput(outputId = ns("html"))
      )
    )
  )
}

# region server ###############################################################
bzmap_server <- function(id){
  context <- initDataContext()
  moduleServer(
    id,
    function(input, output, session){ # logic
      # reactive controls
      observe({observable_txtSearch(input, output, session, context)})
      observe({observable_drpSelect(input, output, session, context)})
      observe({observable_chkChange(input, output, session, context)})
      # normal on controls
      output$txtTableSelectedTitle <- renderText({"About this location:"})
    }
  )
}

shinyApp(
  ui = fluidPage(
    bzmap_UI("bzmap")
  ),
  server = function(input, output, session) {
    bzmap_server("bzmap")
  }
)