library(fontawesome)
source("bzmap_funcs.R")
# region subs (no return value) ###############################################
# check if input data is valid, throws errors if not
dataValidation <- function(names, labels, fields){
  if(length(names) != length(labels)) stop("Error: Not all FileName is associated with a Label")
  if(length(names) != length(fields)) stop("Error: Not all FileName is associated with a Field")
  if(!(mode(names) %in% c("character")) || !(mode(labels) %in% c("character")) || !(mode(fields) %in% c("character"))) stop("Error: data not characters")
}

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
  else{
    context$plotlat = match$lat
    context$plotlng = match$lng
    if(!is.null(input$map_zoom)) context$plotzoom = input$map_zoom
    leafletProxy("map") %>% setView(context$plotlng, context$plotlat, context$plotzoom)
  }
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
  for (i in 1:length(context$name)) {
    if (context$name[i] %in% checkedVec){
      popups = as.data.frame(context$data[[i]][ , context$popupfields[i]])[[1]]
      context$map = addCircleMarkers(context$map, data = context$data[[i]], 
        color = context$colors[i], label = popups)
    }
  }
  if ("Weather" %in% checkedVec) {
    context$map = addCircleMarkers(context$map, data = context$data.wea)
  }
  output$map <- renderLeaflet({context$map})
}

# region ui ###################################################################
bzmap_UI <- function(id, vecDataFileNames, vecDataLabels, vecPopupFields){
  dataValidation(vecDataFileNames, vecDataLabels, vecPopupFields)
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
          checkboxGroupInput(ns("chkData"), "Which data sources to show?", vecDataLabels)
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
bzmap_server <- function(id, vecDataFileNames, vecDataLabels, vecPopupFields){
  dataValidation(vecDataFileNames, vecDataLabels, vecPopupFields)
  context <- initDataContext(vecDataFileNames, vecDataLabels, vecPopupFields)
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

# use case ####################################################################
files = c("data/hawkers.csv", "data/hotels.csv", "data/mrts.csv", "data/restaurants.csv", "data/taxis.csv", "data/tourism.csv")
labels = c("Hawker Centres", "Hotels", "MRT / LRTs", "Restaurants", "Taxi Stands", "Tourism Info")
fields = c("name_of_centre", "Name", "station_name", "name", "TYPE_CD_DE", "Name")
shinyApp(
  ui = fluidPage(
    bzmap_UI("bzmap", files, labels, fields)
  ),
  server = function(input, output, session) {
    bzmap_server("bzmap", files, labels, fields)
  }
)