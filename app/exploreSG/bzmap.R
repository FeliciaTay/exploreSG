source("bzmap_funcs.R")
# region subs (no return value) ###############################################
# edits a leaflet map in plade
editMap <- function(context, data, appIcon, withDesc = FALSE) {
  for (i in 1:nrow(data)) {
    curr = data[i,]
    long = as.numeric(curr[1])
    lati = as.numeric(curr[2])
    if(!withDesc) context$map = addAwesomeMarkers(context$map, lat=lati,lng=long, icon=appIcon)
    else{
      desc = as.character(curr[3])
      context$map = addAwesomeMarkers(context$map, lat=lati,lng=long, popup=desc, icon=appIcon)
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
}

observable_btnRefresh <- function(input, output, session, context){
  if (!exists(curr_loc, envir = context, inherits = FALSE)){
    output$map <- renderLeaflet({context$map})
    return(NULL)
  }
  # prepare variables
  clng = context$curr_loc$lng
  clat = context$curr_loc$lat
  checkedVec = context$checked
  ico = makeAwesomeIcon(text=fa("circle"), iconColor="white", markerColor="blue")
  # reset context$map
  context$map <- leaflet() %>% addTiles()
  if ("Hawker Centres" %in% checkedVec) {
    haw = context$data.haw
    haw = haw %>% select(lng, lat, name_of_centre) %>% mutate(dist = geogDist(lng, lat, clng, clat)) %>% arrange(dist)
    haw = head(haw, 5) 
    editMap(haw, ico, withDesc = TRUE)
  }
  if ("Hotels" %in% checkedVec) {
    hot = context$data.hot
    hot = hot %>% select(lng, lat, Name, TOTALROOMS, ADDRESS) %>% 
      mutate(dist = geogDist(lng, lat, clng, clat), 
             desc = paste0("Name: ", name, "\nTotal Rooms: ", TOTALROOMS, "\nAddress: ", ADDRESS)) %>% 
      arrange(dist) %>% select(lng, lat, desc)
    hot = head(hot, 5)
    editMap(hot, ico, withDesc = TRUE)
  }
  if ("MRT / LRTs" %in% checkedVec) {
    mrt = context$data.mrt
    mrt = mrt %>% select(lng, lat, station_name, type) %>% 
      mutate(dist = geogDist(lng, lat, clng, clat), 
             desc = paste0("Station Name: ", station_name, "\nStation Type: ", type)) %>% 
      arrange(dist) %>% select(lng, lat, desc)
    mrt = head(mrt, 5)
    editMap(mrt, ico, withDesc = TRUE)
  }
  if ("Taxi Stands" %in% checkedVec) {
    tax = context$data.tax
    tax = tax %>% select(lng, lat) %>% mutate(dist = geogDist(lng, lat, clng, clat)) %>% arrange(dist)
    tax = head(tax, 5)
    editMap(tax, ico, withDesc = FALSE)
  }
  if ("Restaurants" %in% checkedVec) {
    print("Restaurants Not Implemented")
  }
  if ("Tourism Info" %in% checkedVec) {
    tou = context$data.tou
    tou = tou %>% select(lng, lat, Name, description) %>% 
      mutate(dist = geogDist(lng, lat, clng, clat), 
             desc = paste0("Name: ", Name, "\nDescription: ", description)) %>% 
      arrange(dist) %>% select(lng, lat, desc)
    tou = head(tou, 5)
    editMap(tou, ico, withDesc = TRUE)
  }
  if ("Weather" %in% checkedVec) {
    wea = context$data.wea
    wea = wea %>% select(lng, lat, area, forecast) %>%
      mutate(dist = geogDist(lng, lat, clng, clat), 
             desc = paste0("Area: ", area, "\nForecast: ", forecast)) %>% 
      arrange(dist) %>% select(lng, lat, desc)
    wea = head(wea, 5)
    editMap(wea, ico, withDesc = TRUE)
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
            c("Hawker Centres", "Hotels", "MRT / LRTs", "Restaurants", "Taxi Stands", "Tourism Info", "Weather")),
          actionButton(ns("btnRefresh"), "Refresh Map")
        )
      ),
      mainPanel(
        leafletOutput(outputId = "map"),
        htmlOutput(outputId = "html")
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
      #reactive changes
      observe({observable_txtSearch(input, output, session, context)})
      observe({observable_drpSelect(input, output, session, context)})
      observe({observable_chkChange(input, output, session, context)})
      #triggers only once for the button for initiation purposes
      observe({observable_btnRefresh(input, output, session, context)})
      #event driven changes
      observeEvent(input$btnRefresh,
                   {observable_btnRefresh(input, output, session, context)})
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