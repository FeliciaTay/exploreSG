source("nearby_funcs.R")

# Event: Triggered when Blob is clicked #######################################
observable_mapClickd <- function(input, output, session, context){
  click <- input$map_marker_click # receives trigger
  if(is.null(click)) return(NULL) # sanity check
  match = NULL                    # minimize distance manually
  for (i in 1:context$length) {
    dat = context$data[[i]] %>% mutate(plg = click$lng, plt = click$lat) %>% 
      mutate(dist = geogVecDist(lng, lat, plg, plt)) %>% arrange(dist)
    fst = head(dat, 1)
    if(is.null(match)) match = fst
    else if (match$dist > fst$dist) match = fst
  }
  match = match[ ,!(names(match) %in% context$drops)] # takes useful columns
  context$activeInfo = match                          # saves as a global variable
  updateSelectInput(session, "selDetail", "Select on a location marker to see more information!", names(match))
}

# Event: Triggered when Input Text is Changed #################################
observable_txtSearch <- function(input, output, session, context){
  x <- input$txtSearch            # receives trigger
  context$lstLoc <- searchLoc(x)  # reset the global variable to a new search result
  name = context$lstLoc$name      # fetches names
  updateSelectInput(session, "drpSelect",
                    label = paste0("Available options based on \"", x, "\":"),
                    choices = name
  )                               # sets the select input options
}

# Event: Triggered when Select Combo Box is Changed ###########################
observable_drpSelect <- function(input, output, session, context){
  x <- input$drpSelect    # receives trigger
  lst = context$lstLoc    # fetches list from previous search
  match = filter(lst, name == UQ(x))  # obsolete: treat x as a literal
  if(match$name == "No Result") match[1:nrow(match),2:ncol(match)] = NA
  else{                   # sets NA except names
    context$plotlat = match$lat       # change global position
    context$plotlng = match$lng
    context$curr_loc <- match
    if(!is.null(input$map_zoom)) context$plotzoom = input$map_zoom # save zoom
    leafletProxy("map") %>% setView(context$plotlng, context$plotlat, context$plotzoom)
  }                             # moves the map
  output$tableSelected <- renderTable(t(match), rownames = TRUE, colnames = FALSE)
  sub_updateData(input, output, session, context) # save the changed data
}

# Event: Triggered when Different Data are Selected ###########################
observable_chkChange <- function(input, output, session, context){
  x <- input$chkData     # receives trigger
  context$checked = x    # sets global variable
  sub_updateLocation(input, output, session, context)
  sub_redraw(input, output, session, context) # redraw graph due to changed blobs
}

# Event: Triggered when Percentage Scale is Changed ###########################
observable_sliChange <- function(input, output, session, context){
  x <- input$sliDistance    # receives trigger
  context$percentage = x    # sets global variable
  # update sdata only since location have not changed
  for(i in 1:context$length){
    sdatlen = context$percentage * nrow(context$data[[i]]) / 100
    sdat = context$data[[i]][1:sdatlen, ]
    coordinates(sdat) = context$geonames
    context$sdata[[i]] = sdat
  }               # must redraw to show changes on number of blobs
  sub_updateLocation(input, output, session, context)
  sub_redraw(input, output, session, context)
}

# Event: Triggered when Attribute Dropbox is Changed ##########################
observable_drpDetail <- function(input, output, session, context){
  sel <- input$selDetail     # receives trigger
  if(is.null(context$activeInfo)) return(NULL) # sanity check
  tab = context$activeInfo[ , sel]   # retrieves global variable
  if(is.null(tab)) tab = "Unknown"
  output$tableNear <- renderTable(tab, rownames = FALSE, colnames = TRUE)
}

# Procedure: Re-calculates the data and sdata globally ########################
sub_updateData <- function(input, output, session, context){
  # update weather forecast
  context$data.wea = context$data.wea %>% mutate(plg = context$plotlng, plt = context$plotlat) %>% 
    mutate(dist = geogVecDist(lng, lat, plg, plt)) %>% arrange(dist)
  output$txtWeather <- renderText({paste0("The weather near you is: ", head(context$data.wea, 1)$forecast)})
  # update data and sdata since location changed
  lst = vector(mode = "list", length = context$length)
  for (i in 1:context$length) {
    context$data[[i]] = context$data[[i]] %>% mutate(plg = context$plotlng, plt = context$plotlat) %>% 
      mutate(dist = geogVecDist(lng, lat, plg, plt)) %>% arrange(dist)
    sdatlen = context$percentage * nrow(context$data[[i]]) / 100
    sdat = context$data[[i]][1:sdatlen, ]
    coordinates(sdat) = context$geonames
    context$sdata[[i]] = sdat
  }
}

# Procedure: Retrieves the current location and stores as global variable #####
sub_updateLocation <- function(input, output, session, context){
  # update map location
  if(!is.null(input$map_zoom)) context$plotzoom = input$map_zoom
  if(!is.null(input$map_center)) {
    if(input$map_center$lng != context$plotlng || input$map_center$lat != context$plotlat){
      context$plotlng = input$map_center$lng
      context$plotlat = input$map_center$lat
    }
  }
}

# Procedure: Renders a new leaflet map ########################################
sub_redraw <- function(input, output, session, context){
  context$map = getSGLeafletMap(context)
  if(!is.null(context$checked)){
    for (i in 1:context$length) {
      if (context$name[i] %in% context$checked){
        popups = as.data.frame(context$sdata[[i]][ , context$popupfields[i]])[[1]]
        context$map = addCircleMarkers(context$map, data = context$sdata[[i]], 
          color = context$colors[i], label = popups, clusterOptions = markerClusterOptions())
      }
    }
  } else { # clear all markers
    clearMarkerClusters(context$map)
  }
  output$map <- renderLeaflet({context$map})
}

# End of File