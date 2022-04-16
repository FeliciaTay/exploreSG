source("bzmap_funcs.R")

# region events ###############################################################
# check if input data is valid, throws errors if not
dataValidation <- function(names, labels, fields, drops){
  if(length(names) != length(labels)) stop("Error: Not all FileName is associated with a Label")
  if(length(names) != length(fields)) stop("Error: Not all FileName is associated with a Field")
  if(!(mode(names) %in% c("character")) || !(mode(labels) %in% c("character")) || 
     !(mode(fields) %in% c("character")) || !(mode(drops) %in% c("character"))) stop("Error: data not characters")
}

sub_updateLocation <- function(input, output, session, context){
  # update map location
  if(!is.null(input$map_zoom)) context$plotzoom = input$map_zoom
  if(!is.null(input$map_center)) {
    if(input$map_center$lng != context$plotlng || input$map_center$lat != context$plotlat){
      context$plotlng = input$map_center$lng
      context$plotlat = input$map_center$lat
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
  }
}

sub_redraw <- function(input, output, session, context){
  context$map = getSGLeafletMap(context) %>% 
    setView(lng = context$plotlng, lat = context$plotlat, zoom = context$plotzoom)
  if(!is.null(context$checked)){
    for (i in 1:context$length) {
      if (context$name[i] %in% context$checked){
        popups = as.data.frame(context$sdata[[i]][ , context$popupfields[i]])[[1]]
        context$map = addCircleMarkers(context$map, data = context$sdata[[i]], 
          color = context$colors[i], label = popups, clusterOptions = markerClusterOptions())
      }
    }
  }
  output$map <- renderLeaflet({context$map})
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
  if(match$name == "No Result") match[1:nrow(match),2:ncol(match)] = NA
  else{
    context$plotlat = match$lat
    context$plotlng = match$lng
    context$curr_loc <- match
    leafletProxy("map") %>% setView(context$plotlng, context$plotlat, context$plotzoom)
  }
  output$tableSelected <- renderTable(t(match), rownames = TRUE, colnames = FALSE)
  sub_updateLocation(input, output, session, context)
}

observable_chkChange <- function(input, output, session, context){
  x <- input$chkData
  context$checked = x
  sub_redraw(input, output, session, context)
}

observable_sliChange <- function(input, output, session, context){
  x <- input$sliDistance
  context$percentage = x
  # update sdata only since location have not changed
  for(i in 1:context$length){
    sdatlen = context$percentage * nrow(context$data[[i]]) / 100
    sdat = context$data[[i]][1:sdatlen, ]
    coordinates(sdat) = context$geonames
    context$sdata[[i]] = sdat
  }
  sub_redraw(input, output, session, context)
}

observable_mapClickd <- function(input, output, session, context){
  click <- input$map_marker_click
  if(is.null(click)) return(NULL)
  match = NULL
  for (i in 1:context$length) {
    dat = context$data[[i]] %>% mutate(plg = click$lng, plt = click$lat) %>% 
      mutate(dist = geogVecDist(lng, lat, plg, plt)) %>% arrange(dist)
    fst = head(dat, 1)
    if(is.null(match)) match = fst
    else if (match$dist > fst$dist) match = fst
  }
  match = match[ ,!(names(match) %in% context$drops)]
  context$activeInfo = match
  updateSelectInput(session, "selDetail", "See what you have selected", names(match))
}

observable_drpDetail <- function(input, output, session, context){
  sel <- input$selDetail
  if(is.null(context$activeInfo)) return(NULL)
  tab = context$activeInfo[ , sel]
  output$tableNear <- renderTable(tab, rownames = FALSE, colnames = TRUE)
}

# End of File