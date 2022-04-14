source("lib.R")
source("datascript/weather.R")
source("datascript/restaurants.R")

bRegisterGGmap <- function(name) {
  strkey = switch (name,
    "lbz" = 'AIzaSyAPypfkt7AxQ7o2r96Xw1-HJzXcBEnh-Nc'
  )
  if(is.null(strkey)) return(FALSE)
  else{
    ggmap::register_google(key=strkey)
    return(TRUE)
  }
}

cComment <- function(text) {
  return(NULL)
}

dReadData <- function(folder, extension) {
  filenames = list.files(folder, pattern = extension, full.names = TRUE)
  ldf = lapply(filenames, read.csv, check.names = FALSE, fileEncoding="UTF-8-BOM")
  d1 = ldf[[1]]
  if(length(ldf) < 2) return(ldf[[1]])
  d1$f_cat = 1
  for (i in 2:length(ldf)) {
    d2 = ldf[[i]]
    d1 = full_join(d1, d2, by = c("lng" = "lng", "lat" = "lat"))
  }
  return(d1)
}

plotMap <- function(data, appIcon, withDesc = FALSE) {
  test_map = leaflet() %>% addTiles() 
  for (i in 1:nrow(data)) {
    curr = data[i,]
    long = as.numeric(curr[1])
    lati = as.numeric(curr[2])
    if(!withDesc) test_map = addAwesomeMarkers(test_map, lat=lati,lng=long, icon=appIcon)
    else{
      desc = as.character(curr[3])
      test_map = addAwesomeMarkers(test_map, lat=lati,lng=long, popup=desc, icon=appIcon)
    }
  }
  return(test_map)
}

findAverageCoordOfSearch <- function(input) {
  url = "https://developers.onemap.sg/commonapi/search"
  qry = list('searchVal' = as.character(input$loc_input), 'returnGeom' = 'Y', 'getAddrDetails' = 'N', 'pageNum' = '1')
  res = GET(url, query = qry, verbose())
  lst = content(res)$results
  rtn = NULL
  # contents: SEARCHVAL, X, Y, LATITUDE, LONGTITUDE, LONGTITUDE
  if (length(lst) == 0) 
    rtn = c(103.851959, 1.290270)
  else if (length(lst) < 3) # less than 3 search results
    rtn = c(as.numeric(lst[[1]]$LONGTITUDE), as.numeric(lst$results[[1]]$LATITUDE))
  else {
    avg_lng = mean(as.numeric(list[[1]]$LONGTITUDE), as.numeric(list[[2]]$LONGTITUDE), as.numeric(list[[3]]$LONGTITUDE))
    avg_lat = mean(as.numeric(list[[1]]$LATITUDE), as.numeric(list[[2]]$LATITUDE), as.numeric(list[[3]]$LATITUDE))
    rtn = c(avg_lng, avg_lat)
  }
  return(rtn)
}


findDist <- function(X1, Y1, X2, Y2) {
  return(distm(c(X1, Y1), c(X2, Y2), fun = distHaversine))
}

