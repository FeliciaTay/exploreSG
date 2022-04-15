library(shiny)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(httr)
library(jsonlite)
library(rvest)
library(xml2)
library(tidyr)
# region functions ############################################################
# find a location [name, x, y, lng, lat] based on a string
searchLoc <- function(locname = '') {
  url = "https://developers.onemap.sg/commonapi/search"
  qry = list('searchVal' = locname, 'returnGeom' = 'Y', 'getAddrDetails' = 'N', 'pageNum' = '1')
  res = GET(url, query = qry, verbose())
  lst = content(res)$results
  # contents: SEARCHVAL, X, Y, LATITUDE, LONGITUDE, LONGTITUDE
  default_row = c("No Result", 21700.880785, 30966.194690, 1.296321, 103.776718)
  names(default_row) = c("name", "x", "y", "lng", "lat")
  df = NULL
  if(length(lst) == 0){
    df = bind_rows(default_row)
  } else {
    df = bind_rows(lapply(lst, as.data.frame.list))
    df = subset(df, select = -c(LONGTITUDE))
    names(df) = names(default_row)
    df$x = as.numeric(df$x)
    df$y = as.numeric(df$y)
    df$lng = as.numeric(df$lng)
    df$lat = as.numeric(df$lat)
  }
  return(combineSameNameEntries(df))
}

# collapse multiple locations into one by averaing numeric fields
combineSameNameEntries <- function(df){
  if(nrow(df) > 1){
    df = df %>% group_by(name) %>% 
      summarise(x = mean(x), y = mean(y), lng = mean(lng), lat = mean(lat))
  }
  return(df)
}

# generate html text to wrap around a named row
namedRowToHtml <- function(namedRow){
  types = names(namedRow)
  row = namedRow
  rtn = ""
  for(i in 1:length(types)){
    rtn = paste(rtn, "<li><h4>", types[i], "</h4><p>", row[i], "</p></li>", sep = "")
  }
  return(rtn)
}

# get the string representation of the current datetime
dateTimeStr <- function(){
  dt = Sys.time()
  d = format(dt, format="%Y-%m-%d")
  t = format(dt, format="%H:%M:%S")
  t = gsub(":", "%3A", t)
  str = paste(d, t, sep = "T", collapse = NULL)
  return(str)
}

# gets weather [area, forecast, name, lat, lng] data live
getWeatherForecast <- function(){
  url_base = "https://api.data.gov.sg/v1/environment/2-hour-weather-forecast?date_time="
  weather_url = paste(url_base, dateTimeStr(), sep = "", collapse = NULL)
  response = GET(weather_url)
  responseStr = content(response, "text", encoding = "UTF-8")
  responseJson = fromJSON(responseStr, flatten = TRUE)
  forecasts = as.data.frame(responseJson$items$forecasts)
  metadata = as.data.frame(responseJson$area_metadata)
  merged = merge.data.frame(forecasts, metadata, by.x = "area", by.y = "name")
  names(merged) = c("area","forecast", "lat","lng")
  merged = relocate(merged, where(is.numeric), .before = where(is.character))
  return(merged)
}

# reads a URL safely
safeReadUrl = function(url) {
  myurl = url(url,"rb") # url format
  page = read_html(myurl)
  close(myurl) #close connection
  return(page)
}

# scans through a page and return a dataframe of node texts using css selector
getTextDfFromCss = function(page, selector){
  nodes = html_nodes(page, selector)
  df = html_text(nodes) %>% as.data.frame()
  return(df)
}

# scans through a page and return a dataframe of node attributes using css selector
getAttrDfFromCss = function(page, selector, attribute){
  nodes = html_nodes(page, selector)
  df = html_attr(nodes, attribute) %>% as.data.frame()
  return(df)
}

# crawls a restaurant [name, href] list from Chope live
getRestaurantLinks = function(url = "https://www.chope.co/singapore-restaurants/list_of_restaurants?source=chope.com.sg&lang=en_US"){
  page = safeReadUrl(url)
  names = getTextDfFromCss(page, ".az-result>ul>li>a")
  hrefs = getAttrDfFromCss(page, ".az-result>ul>li>a", "href")
  merged = cbind(names, hrefs)
  names(merged) = c("name", "href")
  return(merged)
}

# crawls the information (named row) for a restaurant by a Chope address live
getRestaurantInfoFromLink <- function(href){
  page = safeReadUrl(href)
  types = getTextDfFromCss(page, ".type")
  conts = getTextDfFromCss(page, "li>span~p")
  miscs = getTextDfFromCss(page, "h6")
  texts = getTextDfFromCss(page, ".section>p:last-child")
  while(nrow(types)>nrow(conts)){# case of empty strings
    conts[nrow(conts) + 1, ] = c(NA)
    print(nrow(conts))
  }
  while(nrow(miscs)>nrow(texts)){# case of empty strings
    texts[nrow(texts) + 1, ] = c(NA)
    print(nrow(texts))
  }
  types = cbind(t(types), t(miscs))
  conts = cbind(t(conts), t(texts))
  datum = as.data.frame(conts)
  names(datum) = types
  return(datum)
}

# creates an environment object as the data context to be used by this module
initDataContext <- function(){
  # new environment
  context <- new.env(parent = emptyenv())
  # read tables
  context$data.haw <- read.csv("data/hawkers.csv")
  context$data.hot <- read.csv("data/hotels.csv")
  context$data.mrt <- read.csv("data/mrts.csv")
  context$data.tax <- read.csv("data/taxis.csv")
  context$data.tou <- read.csv("data/tourism.csv")
  # get webdata
  context$data.res <- getRestaurantLinks()
  context$data.wea <- getWeatherForecast()
  # init leaflet map
  context$map <- leaflet() %>% addTiles()
  return(context)
}

# find the distance from two pairs of lng-lat values along earth's curvature
geogDist <- function(lng, lat, lng2, lat2){
  return(distm(c(lng, lat), c(lng2, lat2), fun = distHaversine))
}