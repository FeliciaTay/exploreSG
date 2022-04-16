library(shiny)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(raster)
library(httr)
library(jsonlite)
library(rvest)
library(xml2)
library(dplyr)
library(tidyr)
library(geosphere)
library(sp)
library(rgdal)
library(fontawesome)
source("datascript/weather.R")
source("datascript/restaurants.R")

# region functions ############################################################
# throws errors when data input is illegal
dataValidation <- function(names, labels, fields, drops){
  if(length(names) != length(labels)) stop("Error: Not all FileName is associated with a Label")
  if(length(names) != length(fields)) stop("Error: Not all FileName is associated with a Field")
  if(!(mode(names) %in% c("character")) || !(mode(labels) %in% c("character")) || 
     !(mode(fields) %in% c("character")) || !(mode(drops) %in% c("character"))) stop("Error: data not characters")
}

# creates a leaflet map of SG
getSGLeafletMap <- function(context){
  map = leaflet() %>% addTiles() %>% 
    setView(lng = context$plotlng, lat = context$plotlat, zoom = context$plotzoom) %>% 
    addMouseCoordinates() %>% addHomeButton(ext = context$extent, group = "Home")
  return(map)
}

# find the distance from two pairs of lng-lat values along earth's curvature
geogDist <- function(lng, lat, lng2, lat2){
  return(distm(c(lng, lat), c(lng2, lat2), fun = distGeo))
}

# find the distance from two vector pairs of long-lat values along earth's curvature.
geogVecDist <- function(lng, lat, lng2, lat2){
  vec = numeric(length = length(lng))
  for (i in 1:length(vec)) {
    vec[i] = geogDist(lng[i], lat[i], lng2[i], lat2[i])
  }
  return(vec)
}

# convert hsv color space to rgb color space
hsv2rgb <- function(hue, sat, val){
  c = sat * val
  hp = hue / 60.0
  ccoeff = 1.0 - abs(hp %% 2 - 1.0)
  x = c * ccoeff
  rgb1 = c(0,0,0)
  if(hp < 1)      rgb1 = c(c,x,0)
  else if(hp < 2) rgb1 = c(x,c,0)
  else if(hp < 3) rgb1 = c(0,c,x)
  else if(hp < 4) rgb1 = c(0,x,c)
  else if(hp < 5) rgb1 = c(x,0,c)
  else if(hp < 6) rgb1 = c(c,0,x)
  else            rgb1 = c(0,0,0)
  m = val - c
  r = as.integer((rgb1[1] + m) * 255)
  g = as.integer((rgb1[2] + m) * 255)
  b = as.integer((rgb1[3] + m) * 255)
  return(c(r, g, b))
}

# convert rgb vector[r,g,b] to hex #XXXXXX
rgb2hex <- function(rgb){
  r = as.hexmode(rgb[1])
  g = as.hexmode(rgb[2])
  b = as.hexmode(rgb[3])
  return(paste0("#", r, g, b))
}

# generate color pallette of required length
generatePallette <- function(len){
  seed = runif(1,0,359)
  delta = 360.0 / len
  colors = character(length = len)
  for(i in 1:len){
    hue = (i - 1.0) * delta
    sat = 0.5
    bri = 0.6
    colors[i] = rgb2hex(hsv2rgb(hue, sat, bri))
  }
  return(colors)
}

# creates an environment object as the data context to be used by this module
initDataContext <- function(files, labels, fields, drops){
  # local vars
  len = length(files)
  geonames = c("lng", "lat")
  locs = data.frame(matrix(ncol = 2, nrow = 0))
  names(locs) = geonames
  context = new.env(parent = emptyenv())
  # global vars
  context$length = len
  context$geonames = geonames
  context$plotlat = 1.296321
  context$plotlng = 103.776718
  context$plotzoom = 12
  context$colors = generatePallette(len)
  context$icon = makeAwesomeIcon(text=fa("circle"), iconColor="white", markerColor="blue")
  context$map = getSGLeafletMap(context)
  context$percentage = 100
  context$name = labels
  context$popupfields = fields
  context$data = list()
  context$sdata = list()
  context$activeInfo = NULL
  context$drops = drops
  for(i in 1:len) {
    dat = read.csv(files[i]) # original
    dat = dat %>% mutate(plg = context$plotlng, plt = context$plotlat) %>% 
      mutate(dist = geogVecDist(lng, lat, plg, plt)) %>% arrange(dist)
    context$data[[i]] = dat  # with dist
    context$sdata[[i]] = dat # spacial
    locs = rbind(locs, subset(dat, select = geonames))
  } 
  context$data.wea = getWeatherForecast() %>% mutate(plg = context$plotlng, plt = context$plotlat) %>%
    mutate(dist = geogVecDist(lng, lat, plg, plt)) %>% arrange(dist)
  locs = distinct(locs)
  coordinates(locs) = geonames
  context$extent = extent(locs)
  for(i in 1:len) coordinates(context$sdata[[i]]) = geonames
  return(context)
}

# end of file