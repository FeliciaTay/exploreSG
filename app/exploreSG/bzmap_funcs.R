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
# region datascripts ##########################################################
source("datascript/weather.R")
source("datascript/restaurants.R")

# region functions ############################################################
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
initDataContext <- function(files, labels, fields){
  # new environment
  len = length(files)
  context = new.env(parent = emptyenv())
  # read tables
  context$data = list()
  context$name = labels
  for(i in 1:len) context$data[[i]] = read.csv(files[i])
  # get webdata
  context$data.wea = getWeatherForecast()
  # compute LocSet
  tmploc = data.frame(matrix(ncol = 2, nrow = len))
  geonames = c("lng", "lat")
  names(tmploc) = geonames
  for(i in 1:len){
    ss = subset(context$data[[i]], select = geonames)
    tmploc[i, ] = ss[i, ]
  }
  tmploc = distinct(tmploc)
  coordinates(tmploc) = geonames
  context$extent = extent(tmploc)
  # convert to spacial data
  for(i in 1:len) coordinates(context$data[[i]]) = geonames
  # generate color pallette
  context$colors = generatePallette(len)
  # plot map
  context$plotlat = 1.296321
  context$plotlng = 103.776718
  context$plotzoom = 12
  context$popupfields = fields
  context$map = getSGLeafletMap(context)
  return(context)
}

# end of file