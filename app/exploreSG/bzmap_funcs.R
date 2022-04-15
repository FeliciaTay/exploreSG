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
    setView(lng = 103.776718, lat = 1.296321, zoom = 12) %>% 
    addMouseCoordinates() %>% addHomeButton(ext = context$extent)
  return(map)
}

# find the distance from two pairs of lng-lat values along earth's curvature
geogDist <- function(lng, lat, lng2, lat2){
  return(distm(c(lng, lat), c(lng2, lat2), fun = distGeo))
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
  context$data.res <- read.csv("data/restaurants.csv")
  # get webdata
  context$data.wea <- getWeatherForecast()
  # compute LocSet
  tmploc = rbind.data.frame(
    subset(context$data.haw, select = c(lng, lat)),
    subset(context$data.hot, select = c(lng, lat)),
    subset(context$data.mrt, select = c(lng, lat)),
    subset(context$data.tax, select = c(lng, lat)),
    subset(context$data.tou, select = c(lng, lat)),
    subset(context$data.res, select = c(lng, lat)),
    subset(context$data.wea, select = c(lng, lat))
  )
  tmploc = distinct(tmploc)
  coordinates(tmploc) = c("lng", "lat")
  context$extent = extent(tmploc)
  # convert to spacial data
  coordinates(context$data.haw) = c("lng", "lat")
  coordinates(context$data.hot) = c("lng", "lat")
  coordinates(context$data.mrt) = c("lng", "lat")
  coordinates(context$data.tax) = c("lng", "lat")
  coordinates(context$data.tou) = c("lng", "lat")
  coordinates(context$data.res) = c("lng", "lat")
  coordinates(context$data.wea) = c("lng", "lat")
  # plot map
  context$map <- getSGLeafletMap(context)
  return(context)
}

# end of file