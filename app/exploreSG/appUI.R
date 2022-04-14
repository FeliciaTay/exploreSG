source("lib.R")
source("appUItabs.R")

appUI <- shinyUI({
  fluidPage(
    tabsetPanel(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "dark_mode.css"),
        tags$style("#weather{font-weight: bold; font-family: 'Yusei Magic', sans-serif;}"),
        tags$style("#choose{font-size: 120%;}"),
        tags$style("#title{font-size: 150%;}")
      ),
      tabNearby,
      tabAttractions,
      tabLiveData,
      tabInfo
    )
  )
})

