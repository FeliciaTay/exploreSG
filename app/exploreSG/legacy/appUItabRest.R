source("funcs.R")
source("datascript/restaurants.R")
restaurantLinkTable = getRestaurantLinks()

getUITabHtml <- function(input){
  inputvar = input$restaurant
  lut = restaurantLinkTable$href
  names(lut) = restaurantLinkTable$name
  href = lut[inputvar]
  row = getRowFromLink(href)
  types = names(row)
  rtn = ""
  for(i in 1:length(types)){
    rtn = paste(rtn, "<li><h2>", types[i], "</h2><p>", row[i], "</p></li>", sep = "")
  }
  return(rtn)
}

refreshUITabHtml <- function(input){
  restaurantLinkTable <- getRestaurantLinks()
  return(getUITabHtml(input))
}

#usage:
#output$tabPanelHtml <- renderUI({HTML(getUITabHtml(input))})