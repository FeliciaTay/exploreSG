library(rvest)
library(xml2)
library(dplyr)
library(tidyr)

safeReadUrl = function(url) {
  myurl = url(url,"rb") # url format
  page = read_html(myurl)
  close(myurl) #close connection
  return(page)
}

getTextDfFromCss = function(page, selector){
  nodes = html_nodes(page, selector)
  df = html_text(nodes) %>% as.data.frame()
  return(df)
}

getAttrDfFromCss = function(page, selector, attribute){
  nodes = html_nodes(page, selector)
  df = html_attr(nodes, attribute) %>% as.data.frame()
  return(df)
}

getRestaurantLinks = function(url = "https://www.chope.co/singapore-restaurants/list_of_restaurants?source=chope.com.sg&lang=en_US"){
  page = safeReadUrl(url)
  names = getTextDfFromCss(page, ".az-result>ul>li>a")
  hrefs = getAttrDfFromCss(page, ".az-result>ul>li>a", "href")
  merged = cbind(names, hrefs)
  names(merged) = c("name", "href")
  return(merged)
}

# exposed function for getting single entry
getRowFromLink <- function(href){
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

# exposed function for getting the entire list
getRestaurantsData <- function(){
  refs = getRestaurantLinks(url_restaurant)
  dats = lapply(refs$href, getRowFromLink)
  return(dats)
}

#usage 1:
#restaurantsLinkTable = getRestaurantLinks(url_restaurant)

#usage 2:
#restaurantRow = getRowFromLink(href = "...")

#usage 3:
#restaurants = getRestaurantsData()