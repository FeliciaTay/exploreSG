library(rvest)
library(xml2)
library(dplyr)
library(tidyr)
library(stringr)

# search for location from onemap API and gets results as data frame
searchLoc = function(locname = '') {
  url = "https://developers.onemap.sg/commonapi/search"
  qry = list('searchVal' = locname, 'returnGeom' = 'Y', 'getAddrDetails' = 'N', 'pageNum' = '1')
  res = GET(url, query = qry, verbose())
  lst = content(res)$results
  # contents: SEARCHVAL, X, Y, LATITUDE, LONGITUDE, LONGTITUDE
  default_row = c("No Result", 21700.880785, 30966.194690, 1.296321, 103.776718)
  names(default_row) = c("name", "x", "y", "lat", "lng")
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
combineSameNameEntries = function(df){
  if(nrow(df) > 1){
    df = df %>% group_by(name) %>% 
      summarise(x = mean(x), y = mean(y), lng = mean(lng), lat = mean(lat))
  }
  return(df)
}

# generate html text to wrap around a named row
namedRowToHtml = function(namedRow, hlevel){
  types = names(namedRow)
  row = namedRow
  rtn = ""
  for(i in 1:length(types)){
    rtn = paste0(rtn, "<li><", hlevel, ">", types[i], "</", hlevel, "><p>", row[i], "</p></li>")
  }
  return(rtn)
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
  for (i in 1:nrow(merged)) { # remove spaces, etc.
    merged$name[i] = str_squish(merged$name[i])
  }
  return(merged)
}

# crawls the unordered information (named row) for a restaurant by a Chope address live
getRestaurantInfoFromLink = function(href){
  page = safeReadUrl(href)
  types = getTextDfFromCss(page, ".type")
  conts = getTextDfFromCss(page, "li>span~p")
  miscs = getTextDfFromCss(page, "h6")
  texts = getTextDfFromCss(page, ".section>p:last-child")
  for(i in 1:length(conts)){ # get concise strings for conts
    if(is.character(conts[i])) conts[i] = str_squish(conts[i])
  }
  for(i in 1:length(texts)){ # get concise strings for texts
    if(is.character(texts[i])) texts[i] = str_squish(texts[i])
  }
  while(nrow(types)>nrow(conts)){# case of empty strings
    conts[nrow(conts) + 1, ] = c(NA)
  }
  while(nrow(miscs)>nrow(texts)){# case of empty strings
    texts[nrow(texts) + 1, ] = c(NA)
  }
  types = cbind(t(types), t(miscs))
  conts = cbind(t(conts), t(texts))
  useful = c("Cuisine", "Location", "Menus", "Address", "Hours", "Price")
  for (i in 1:length(useful)) {
    if (!(useful[i] %in% types)){
      conts = cbind(conts, c("Unknown"))
      types = cbind(types, c(useful[i]))
    }
  }
  datum = as.data.frame(conts)
  names(datum) = types
  return(datum)
}

# gets one ordered row of restaurant's info with geo data as a data frame
getRestaurantGeoInfoFromLink = function(href){
  datum = getRestaurantInfoFromLink(href)
  datum$href = href
  addr = str_extract(datum$Address, "\\d{6}")
  loc = searchLoc(addr)
  datum$x = loc$x[1]
  datum$y = loc$y[1]
  datum$lng = loc$lng[1]
  datum$lat = loc$lat[1]
  sdata = subset(datum, select = c(lng, lat, x, y, Cuisine, Location, Menus, Address, Hours, Price))
  odata = subset(datum, select = -c(lng, lat, x, y, Cuisine, Location, Menus, Address, Hours, Price))
  sdata$html = namedRowToHtml(odata, "h5")
  return(sdata)
}

# gets restaurants data (very slow)
crawRestaurantsInfo = function(refs){
  df <- data.frame(matrix(ncol = 11, nrow = nrow(refs)))
  refs$success = 0 # 3
  for (i in 1:nrow(refs)) {
    tryCatch({
      row = getRestaurantGeoInfoFromLink(refs$href[i]) # 11
      df[i, ] = row[1, ]
      refs$success[i] = 1
    }, error = function(e){})
  }
  df = cbind(df, refs)
  names(df) = c("lng", "lat", "x", "y", "Cuisine", "Location", "Menus", 
                "Address", "Hours", "Price", "html", "name", "href", "success")
  df$lng = as.numeric(df$lng)
  df$lat = as.numeric(df$lat)
  df$x = as.numeric(df$x)
  df$y = as.numeric(df$y)
  return(df)
}

# crawls all restaurant data from Chope except for 404 pages, and save as csv
getAndSaveRestaurantsInfo = function(){
  links = getRestaurantLinks()
  res = crawRestaurantsInfo(links)
  res = res %>% filter(success == 1)
  write.csv(res, "restaurants.csv", row.names = TRUE)
}