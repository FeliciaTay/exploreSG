library(rvest)
library(xml2)
library(dplyr)
library(tidyr)

url<-"https://www.chope.co/singapore-restaurants/list_of_restaurants?source=chope.com.sg&lang=en_US"
page<-read_html(url)


restaurants <- html_nodes(page,".az-result+ .az-result .cf > a , .cf+ .cf > a") 
length(restaurants) # 1549 restaurants

restaurants.names<-html_text(restaurants) %>%as.data.frame()
restaurants.names


restaurant.href<-html_attr(restaurants,"href")
as.data.frame(restaurant.href)


prices<-list()
cuisines<-list()
locations<-list()

final <- data.frame(matrix(nrow=0,ncol=3))
for (i in 1:length(restaurants))
  {
  
  restaurant.info<-read_html(restaurant.href[i])
  cuisines <- html_nodes(restaurant.info,".col-sm-6:nth-child(1) li:nth-child(1) p")%>%html_text()
  prices<-html_nodes(restaurant.info,"#rstr_info li:nth-child(4) p")%>% html_text()
  locations<-html_nodes(restaurant.info,".mapbox")%>%html_text()
  curr <- data.frame(matrix(nrow=0,ncol=3))
  curr <- cbind(cuisines, prices, locations)
  final <- rbind(final, curr)
  }















