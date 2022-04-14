require("httr")
require("jsonlite")

dateTimeStr = function(){
  dt = Sys.time()
  d = format(dt, format="%Y-%m-%d")
  t = format(dt, format="%H:%M:%S")
  t = gsub(":", "%3A", t)
  str = paste(d, t, sep = "T", collapse = NULL)
  return(str)
}

# the only exposed function
getWeatherForecast <- function(){
  url_base = "https://api.data.gov.sg/v1/environment/2-hour-weather-forecast?date_time="
  weather_url = paste(url_base, dateTimeStr(), sep = "", collapse = NULL)
  response = GET(weather_url)
  responseStr = content(response, "text", encoding = "UTF-8")
  responseJson = fromJSON(responseStr, flatten = TRUE)
  forecasts = as.data.frame(responseJson$items$forecasts)
  metadata = as.data.frame(responseJson$area_metadata)
  merged = merge(forecasts, metadata)
  names(merged) = c("area","forecast","name","lat","lng")
  merged = relocate(merged, where(is.numeric), .before = where(is.character))
  merged = relocate(merged, lng)
  return(merged)
}

#usage:
#weather = getWeatherForecast()
