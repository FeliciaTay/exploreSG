source("lib.R")

bRegisterGGmap <- function(name) {
  strkey = switch (name,
    "lbz" = 'AIzaSyAPypfkt7AxQ7o2r96Xw1-HJzXcBEnh-Nc'
  )
  if(is.null(strkey)) return(FALSE)
  else{
    ggmap::register_google(key=strkey)
    return(TRUE)
  }
}

cComment <- function(text) {
  return(NULL)
}

dReadData <- function(folder, extension) {
  filenames = list.files(folder, pattern = extension, full.names = TRUE)
  ldf = lapply(filenames, read.csv, check.names = FALSE, fileEncoding="UTF-8-BOM")
  d1 = ldf[[1]]
  if(length(ldf) < 2) return(ldf[[1]])
  d1$f_cat = 1
  for (i in 2:length(ldf)) {
    d2 = ldf[[i]]
    d1 = full_join(d1, d2, by = c("lng" = "lng", "lat" = "lat"))
  }
  return(d1)
}