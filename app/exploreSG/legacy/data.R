getData <- function(dir, ext){
  fsource = list.files(dir, pattern = ext, full.names = TRUE, ignore.case = TRUE)
  sapply(fsource, source, .GlobalEnv)
}

getData("datascript", "*.R")