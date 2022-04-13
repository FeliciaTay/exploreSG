source("lib.R")
source("funcs.R")

isGGmapRegistered = bRegisterGGmap("lbz")
if(!isGGmapRegistered) cComment("Swap the Map service from GGmap to something else.")

cComment("td: Sanity checks")

source("appUI.R")
source("appServer.R")

shinyApp(
  ui = appUI,
  server = appServer
)