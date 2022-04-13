source("lib.R")
    
tabNearby <- tabPanel(
  "What's nearby?", 
  fluid = TRUE,
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "choose",
        label="What to look for:",
        choices = c("Hotels","MRT stations","Taxi stands", "Hawker centres")
        ),
      textInput("loc_input", label = h3("I am  near:"))
      ),
    mainPanel(
      leafletOutput(outputId = "nearby"),
      htmlOutput(outputId = "weather")
      )
    ),
  )

tabAttractions <- tabPanel(
  "Tourist attractions", 
  fluid = TRUE,
  mainPanel(
    plotOutput("tourist")
    )
  )

tabLiveData <- tabPanel(
  "Live data", 
  fluid = TRUE,
  sidebarLayout(
    sidebarPanel(
      actionButton("refresh", label = "Refresh")
      ),
    mainPanel(
      textOutput("text_output")
      )
    )
  )

tabInfo <- tabPanel(
  "Information about this app", 
  fluid = TRUE,
  mainPanel(
    htmlOutput("title"),
    htmlOutput("desc")
    )
  )
