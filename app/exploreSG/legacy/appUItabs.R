source("lib.R")
source("funcs.R")
source("datascript/restaurants.R")
restaurantLinkTable = getRestaurantLinks()
    
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

tabRestaurants <- tabPanel(
  "Check Restaurants", 
  fluid = TRUE,
  sidebarLayout(
    sidebarPanel(
      selectInput("restaurant", "Restaurants:", restaurantLinkTable$name),
      actionButton("refresh", label = "Refresh")
      ),
    mainPanel(
      htmlOutput("tabPanelHtml")
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
