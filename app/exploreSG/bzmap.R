source("bzmap_events.R")

# region ui ###################################################################
bzmap_UI <- function(id, vecDataFileNames, vecDataLabels, vecPopupFields, vecOutputDropFields){
  dataValidation(vecDataFileNames, vecDataLabels, vecPopupFields, vecOutputDropFields)
  ns <- NS(id) # generate namespace function
  tagList( # UI elements
    titlePanel("Welcome to SG!"),
    sidebarLayout(
      sidebarPanel(
        textInput(ns("txtSearch"), "Type here to search your location:"),
        selectInput(ns("drpSelect"), "Available options based on \"\":",searchLoc()$name),
        textOutput(ns("txtTableSelectedTitle")),
        wellPanel(
          tableOutput(ns("tableSelected")),
        ),
        wellPanel(
          checkboxGroupInput(ns("chkData"), "Which data sources to show?", vecDataLabels)
        )
      ),
      mainPanel(
        leafletOutput(outputId = ns("map")),
        fluidRow(
          wellPanel(
            sliderInput(ns("sliDistance"), "<---Near-you-only-----[Percentage]-----Show-all-data--->", 0, 100, 100),
            textOutput(ns("txtWeather"))
          ),
          wellPanel(
            selectInput(ns("selDetail"), "See what attribute you have selected after clicking on a blob!", NULL),
            tableOutput(ns("tableNear"))
          )
        )
      )
    )
  )
}

# region server ###############################################################
bzmap_server <- function(id, vecDataFileNames, vecDataLabels, vecPopupFields, vecOutputDropFields){
  dataValidation(vecDataFileNames, vecDataLabels, vecPopupFields, vecOutputDropFields)
  context <- initDataContext(vecDataFileNames, vecDataLabels, vecPopupFields, vecOutputDropFields)
  moduleServer(
    id,
    function(input, output, session){ # logic
      # event-triggering controls
      observeEvent(input$map_marker_click,{observable_mapClickd(input, output, session, context)})
      observeEvent(input$txtSearch       ,{observable_txtSearch(input, output, session, context)})
      observeEvent(input$drpSelect       ,{observable_drpSelect(input, output, session, context)})
      observeEvent(input$chkData         ,{observable_chkChange(input, output, session, context)}, ignoreNULL = FALSE)
      observeEvent(input$sliDistance     ,{observable_sliChange(input, output, session, context)})
      observeEvent(input$selDetail       ,{observable_drpDetail(input, output, session, context)})
      # normal on controls
      output$txtTableSelectedTitle <- renderText({"About this location:"})
    }
  )
}