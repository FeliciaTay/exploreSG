library(DT)

tourists_UI <- function(id){
  ns <- NS(id)
  tagList( # UI elements
    mainPanel(
      selectInput(ns("tourist_input"),
                  "I would like to find out more about:",
                  choices = ""),
      leafletOutput(ns("touristmap")),
      dataTableOutput(ns("tourist"))
    ),
    fluidRow(
      column(4, class= "R1",
             tabsetPanel(type= "pills",
                         tabPanel("Description", fluid = TRUE, mainPanel(htmlOutput(ns("location_title")),
                                                                         htmlOutput(ns("descrip")))),
                         tabPanel("Other information", fluid = TRUE, mainPanel(htmlOutput(ns("more_info")))))
      )                   
    )
  )
}

tourists_server <- function(id){
  tourist <- read.csv("data/TOURISM.csv") %>% filter(!grepl("?€?", Name, fixed = T))
  temp_icon <- makeAwesomeIcon(text=fa("circle"), iconColor="white", markerColor="blue")
  
  moduleServer(
    id,
    function(input, output, session) {
      
      cleanName <- function() {
        if (tourist$name) {
          
        }
      }
      
      observe({
        updateSelectInput(
          inputId = "tourist_input",
          choices = tourist$Name
        )
      })
      
      getLink <- function() {
        curr <- tourist %>% filter(Name == input$tourist_input) %>% select(Field_1)
        curr <- gsub("yoursingapore", "visitsingapore", curr)
        curr <- gsub("/en", "", curr)
        curr <- gsub(".html", "/", curr)
        curr <- gsub("www", "https://www", curr)
        curr
      }
      
      output$location_title <- renderUI({ 
        HTML(paste(input$tourist_input))
      })
      
      readContent <- function(url) {
        tryCatch ({
          info <- read_html(url) %>% html_elements(xpath = "//div[@class='main-content column']/div/div/p") %>% html_text()
          str <- ""
          for (i in 1:length(info)) {
            if (i == 1) {
              str <- paste(str,  "<br>", info[i])
            } else {
              str <- paste(str,  "<br><br>", info[i])
            }
          }
          str
        }, error=function(e) {
          str <- ""
          str
        })
      }
      
      output$descrip <- renderUI({ 
        url <- getLink()
        str <- readContent(url)
        str <- gsub("?<80>?", "", str)
        HTML(paste("Website reference :", url, "<br><br>", str))
      })
      
      output$more_info <- renderUI({
        url <- getLink()
        opening_hrs <- read_html(url) %>% html_elements(xpath = "//div[@class='boxup clearfix']/aside/ul/li[2]/div/small") %>% html_text()
        street_addr <- read_html(url) %>% html_elements(xpath = "//span[@itemprop='streetAddress']") %>% html_text()
        postal <- read_html(url) %>% html_elements(xpath = "//span[@itemprop='postalCode']") %>% html_text()
        addr <- paste0(street_addr, ", Singapore ", postal)
        HTML(paste("<b>", "Opening hours:", "</b>", opening_hrs,
                   "<br><br>", "<b>", "Address: ", "</b>", addr))
      })
      
      plotWithDesc <- function(data) {
        test_map <- leaflet() %>% addTiles() 
        for (i in 1:nrow(data)) {
          curr <- data[i,]
          long <- as.numeric(curr[1])
          lati <- as.numeric(curr[2])
          desc <- as.character(curr[3])
          test_map <- addAwesomeMarkers(test_map, lat=lati,lng=long, popup=desc, icon=temp_icon)
        }
        test_map
      }
      
      output$touristmap <-renderLeaflet(
        {
          tourist.plot <- tourist %>% select(lng, lat, Name)
          plotWithDesc(tourist.plot)
        }
      )
      
      output$tourist <-renderDataTable(
        {
          tourist.plot <- tourist %>% select(Name, description)
          datatable(tourist.plot)
        }
      )
      
    }
    
  )
}