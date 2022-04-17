library(rvest)

parks_UI <- function(id){
  ns <- NS(id)
  tagList( # UI elements
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("parks_input"),
                    "I would like to find out more about:",
                    choices = "")
      ),
      mainPanel(
        uiOutput(ns("image")),
        htmlOutput(ns("parks"))
      )
    )
  )
}

parks_server <- function(id){
  
  moduleServer(
    id,
    function(input, output, session) {
      
      getLink <- function(vec) {
        link <- "https://sg.search.yahoo.com/search?p="
        for (i in 1:length(vec)) {
          if (i == length(vec)) {
            link <- paste0(link, vec[i])
          } else {
            link <- paste0(link, vec[i], "+")
          }
        }
        link
      }
      
      output$image <- renderUI({
        str <- strsplit(input$parks_input, " ")[[1]] 
        url <- getLink(str)
        link <- read_html(url) %>% html_elements(xpath = "//div[@class='compImageProfile noImage bb-1-E0E4E9 m-0 ov-h']/a/img") %>% html_attr("src")
        tags$img(src = link)
      })
      
      getParksNames <- function(){
        parks_url <- "https://www.nparks.gov.sg/gardens-parks-and-nature/parks-and-nature-reserves"
        park_names <- read_html(parks_url) %>% html_elements(xpath = "//div[@class='dropdown-routing-lists']/select/option") %>% html_text()
        park_names[-1]
      }
      
      readParkContent <- function(){
        tryCatch ({
          parks_url <- "https://www.nparks.gov.sg/gardens-parks-and-nature/parks-and-nature-reserves"
          park_names <- read_html(parks_url) %>% html_elements(xpath = "//div[@class='dropdown-routing-lists']/select/option") %>% html_text()
          park_names <- park_names[-1]
          index <- match(input$parks_input, park_names)
          
          semi_url <- "https://www.nparks.gov.sg"
          all_parks_url <- read_html(parks_url) %>% html_elements(xpath = "//div[@class='dropdown-routing-lists']/select/option") %>% html_attr("value")
          curr <- paste0(semi_url, all_parks_url[index + 1])
          info <- read_html(curr) %>% html_elements(xpath = "//div[@class='two-columns one-third']/div[@class='column description']/p") %>% html_text()
          str <- ""
          for (i in 1:length(info)) {
            str <- paste(str,  "<br><br>", info[i])
          }
          c(length(info), str)
        }, error=function(e) {
          length <- 0
          str <- "null"
          c(length(info), str)
        })
        
      }
      
      observe({
        updateSelectInput(
          inputId = "parks_input",
          choices = getParksNames()
        )
      })
      
      
      output$parks <- renderUI({
        curr <- readParkContent()
        len <- curr[1]
        str <- curr[2]
        
        if (len == 0) {
          HTML(paste("There is no related information according to Nparks:("))
        } else {
          HTML(paste(str))
        }
        
      })
      
    }
    
  )
}