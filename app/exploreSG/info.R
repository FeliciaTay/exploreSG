info_UI <- function(id){
  ns <- NS(id)
  tagList( # UI elements
    mainPanel(
      htmlOutput(ns("desc"))
    )
  )
}

info_server <- function(id){
  
  moduleServer(
    id,
    function(input, output, session) {
      output$desc <- renderUI({ 
        HTML(paste("<br>", "What's nearby? tab:",
                   "<br>", "1. You can input a location using the text input box and you can choose a specific location nearest to what you inputted.",
                   "<br>", "2. You can choose to show hawker centres, hotels, MRT/LRT stations, restaurants, taxi stands or tourist attractions nearby.",
                   "<br>", "3. You can adjust the number of data sources you want to see using the slider. Notice that you can see the 2-hr weather forecast for the area!",
                   "<br>", "4. Click on a location marker on the map and you will be able to see more information using the drop-down box at the bottom.",
                   "<br>", "5. You can zoom in and out as well as drag around on the map for a clearer street view:)",
                   "<br>", "! Please only input places in Singapore.",
                   "<br>", "<br>", "Tourist attractions tab: You will be able to see a map visualisation of all tourist attractions in Singapore, along with a short description of the places at the bottom. Select an attraction using the drop-down box at the top to see more information about the attraction in the right panel.",
                   "<br>", "<br>", "Parks tab: You can select the park or nature reserve you will like to find out more about using the drop-out box."))
      })
    }
    
  )
}