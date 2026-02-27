#' Creates Shiny app to assist in sharing and collecting community-focused data
#' @description Creates web app using the R Shiny package, with options for 
#' visualizing your existing data and analysis for community sharing, as well
#' as for visualizing and collecting data from app viewers (e.g. community 
#' members). If you have not yet done so, you must run [google_sheets_auth()], 
#' following the input instructions, before being able to visualize and collect 
#' data from viewers through this function.
#' @param response_question Prompt for web app viewers on the type of data you 
#' are looking for from them
#' @param sheet_id ID of the Google Sheet to use (WILL ADD FURTHER INSTRUCTIONS)
#' @param sheet_name Name of the sheet to use within Google Sheets document
#' @export

web_app <- function(response_question, sheet_id, sheet_name) {

  options(gargle_oauth_cache = ".secrets", email = Sys.getenv("google_sheets_email"))
  googhesheets4::gs4_auth(email = Sys.getenv("google_sheets_email"))
  
  ui <- shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::h4(response_question),
        shiny::textInput("comment", "Enter comment/provide data value corresponding to coordinates"),
        shiny::actionButton("submit_button", "Submit your response and see updated map")
      ),
    shiny::mainPanel(
        leaflet::leafletOutput("map")
    )
    )
  )
  
  server <- function(input, output) {
    
    map_output <- shiny::reactive({
      
      if (input$submit_button == 0) {
        
        points <- googlesheets4::read_sheet(ss = sheet_id, sheet = sheet_name) |>
          dplyr::mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude))
        leaflet::leaflet() |> 
          leaflet::addTiles() |> 
          leaflet::addMarkers(data = points, lat = ~latitude, lng = ~longitude, label = ~comment) |>
          leaflet::addTiles(layerId = "map_click")
        
       } else {
      data_collected <- data.frame(input$map_click$lat, input$map_click$lng, input$comment)
      googlesheets4::sheet_append(ss = sheet_id, data = data_collected, sheet = sheet_name)
      shiny::h5("Your data has been submitted. Thank you! You will now see it on the map.")
      points <- googlesheets4::read_sheet(ss = sheet_id, sheet = sheet_name) |>
        dplyr::mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude))
      leaflet::leaflet() |> 
        leaflet::addTiles() |> 
        leaflet::addMarkers(data = points, lat = ~latitude, lng = ~longitude, label = ~comment)}
    })
    
    output$map <- leaflet::renderLeaflet({
      map_output()
    })
    
    observeEvent(input$map_click, {
      click <- input$map_click
      latitude <- click$lat
      longitude <- click$lng
      
      leaflet::leafletProxy("map") |>
        leaflet::addPopups(longitude, latitude, "This point is selected.")
    })
  }
  shiny::shinyApp(ui = ui, server = server)
}