#' Creates Shiny app to assist in sharing and collecting community-focused data
#' @description Creates web app using the [shiny][shiny::shiny-package] package,
#'  with options for visualizing your existing data and analysis for community 
#'  sharing, as well as for visualizing and collecting data from app viewers 
#'  (e.g. community members). If you have not yet done so, you must run 
#'  [google_sheets_auth()], following the input instructions, before being able
#'   to visualize and collect data from viewers through this function, which 
#'   will draw from the [googlesheets4][googlesheets4::googlesheets4-package]
#'    package functionality to do so.
#' @param response_question Prompt for web app viewers on the type of data you 
#' are looking for from them
#' @param sheet_id ID of the Google Sheet to use (WILL ADD FURTHER INSTRUCTIONS)
#' @param sheet_name Name of the sheet to use within Google Sheets document
#' @param shapefile Shapefile of data to display on map. Shapefile must contain
#'  a `"year"` variable (the web app will include a selector slider for 
#'  years), a `"variable"` variable (the web app will include a page displaying 
#'  data on each variable), and an `"estimate"` variable (for the value or 
#'  estimate associated with each value in the `"variable"` column). The 
#'  shapefile's geometry must be polygons. The `"year"` variable values must all
#'   be the same number of years apart.
#' @export

web_app <- function(response_question, sheet_id, sheet_name, shapefile) {

  options(gargle_oauth_cache = ".secrets", email = Sys.getenv("google_sheets_email"))
  googlesheets4::gs4_auth(email = Sys.getenv("google_sheets_email"))
  
  shapefile <- sf::st_transform(shapefile, crs = "WGS84")
  
  ui <- shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::h4(response_question),
        shiny::textInput("comment", "Enter comment/provide data value corresponding to coordinates"),
        shiny::actionButton("submit_button", "Submit your response and see updated map"),
        shiny::sliderInput("year_input", "Data year to display", min = min(shapefile$year, na.rm = TRUE), max = max(shapefile$year, na.rm = TRUE), value = min(shapefile$year, na.rm = TRUE), step = min(shapefile$year[shapefile$year != min(shapefile$year, na.rm = TRUE)], na.rm = TRUE) - min(shapefile$year, na.rm = TRUE)),
        shiny::selectInput("variable_input", "Data variable to display", choices = unique(shapefile$variable))
      ),
    shiny::mainPanel(
        leaflet::leafletOutput("map")
    )
    )
  )
  
  server <- function(input, output) {
    
    map_output <- shiny::reactive({
      
      pal <- leaflet::colorNumeric(palette = "BuPu", domain = shapefile$estimate[shapefile$variable == input$variable_input])
      
      if (input$submit_button == 0) {
        
        points <- googlesheets4::read_sheet(ss = sheet_id, sheet = sheet_name) |>
          dplyr::mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude))
        leaflet::leaflet(shapefile[shapefile$year == input$year_input,][shapefile$variable[shapefile$year == input$year_input] == input$variable_input,]) |> 
          leaflet::addTiles() |>
          leaflet::addPolygons(color = ~pal(estimate), fillOpacity = 0.7, label = ~paste(county, estimate)) |>
          leaflet::addLegend(pal = pal, values = ~shapefile$estimate[shapefile$variable == input$variable_input], title = "estimate") |>
          leaflet::addMarkers(data = points, lat = ~latitude, lng = ~longitude, label = ~comment) |>
          leaflet::addTiles(layerId = "map_click")
        
       } else {
      
      data_collected <- data.frame(input$map_click$lat, input$map_click$lng, input$comment, Sys.time())
      googlesheets4::sheet_append(ss = sheet_id, data = data_collected, sheet = sheet_name)
      points <- googlesheets4::read_sheet(ss = sheet_id, sheet = sheet_name) |>
        dplyr::mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude))
      leaflet::leaflet(shapefile[shapefile$year == input$year_input,][shapefile$variable[shapefile$year == input$year_input] == input$variable_input,]) |> 
        leaflet::addTiles() |> 
        leaflet::addPolygons(color = ~pal(estimate), fillOpacity = 0.7, label = ~paste(county, estimate)) |>
        leaflet::addLegend(pal = pal, values = ~shapefile$estimate[shapefile$variable == input$variable_input], title = "estimate") |>
        leaflet::addMarkers(data = points, lat = ~latitude, lng = ~longitude, label = ~comment)}
    })
    
    output$map <- leaflet::renderLeaflet({
      map_output()
    })
    
    shiny::observeEvent(input$map_click, {
      click <- input$map_click
      latitude <- click$lat
      longitude <- click$lng
      
      leaflet::leafletProxy("map") |>
        leaflet::addPopups(longitude, latitude, "This point is selected.")
    })
    
  }
  shiny::shinyApp(ui = ui, server = server)
}