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
#' @param shapefile_vars Shapefile of data, specifically with numeric variables 
#' that can be color-coded, to display on map alongside a slider to select a variable
#'  to display. Shapefile must contain
#'  a `"year"` variable (the web app will include a selector slider for
#'  years), a `"variable"` variable (the web app will include a page displaying
#'  data on each variable, e.g. unemployment, median air quality), and an 
#'  `"estimate"` variable (for the value or
#'  estimate associated with each value in the `"variable"` column). The
#'  shapefile's geometry must be polygons. The `"year"` variable values must all
#'   be the same number of years apart.
#' @param shapefile_features Shapefile of points and/or polygons, without a corresponding
#'  numeric variable. All points and polygons in this 
#'  shapefile will display simultaneously by default. Shapefile must contain 
#'  a `"features"` variable (for the name of the feature category, e.g. park, school), 
#'  a `"name"` variable (for the name of each individual feature, e.g. Central Park), 
#'  and a `"type"` variable (delineating different feature types within each feature 
#'  category, e.g. private school, public school). All observations corresponding
#'   to a given value of the `"features"` variable must be either points or 
#'   polygons (i.e. no mixing of points and polygons within a feature category).
#' @export

web_app <- function(response_question, sheet_id, sheet_name, shapefile_vars, shapefile_features) {
  options(gargle_oauth_cache = ".secrets", email = Sys.getenv("google_sheets_email"))
  googlesheets4::gs4_auth(email = Sys.getenv("google_sheets_email"))

  shapefile_vars <- sf::st_transform(shapefile_vars, crs = "WGS84")
  shapefile_features <- sf::st_transform(shapefile_features, crs = "WGS84")

  ui <- shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::h4(response_question),
        shiny::textInput("comment", "Enter comment/provide data value corresponding to coordinates"),
        shiny::actionButton("submit_button", "Submit your response and see updated map"),
        shiny::sliderInput("year_input", "Data year to display", min = min(shapefile_vars$year, na.rm = TRUE), max = max(shapefile_vars$year, na.rm = TRUE), value = min(shapefile_vars$year, na.rm = TRUE), step = min(shapefile_vars$year[shapefile_vars$year != min(shapefile_vars$year, na.rm = TRUE)], na.rm = TRUE) - min(shapefile_vars$year, na.rm = TRUE), sep = ""),
        shiny::selectInput("variable_input", "Data variable to display", choices = c("none", unique(shapefile_vars$variable)), selected = "none"),
        shiny::checkboxGroupInput("features_input", "Features to display", choices = unique(shapefile_features$features), selected = unique(shapefile_features$features))
      ),
      shiny::mainPanel(
        leaflet::leafletOutput("map")
      )
    )
  )

  server <- function(input, output) {
    
    filtered_features <- reactive({shapefile_features[shapefile_features$features %in% input$features_input,]})
    
    features_pal <- leaflet::colorFactor(palette = "RdGy", domain = shapefile_features$features)
    
    vars_none <- reactive({ifelse(input$variable_input == "none", TRUE, FALSE)})
    
    filtered_vars_pal <- reactive({shapefile_vars$estimate[shapefile_vars$variable == input$variable_input]})
    
    vars_pal <- reactive({leaflet::colorNumeric(palette = "BuPu", domain = filtered_vars_pal())})
    
    filtered_vars <- reactive({shapefile_vars[shapefile_vars$year == input$year_input, ][shapefile_vars$variable[shapefile_vars$year == input$year_input] == input$variable_input, ]})
    
    output$map <- leaflet::renderLeaflet({
      
      leaflet::leaflet() |>
        leaflet::addTiles()
    })
      
      observe({
        
        for (x in unique(shapefile_features$features)) {
          
          if (!(x %in% unique(filtered_features()$features))) {
              
              leaflet::leafletProxy("map") |>
                leaflet::clearGroup(group = x)
            }
        }
        
        for (i in unique(filtered_features()$features)) {
        
        if (unique(sf::st_geometry_type(filtered_features()[filtered_features()$features == i,])) == "POLYGON") {
          
          leaflet::leafletProxy("map", data = filtered_features()[filtered_features()$features == i,]) |>
            leaflet::addPolygons(group = i, label = ~name, color = ~features_pal(features), fillOpacity = 0.9)
        }
        
        if (unique(sf::st_geometry_type(filtered_features()[filtered_features()$features == i,])) == "POINT") {
          
          leaflet::leafletProxy("map", data = filtered_features()[filtered_features()$features == i,]) |>
            leaflet::addCircles(group = i, label = ~name, color = ~features_pal(features), fillOpacity = 0.9)
        }}
        
        leaflet::leafletProxy("map", data = filtered_features()[filtered_features()$features == i,]) |>
          leaflet::addLegend(pal = features_pal, values = ~ filtered_features()$features, title = "feature")
      })
        
    observe({
      
      pal <- vars_pal()

      if (input$submit_button == 0) {
        points <- googlesheets4::read_sheet(ss = sheet_id, sheet = sheet_name) |>
          dplyr::mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude))
        
        if (vars_none() == FALSE) {
        leaflet::leafletProxy("map") |>
          leaflet::addTiles() |>
          leaflet::addPolygons(data = filtered_vars(), color = ~ pal(estimate), fillOpacity = 0.7, label = ~ paste(county, estimate)) |>
          leaflet::addLegend(data = filtered_vars(), pal = pal, values = ~ filtered_vars_pal(), title = "estimate") |>
          leaflet::addMarkers(data = points, lat = ~latitude, lng = ~longitude, label = ~comment) |>
          leaflet::addTiles(layerId = "map_click")
        }
      } else {
        data_collected <- data.frame(input$map_click$lat, input$map_click$lng, input$comment, Sys.time())
        googlesheets4::sheet_append(ss = sheet_id, data = data_collected, sheet = sheet_name)
        points <- googlesheets4::read_sheet(ss = sheet_id, sheet = sheet_name) |>
          dplyr::mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude))
        
        if (vars_none() == FALSE) {
        leaflet::leafletProxy("map") |>
          leaflet::addTiles() |>
          leaflet::addPolygons(data = filtered_vars(), color = ~ pal(estimate), fillOpacity = 0.7, label = ~ paste(county, estimate)) |>
          leaflet::addLegend(data = filtered_vars(), pal = pal, values = ~ filtered_vars_pal(), title = "estimate") |>
          leaflet::addMarkers(data = points, lat = ~latitude, lng = ~longitude, label = ~comment)
        }
      }
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
