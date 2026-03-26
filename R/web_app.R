#' Creates Shiny app to assist in sharing and collecting community-focused data
#' @description Creates web app using the [shiny][shiny::shiny-package] package,
#'  with options for visualizing your existing data and analysis for community
#'  sharing, as well as for visualizing and collecting data from app viewers
#'  (e.g. community members). If you have not yet done so, you must run
#'  [google_sheets_auth()], following the input instructions, before being able
#'   to visualize and collect data from viewers through this function, which
#'   will draw from the [googlesheets4][googlesheets4::googlesheets4-package]
#'    package functionality to do so.'
#' @param project_name Name of project you are running this function for. Will 
#' use the same Google Sheets document as any other time you call this function.
#'  The first time you specify a certain project name, this function will 
#'  create a corresponding Google Sheets document.
#' @param title Title to display at the top of the web app.
#' @param response_questions Vector of prompts for web app viewers on the type 
#' of data you are looking for from them. Each prompt will appear as a different
#'  question, with data to be included on a different sheet in the Google Sheets
#'   document.
#' @param shapefile_vars Shapefile of data, specifically with numeric variables 
#' that can be color-coded, to display on map alongside a slider to select a variable
#'  to display. Shapefile must contain
#'  a `"year"` variable (the web app will include a selector slider for
#'  years), a `"variable"` variable (the web app will include a dropdown for 
#'  viewers to select a variable for display, e.g. unemployment, median air quality), an 
#'  `"estimate"` variable (for the value or
#'  estimate associated with each value in the `"variable"` column), and a `"name"` variable containing
#'    the names of the geographic areas (e.g. Orange County), which will be 
#'    displayed alongside estimates upon mouse-over of the polygons. The
#'  shapefile's geometry must be polygons.
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

web_app <- function(project_name, title, response_questions, shapefile_vars, shapefile_features) {
  options(gargle_oauth_cache = ".secrets", email = Sys.getenv("google_sheets_email"))
  googlesheets4::gs4_auth(email = Sys.getenv("google_sheets_email"))
  
  num_questions <- length(response_questions)
  
  renv <- file.path(Sys.getenv("HOME"), ".Renviron")
  if (!file.exists(renv)) {
    file.create(renv)
  }
  oldenv <- readLines(renv)
  id_exists <- FALSE
  for (i in 1:length(oldenv)) {
    if (substr(oldenv[i], 1, nchar(project_name)) == project_name) {
      sheet_id <- substr(oldenv[i], (nchar(project_name) + 3), (nchar(oldenv[i]) - 1))
      id_exists <- TRUE
    }
  }
  if (id_exists == FALSE) {
    sheets_vector <- rep(NA, num_questions)
    for (x in 1:num_questions) {
      sheets_vector[x] <- paste0("q", as.character(x))
    }
    sheet_obj <- googlesheets4::gs4_create(name = project_name, sheets = sheets_vector)
    sheet_id <- as.character(sheet_obj)
    for (x in 1:num_questions) {
      googlesheets4::sheet_write(data = data.frame("latitude" = "", "longitude" = "", "comment" = "", "timestamp" = ""), ss = sheet_id, sheet = paste0("q", as.character(x)))
    }
    write(paste0(project_name, "=\"", sheet_id, "\""), renv, sep = "\n", append = TRUE)
  }
  
  
  shapefile_vars <- sf::st_transform(shapefile_vars, crs = "WGS84")
  shapefile_features <- sf::st_transform(shapefile_features, crs = "WGS84")
  
  ui <- shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::wellPanel(style = "border-width: 2px; border-color: black",
                         shiny::selectInput("variable_input", "Data variable to display", choices = c("none", unique(shapefile_vars$variable)), selected = "none"),
                         shiny::conditionalPanel(condition = "input.variable_input != 'none'", shiny::sliderInput("year_input", "Data year to display", min = min(shapefile_vars$year, na.rm = TRUE), max = max(shapefile_vars$year, na.rm = TRUE), value = min(shapefile_vars$year, na.rm = TRUE), step = min(shapefile_vars$year[shapefile_vars$year != min(shapefile_vars$year, na.rm = TRUE)], na.rm = TRUE) - min(shapefile_vars$year, na.rm = TRUE), sep = ""))),
        shiny::wellPanel(style = "border-width: 2px; border-color: black",
                         shiny::checkboxGroupInput("features_input", "Features to display", choices = unique(shapefile_features$features), selected = unique(shapefile_features$features))),
        shiny::wellPanel(style = "border-width: 2px; border-color: black",
                         shiny::selectInput("points_input", "Community data to display", choices = c("none", response_questions), selected = "none"),
                         shiny::textOutput("community_data_output"))
      ),
      shiny::mainPanel(
        leaflet::leafletOutput("map"),
        shiny::wellPanel(style = "border-width: 2px; border-color: black; margin-top: 30px",
                         shiny::h4(shiny::textOutput("question_text")),
                         shiny::conditionalPanel(condition = "(input.submit_button > 0) && (output.submit_text != 'Submit final response')", shiny::textInput("comment", "Click on map above, then enter comment/provide data value corresponding to coordinates")),
                         shiny::conditionalPanel(condition = "(output.submit_text != 'Submit final response')", shiny::actionButton("submit_button", shiny::textOutput("submit_text"))))
      )
    )
  )
  
  server <- function(input, output) {
    
    output$question_text <- renderText({
      
      if (input$submit_button == 0) {"Click button below to provide your own data/comments to add to the map"} 
      
      else if (input$submit_button <= num_questions) {response_questions[input$submit_button]}
      
      else {"All responses have been submitted."}
      
    })
    
    output$submit_text <- renderText({
      
      if (input$submit_button == 0) {"Click to begin"} 
      
      else if (input$submit_button <= num_questions) {"Submit response and continue"}
      
      else {"Submit final response"}
      
    })
    
    filtered_features <- reactive({shapefile_features[shapefile_features$features %in% input$features_input,]}) |>
      # binding the event makes the features and their legend reload every time the vars or features are changed, so that the features will stay on top of the vars on the map and so the features legend won't go above the vars legend
      bindEvent(input$year_input, input$variable_input, input$features_input)
    
    features_pal <- leaflet::colorFactor(palette = "RdGy", domain = shapefile_features$features)
    
    vars_none <- reactive({ifelse(input$variable_input == "none", TRUE, FALSE)})
    
    filtered_vars_pal <- reactive({shapefile_vars$estimate[shapefile_vars$variable == input$variable_input]})
    
    vars_pal <- reactive({leaflet::colorNumeric(palette = "BuPu", domain = filtered_vars_pal())})
    
    filtered_vars <- reactive({shapefile_vars[shapefile_vars$year == input$year_input, ][shapefile_vars$variable[shapefile_vars$year == input$year_input] == input$variable_input, ]})
    
    collected_points <- reactive({
      points_df <- data.frame(latitude = numeric(), longitude = numeric(), comment = character(), timestamp = as.POSIXct(character()), sheet = character())
      for (i in 1:num_questions) {
        points_df_1 <- googlesheets4::read_sheet(ss = sheet_id, sheet = paste0("q", as.character(i))) |>
          dplyr::mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude), sheet = paste0("q", as.character(i)))
        points_df <- rbind(points_df, points_df_1)
      }
      points_df
    }) |>
      shiny::bindEvent(input$submit_button, ignoreNULL = TRUE)
    
    collected_points_initial <- data.frame(latitude = numeric(), longitude = numeric(), comment = character(), timestamp = as.POSIXct(character()), sheet = character())
    for (i in 1:num_questions) {
      collected_points_initial_1 <- googlesheets4::read_sheet(ss = sheet_id, sheet = paste0("q", as.character(i))) |>
        dplyr::mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude), sheet = paste0("q", as.character(i)))
      collected_points_initial <- rbind(collected_points_initial, collected_points_initial_1)
    }
    
    input_comment <- reactive({input$comment})
    
    map_click_true <- FALSE
    
    output$map <- leaflet::renderLeaflet({
      
      leaflet::leaflet() |>
        leaflet::addTiles() |>
        leaflet::fitBounds(
          lng1 = min(as.numeric(sf::st_bbox(shapefile_vars)$xmin), as.numeric(sf::st_bbox(shapefile_features)$xmin), collected_points_initial$longitude, na.rm = TRUE), 
          lat1 = min(as.numeric(sf::st_bbox(shapefile_vars)$ymin), as.numeric(sf::st_bbox(shapefile_features)$ymin), collected_points_initial$latitude, na.rm = TRUE), 
          lng2 = max(as.numeric(sf::st_bbox(shapefile_vars)$xmax), as.numeric(sf::st_bbox(shapefile_features)$xmax), collected_points_initial$longitude, na.rm = TRUE), 
          lat2 = max(as.numeric(sf::st_bbox(shapefile_vars)$ymax), as.numeric(sf::st_bbox(shapefile_features)$ymax), collected_points_initial$latitude, na.rm = TRUE)
        )
    })
    
    observe({updateTextInput(inputId = "comment", value = "")}) |>
      bindEvent(input$submit_button)
    
    observe({
      
      if (input$submit_button > 0 & (map_click_true == FALSE | input_comment() == "")) {
        
        updateActionButton(inputId = "submit_button", disabled = TRUE)
        
      }
      
      if (input$submit_button > 0 & (map_click_true == TRUE | input_comment() != "")) {
        
        updateActionButton(inputId = "submit_button", disabled = FALSE)
      }
    })
    
    observe({
      
      if (input$submit_button > 1) {
        
        map_click_true <- FALSE
        
        data_collected <- data.frame(input$map_click$lat, input$map_click$lng, input$comment, Sys.time())
        googlesheets4::sheet_append(ss = sheet_id, data = data_collected, sheet = paste0("q", as.character(input$submit_button - 1)))
        
        leaflet::leafletProxy("map") |>
          leaflet::clearGroup(group = "point selection")}
      
      if (input$submit_button > 0 & input$submit_button <= num_questions) {
        
        leaflet::leafletProxy("map") |>
          leaflet::addTiles(layerId = "map_click", group = "point selection")
      }
      }, priority = 1) |> bindEvent(input$submit_button, ignoreNULL = TRUE)
    
    observe({
        
        if (input$points_input != "none") {
          observe({
            
            if (input$points_input != "none") {
              
              sheet_num <- which(response_questions == input$points_input)
              
              if (input$submit_button == 0) {
                
                selected_sheet <- collected_points_initial[collected_points_initial$sheet == paste0("q", as.character(sheet_num)),]
                
              } else {
              
                selected_sheet <- collected_points()[collected_points()$sheet == paste0("q", as.character(sheet_num)),]}
              
              if (nrow(selected_sheet) > 0) {
                
                output$community_data_output <- renderText({""})
              
              leaflet::leafletProxy("map") |>
                leaflet::clearGroup(group = c("collected points")) |>
                leaflet::addMarkers(data = selected_sheet, lat = ~latitude, lng = ~longitude, label = ~comment, group = "collected points")
            } else {
              
              output$community_data_output <- renderText({"No data available yet for this selection."})
              
              leaflet::leafletProxy("map") |>
                leaflet::clearGroup(group = c("collected points"))
              
            }}}, priority = 0) |>
            bindEvent(input$submit_button, ignoreNULL = FALSE)
          
        } else {
          
          output$community_data_output <- renderText({""})
          
          leaflet::leafletProxy("map") |>
            leaflet::clearGroup(group = c("collected points"))
        }
    })
    
    observe({
      
      pal <- vars_pal()
      
      leaflet::leafletProxy("map") |>
        leaflet::clearGroup(group = "vars polygons") |>
        leaflet::removeControl(layerId = "vars legend")
      
      
      if (vars_none() == FALSE) {
        # setting layerId makes legend reload itself when addLegend is run again rather than creating an additional legend
        leaflet::leafletProxy("map") |>
          leaflet::addPolygons(data = filtered_vars(), color = ~ pal(estimate), fillOpacity = 0.7, label = ~ paste(name, estimate), group = "vars polygons") |>
          leaflet::addLegend(data = filtered_vars(), pal = pal, values = ~ filtered_vars_pal(), title = "estimate", layerId = "vars legend")
      }
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
      
      # setting layerId makes legend reload itself when addLegend is run again rather than creating an additional legend
      leaflet::leafletProxy("map", data = filtered_features()[filtered_features()$features == i,]) |>
        leaflet::addLegend(pal = features_pal, values = ~ filtered_features()$features, title = "feature", layerId = "features legend")
    })
    
    observe({
      if (input$submit_button > 0 & input$submit_button <= num_questions) {
      click <- input$map_click
      latitude <- click$lat
      longitude <- click$lng
      
      map_click_true <- TRUE
      
      leaflet::leafletProxy("map") |>
        leaflet::addPopups(longitude, latitude, "This point is selected.", group = "point selection")
      }
    }) |>
      shiny::bindEvent(input$map_click)
  }
  shiny::shinyApp(ui = ui, server = server)
}
