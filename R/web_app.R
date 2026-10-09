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
#' @param shapefile_vars Shapefile of polygon data, specifically with numeric variables
#' that can be color-coded, to display on map alongside a slider to select a variable
#'  to display. Shapefile must contain a `"variable"` variable (the web app will include a dropdown for
#'  viewers to select a variable for display, e.g. unemployment, median air quality), an
#'  `"estimate"` variable (for the value or
#'  estimate associated with each value in the `"variable"` column), and a `"name"` variable containing
#'    the names of the geographic areas (e.g. Orange County), which will be
#'    displayed alongside estimates upon mouse-over of the polygons. The
#'  shapefile's geometry must be polygons. If the shapefile contains a `"year"`
#'  variable (which must take more than one possible value), the web app will include a slider for switching between different
#'   years of data. Providing `shapefile_vars` is optional if a shapefile is provided
#'    for `shapefile_features`.
#' @param shapefile_features Shapefile containing point, linestring, and/or polygon 
#' geography types, without a corresponding numeric variable. All points, lines, and polygons in this
#'  shapefile will display simultaneously by default. Shapefile must contain
#'  a `"features"` variable (for the name of the feature category, e.g. park, school) and
#'   a `"name"` variable (for the name of each individual feature, e.g. Central Park).
#'   All observations corresponding
#'   to a given value of the `"features"` variable must be either points, lines, or
#'   polygons (i.e. no mixing of points, lines, and/or polygons within a feature category).
#'   If the shapefile contains a `"year"` variable (which must take more than one possible value), the web app will include a
#'   slider for switching between different years of data. Providing `shapefile_features`
#'    is optional if a shapefile is provided for `shapefile_vars`.
#' @param vars_pal Optionally, define a color palette for displaying the values
#' of the `"estimate"` variable from `shapefile_vars` on the web app map. By default,
#'  the palette is set to `"BuPu"`.
#' @param features_pal Optionally, define a color palette for displaying the values
#' of the `"features"` variable from `shapefile_features` on the web app map. By default,
#'  the palette is set to `"RdGy"`.
#' @param community_data_color Optionally, define a color for the markers and points/lines/polygons
#'  used to denote a feature or location at which community-provided data is present, when the 
#'  viewer selects a question for corresponding community-provided data to be shown. The color 
#'  must be one of those available as a value of `markerColor` for `leaflet::awesomeIcons`. By default, 
#'  the color is set to `"cadetblue"`.
#' @param select_on_click Optionally, provide a vector the same length as `response_questions` 
#' where `TRUE` indicates the corresponding question should be responded to through 
#' selecting an existing point, line, or polygon on the map, and `FALSE` indicates 
#' the corresponding question should be responded through creating a new point on the map. 
#' The default value is `FALSE` for all provided questions.
#' @param allow_multiple_responses By default, this is set to `FALSE`, meaning that 
#' respondents can only answer each question once. Set to `TRUE` to allow for multiple responses 
#' to each question from a single person.
#' @param allow_skips By default, this is set to `FALSE`. Set to `TRUE` to allow 
#' respondents to skip questions.
#' @export

web_app <- function(project_name, title, response_questions, shapefile_vars = NULL, shapefile_features = NULL, vars_pal = "BuPu", features_pal = "RdGy", community_data_color = "cadetblue", select_on_click = rep(FALSE, length(response_questions)), allow_multiple_responses = FALSE, allow_skips = FALSE) {
  
  # if any select_on_click values are `TRUE`, creating unique IDs for each row of each provided dataset so that it can be used to identify selected features
  if (TRUE %in% select_on_click) {
    if (!is.null(shapefile_vars)) {
      shapefile_vars$id <- paste0("vars", as.character(1:nrow(shapefile_vars)))
      
      shapefile_vars <- shapefile_vars |>
        dplyr::mutate(name = dplyr::case_when(is.na(name) ~ "Unnamed",
                                              .default = name))
    }
    if (!is.null(shapefile_features)) {
      shapefile_features$id <- paste0("features", as.character(1:nrow(shapefile_features)))
      
      shapefile_features <- shapefile_features |>
        dplyr::mutate(name = dplyr::case_when(is.na(name) ~ "Unnamed",
                                              .default = name))
    }
  }
  
  vars_palette_defined <- vars_pal
  features_palette_defined <- features_pal
  
  googlesheets4::gs4_auth(token = gargle::secret_read_rds(".secrets/gs4-token.rds", key = "google_sheets_key"))

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
      if (select_on_click[x] == FALSE) {
        googlesheets4::sheet_write(data = data.frame("latitude" = "", "longitude" = "", "comment" = "", "timestamp" = ""), ss = sheet_id, sheet = paste0("q", as.character(x)))
        googlesheets4::range_delete(ss = sheet_id, sheet = paste0("q", as.character(x)), range = "2")
      }
      if (select_on_click[x] == TRUE) {
        googlesheets4::sheet_write(data = data.frame("id" = "", "name" = "", "comment" = "", "timestamp" = ""), ss = sheet_id, sheet = paste0("q", as.character(x)))
        googlesheets4::range_delete(ss = sheet_id, sheet = paste0("q", as.character(x)), range = "2")
      }
    }
    write(paste0(project_name, "=\"", sheet_id, "\""), renv, sep = "\n", append = TRUE)
  }


  if (!is.null(shapefile_vars)) {
    shapefile_vars <- sf::st_transform(shapefile_vars, crs = "WGS84")
    
    if ("year" %in% colnames(shapefile_vars)) {
      vars_yr <- TRUE
    } else {
      vars_yr <- FALSE
    }
    
    if (vars_yr == TRUE) {
      wellPanel_vars <- shiny::wellPanel(
        style = "border-width: 2px; border-color: black",
        shiny::selectInput("variable_input", "Data variable to display", choices = c("none", unique(shapefile_vars$variable)), selected = "none"),
        shiny::conditionalPanel(condition = "input.variable_input != 'none'", shiny::sliderInput("vars_year_input", "Data year to display", min = min(shapefile_vars$year, na.rm = TRUE), max = max(shapefile_vars$year, na.rm = TRUE), value = min(shapefile_vars$year, na.rm = TRUE), step = min(shapefile_vars$year[shapefile_vars$year != min(shapefile_vars$year, na.rm = TRUE)], na.rm = TRUE) - min(shapefile_vars$year, na.rm = TRUE), sep = "")),
        shiny::p("Mouse over an area shown on the map to see its name.")
      )
    }
    if (vars_yr == FALSE) {
      wellPanel_vars <- shiny::wellPanel(
        style = "border-width: 2px; border-color: black",
        shiny::selectInput("variable_input", "Data variable to display", choices = c("none", unique(shapefile_vars$variable)), selected = "none"),
        shiny::p("Mouse over an area shown on the map to see its name.")
      )
    }
  }
  
  if (!is.null(shapefile_features)) {
    shapefile_features <- sf::st_transform(shapefile_features, crs = "WGS84")
    
    if ("year" %in% colnames(shapefile_features)) {
      features_yr <- TRUE
    } else {
      features_yr <- FALSE
    }
    
    if (features_yr == TRUE) {
      wellPanel_features <- shiny::wellPanel(
        style = "border-width: 2px; border-color: black",
        shiny::checkboxGroupInput("features_input", "Features to display", choices = unique(shapefile_features$features), selected = unique(shapefile_features$features)),
        shiny::conditionalPanel(condition = "input.features_input.length != 0", shiny::sliderInput("features_year_input", "Data year to display", min = min(shapefile_features$year, na.rm = TRUE), max = max(shapefile_features$year, na.rm = TRUE), value = min(shapefile_features$year, na.rm = TRUE), step = min(shapefile_features$year[shapefile_features$year != min(shapefile_features$year, na.rm = TRUE)], na.rm = TRUE) - min(shapefile_features$year, na.rm = TRUE), sep = "")),
        shiny::p("Mouse over a feature on the map to see its name.")
      )
    }
    if (features_yr == FALSE) {
      wellPanel_features <- shiny::wellPanel(
        style = "border-width: 2px; border-color: black",
        shiny::checkboxGroupInput("features_input", "Features to display", choices = unique(shapefile_features$features), selected = unique(shapefile_features$features)),
        shiny::p("Mouse over a feature on the map to see its name.")
      )
    }
  }
  
  if (!is.null(shapefile_vars) & !is.null(shapefile_features)) {
    ui <- shiny::fluidPage(
      shiny::titlePanel(title),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          wellPanel_vars,
          wellPanel_features,
          shiny::wellPanel(
            style = "border-width: 2px; border-color: black",
            shiny::selectInput("points_input", "Community data to display", choices = c("none", response_questions), selected = "none"),
            shiny::textOutput("community_data_output")
          )
        ),
        shiny::mainPanel(
          leaflet::leafletOutput("map"),
          shiny::wellPanel(
            style = "border-width: 2px; border-color: black; margin-top: 30px",
            shiny::h4(shiny::textOutput("question_text")),
            shiny::conditionalPanel(condition = "(input.submit_button > 0) && (output.submit_text != 'Submit final response')", shiny::textInput("comment", shiny::textOutput("comment_text"))),
            shiny::conditionalPanel(condition = "(output.submit_text != 'Submit final response')", shiny::actionButton("submit_button", shiny::textOutput("submit_text"))),
            shiny::conditionalPanel(condition = "((input.submit_button > 0) && (output.submit_text != 'Submit final response')", shiny::actionButton("respond_again", "Submit response, then add another response to this question")),
            shiny::conditionalPanel(condition = "((input.submit_button > 0) && (output.submit_text != 'Submit final response')", shiny::actionButton("skip", "Skip this question"))
          )
        )
      )
    )
  }
  
  if (is.null(shapefile_vars)) {
    ui <- shiny::fluidPage(
      shiny::titlePanel(title),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          wellPanel_features,
          shiny::wellPanel(
            style = "border-width: 2px; border-color: black",
            shiny::selectInput("points_input", "Community data to display", choices = c("none", response_questions), selected = "none"),
            shiny::textOutput("community_data_output")
          )
        ),
        shiny::mainPanel(
          leaflet::leafletOutput("map"),
          shiny::wellPanel(
            style = "border-width: 2px; border-color: black; margin-top: 30px",
            shiny::h4(shiny::textOutput("question_text")),
            shiny::conditionalPanel(condition = "(input.submit_button > 0) && (output.submit_text != 'Submit final response')", shiny::textInput("comment", shiny::textOutput("comment_text"))),
            shiny::conditionalPanel(condition = "(output.submit_text != 'Submit final response')", shiny::actionButton("submit_button", shiny::textOutput("submit_text"))),
            shiny::conditionalPanel(condition = "(input.submit_button > 0) && (output.submit_text != 'Submit final response')", shiny::actionButton("respond_again", "Submit response, then add another response to this question")),
            shiny::conditionalPanel(condition = "(input.submit_button > 0) && (output.submit_text != 'Submit final response')", shiny::actionButton("skip", "Skip this question"))
          )
        )
      )
    )
  }
  
  if (is.null(shapefile_features)) {
    ui <- shiny::fluidPage(
      shiny::titlePanel(title),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          wellPanel_vars,
          shiny::wellPanel(
            style = "border-width: 2px; border-color: black",
            shiny::selectInput("points_input", "Community data to display", choices = c("none", response_questions), selected = "none"),
            shiny::textOutput("community_data_output")
          )
        ),
        shiny::mainPanel(
          leaflet::leafletOutput("map"),
          shiny::wellPanel(
            style = "border-width: 2px; border-color: black; margin-top: 30px",
            shiny::h4(shiny::textOutput("question_text")),
            shiny::conditionalPanel(condition = "(input.submit_button > 0) && (output.submit_text != 'Submit final response')", shiny::textInput("comment", shiny::textOutput("comment_text"))),
            shiny::conditionalPanel(condition = "(output.submit_text != 'Submit final response')", shiny::actionButton("submit_button", shiny::textOutput("submit_text"))),
            shiny::conditionalPanel(condition = "((input.submit_button > 0) && (output.submit_text != 'Submit final response')", shiny::actionButton("respond_again", "Submit response, then add another response to this question")),
            shiny::conditionalPanel(condition = "((input.submit_button > 0) && (output.submit_text != 'Submit final response')", shiny::actionButton("skip", "Skip this question"))
          )
        )
      )
    )
  }

  server <- function(input, output) {
    
    if (allow_multiple_responses == FALSE) {
      shiny::removeUI("#respond_again")
    }
    
    if (allow_skips == FALSE) {
      shiny::removeUI("#skip")
    }
    
    question_counter <- shiny::reactive({
      input$submit_button + input$skip
    })
    
    
    output$question_text <- shiny::renderText({
      if (input$submit_button == 0) {
        "Click button below to provide your own data/comments to add to the map"
      } else if (question_counter() <= num_questions) {
        response_questions[question_counter()]
      } else {
        "All responses have been submitted."
      }
    })

    output$submit_text <- shiny::renderText({
      if (input$submit_button == 0) {
        "Click to begin"
      } else if (question_counter() <= num_questions) {
        "Submit response and continue"
      } else {
        "Submit final response"
      }
    })
    
    
    output$comment_text <- shiny::renderText({
      if ((input$submit_button > 0) & (question_counter() <= num_questions)) {
      if (select_on_click[question_counter()] == FALSE) {
        "Answer the question using the comment box below, and click the point corresponding to your response on the map above"
      }
      else {
        "Answer the question using the comment box below, and click the feature corresponding to your response on the map above"
      }
      }
    })
    

    if (!is.null(shapefile_vars) & !is.null(shapefile_features)) {
      
      if (features_yr == TRUE & vars_yr == TRUE) {
        filtered_features <- shiny::reactive({
          shapefile_features[shapefile_features$year == input$features_year_input, ][shapefile_features$features[shapefile_features$year == input$features_year_input] %in% input$features_input, ]
        }) |>
          # binding the event makes the features and their legend reload every time the vars or features are changed, so that the features will stay on top of the vars on the map and so the features legend won't go above the vars legend
          shiny::bindEvent(input$vars_year_input, input$variable_input, input$features_input, input$features_year_input)
      }
  
      if (features_yr == FALSE & vars_yr == TRUE) {
        filtered_features <- shiny::reactive({
          shapefile_features[shapefile_features$features %in% input$features_input, ]
        }) |>
          # binding the event makes the features and their legend reload every time the vars or features are changed, so that the features will stay on top of the vars on the map and so the features legend won't go above the vars legend
          shiny::bindEvent(input$vars_year_input, input$variable_input, input$features_input)
      }
  
      if (features_yr == TRUE & vars_yr == FALSE) {
        filtered_features <- shiny::reactive({
          shapefile_features[shapefile_features$year == input$features_year_input, ][shapefile_features$features[shapefile_features$year == input$features_year_input] %in% input$features_input, ]
        }) |>
          # binding the event makes the features and their legend reload every time the vars or features are changed, so that the features will stay on top of the vars on the map and so the features legend won't go above the vars legend
          shiny::bindEvent(input$variable_input, input$features_input, input$features_year_input)
      }
  
      if (features_yr == FALSE & vars_yr == FALSE) {
        filtered_features <- shiny::reactive({
          shapefile_features[shapefile_features$features %in% input$features_input, ]
        }) |>
          # binding the event makes the features and their legend reload every time the vars or features are changed, so that the features will stay on top of the vars on the map and so the features legend won't go above the vars legend
          shiny::bindEvent(input$variable_input, input$features_input)
      }
    }
    
    if (is.null(shapefile_vars)) {
      
      if (features_yr == TRUE) {
        filtered_features <- shiny::reactive({
          shapefile_features[shapefile_features$year == input$features_year_input, ][shapefile_features$features[shapefile_features$year == input$features_year_input] %in% input$features_input, ]
        })
      }
      
      if (features_yr == FALSE) {
        filtered_features <- shiny::reactive({
          shapefile_features[shapefile_features$features %in% input$features_input, ]
        })
      }
      
    }
    
    if (!is.null(shapefile_features)) {
      features_pal <- leaflet::colorFactor(palette = features_palette_defined, domain = shapefile_features$features)
    }

    if (!is.null(shapefile_vars)) {
      
      vars_none <- shiny::reactive({
        ifelse(input$variable_input == "none", TRUE, FALSE)
      })
  
      filtered_vars_pal <- shiny::reactive({
        shapefile_vars$estimate[shapefile_vars$variable == input$variable_input]
      })
  
      vars_pal <- shiny::reactive({
        leaflet::colorNumeric(palette = vars_palette_defined, domain = filtered_vars_pal())
      })
  
      filtered_vars <- shiny::reactive({
        if (vars_yr == TRUE) {
          shapefile_vars[shapefile_vars$year == input$vars_year_input, ][shapefile_vars$variable[shapefile_vars$year == input$vars_year_input] == input$variable_input, ]
        } else {
          shapefile_vars[shapefile_vars$variable == input$variable_input, ]
        }
      })
    }

    collected_points <- shiny::reactive({
      points_df <- data.frame(latitude = numeric(), longitude = numeric(), comment = character(), timestamp = as.POSIXct(character()), sheet = character())
      for (i in 1:num_questions) {
        if (select_on_click[i] == FALSE) {
        points_df_1 <- googlesheets4::read_sheet(ss = sheet_id, sheet = paste0("q", as.character(i))) |>
          dplyr::mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude), sheet = paste0("q", as.character(i)))
        points_df <- rbind(points_df, points_df_1)
        }
      }
      points_df
    }) |>
      shiny::bindEvent(input$submit_button, input$respond_again, ignoreNULL = TRUE)
    
    collected_shapes <- shiny::reactive({
      shapes_df <- data.frame(id = character(), name = character(), comment = character(), timestamp = as.POSIXct(character()), sheet = character())
      for (i in 1:num_questions) {
        if (select_on_click[i] == TRUE) {
          shapes_df_1 <- googlesheets4::read_sheet(ss = sheet_id, sheet = paste0("q", as.character(i))) |>
            dplyr::mutate(sheet = paste0("q", as.character(i)))
          shapes_df <- rbind(shapes_df, shapes_df_1)
        }
      }
      shapes_df
    }) |>
      shiny::bindEvent(input$submit_button, input$respond_again, ignoreNULL = TRUE)

    collected_points_initial <- data.frame(latitude = numeric(), longitude = numeric(), comment = character(), timestamp = as.POSIXct(character()), sheet = character())
    for (i in 1:num_questions) {
      if (select_on_click[i] == FALSE) {
      collected_points_initial_1 <- googlesheets4::read_sheet(ss = sheet_id, sheet = paste0("q", as.character(i))) |>
        dplyr::mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude), sheet = paste0("q", as.character(i)))
      collected_points_initial <- rbind(collected_points_initial, collected_points_initial_1)
      }
    }
    
    collected_shapes_initial <- data.frame(id = character(), name = character(), comment = character(), timestamp = as.POSIXct(character()), sheet = character())
    for (i in 1:num_questions) {
      if (select_on_click[i] == TRUE) {
        collected_shapes_initial_1 <- googlesheets4::read_sheet(ss = sheet_id, sheet = paste0("q", as.character(i))) |>
          dplyr::mutate(sheet = paste0("q", as.character(i)))
        collected_shapes_initial <- rbind(collected_shapes_initial, collected_shapes_initial_1)
      }
    }

    input_comment <- shiny::reactive({
      input$comment
      })

    map_click_true <- FALSE
    
    if (!is.null(shapefile_vars) & !is.null(shapefile_features)) {
      lng1 <- min(as.numeric(sf::st_bbox(shapefile_vars)$xmin), as.numeric(sf::st_bbox(shapefile_features)$xmin), collected_points_initial$longitude, na.rm = TRUE)
      lat1 <- min(as.numeric(sf::st_bbox(shapefile_vars)$ymin), as.numeric(sf::st_bbox(shapefile_features)$ymin), collected_points_initial$latitude, na.rm = TRUE)
      lng2 <- max(as.numeric(sf::st_bbox(shapefile_vars)$xmax), as.numeric(sf::st_bbox(shapefile_features)$xmax), collected_points_initial$longitude, na.rm = TRUE)
      lat2 <- max(as.numeric(sf::st_bbox(shapefile_vars)$ymax), as.numeric(sf::st_bbox(shapefile_features)$ymax), collected_points_initial$latitude, na.rm = TRUE)
    }
    
    if (is.null(shapefile_vars)) {
      lng1 <- min(as.numeric(sf::st_bbox(shapefile_features)$xmin), collected_points_initial$longitude, na.rm = TRUE)
      lat1 <- min(as.numeric(sf::st_bbox(shapefile_features)$ymin), collected_points_initial$latitude, na.rm = TRUE)
      lng2 <- max(as.numeric(sf::st_bbox(shapefile_features)$xmax), collected_points_initial$longitude, na.rm = TRUE)
      lat2 <- max(as.numeric(sf::st_bbox(shapefile_features)$ymax), collected_points_initial$latitude, na.rm = TRUE)
    }
    
    if (is.null(shapefile_features)) {
      lng1 <- min(as.numeric(sf::st_bbox(shapefile_vars)$xmin), collected_points_initial$longitude, na.rm = TRUE)
      lat1 <- min(as.numeric(sf::st_bbox(shapefile_vars)$ymin), collected_points_initial$latitude, na.rm = TRUE) 
      lng2 <- max(as.numeric(sf::st_bbox(shapefile_vars)$xmax), collected_points_initial$longitude, na.rm = TRUE)
      lat2 <- max(as.numeric(sf::st_bbox(shapefile_vars)$ymax), collected_points_initial$latitude, na.rm = TRUE)
    }

    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles("Stadia.StamenTonerLite") |>
        leaflet.extras2::addGeosearch(options = leaflet.extras2::geosearchOptions(showMarker = FALSE, style = "bar", searchLabel = "Address search", resetButton = "", autoClose = TRUE)) |>
        leaflet::fitBounds(
          lng1 = lng1,
          lat1 = lat1,
          lng2 = lng2,
          lat2 = lat2
        )
    })

    shiny::observe({
      shiny::updateTextInput(inputId = "comment", value = "")
    }) |>
      shiny::bindEvent(input$submit_button, input$skip, input$respond_again)

    shiny::observe({
      if (input$submit_button > 0 & map_click_true == FALSE) {
        shiny::updateActionButton(inputId = "submit_button", disabled = TRUE)
        shiny::updateActionButton(inputId = "respond_again", disabled = TRUE)
      }
    }) |>
      shiny::bindEvent(input$submit_button, input$skip, input$respond_again)

    shiny::observe(
      {
        if (question_counter() > 1) {
          map_click_true <- FALSE

          if (select_on_click[question_counter() - 1] == FALSE) {
            
            data_collected <- data.frame(input$map_click$lat, input$map_click$lng, input$comment, Sys.time())
            googlesheets4::sheet_append(ss = sheet_id, data = data_collected, sheet = paste0("q", as.character(question_counter() - 1)))
          }
          
          if (select_on_click[question_counter() - 1] == TRUE) {
            
            if (!is.null(shapefile_vars)) {
              if (input$map_shape_click$id %in% filtered_vars()$id) {
                sheet_name <- filtered_vars()$name[filtered_vars()$id == input$map_shape_click$id]
              }
            }
            
            if (!is.null(shapefile_features)) {
              if (input$map_shape_click$id %in% filtered_features()$id) {
                sheet_name <- filtered_features()$name[filtered_features()$id == input$map_shape_click$id]
              }
            }
            
            data_collected <- data.frame(input$map_shape_click$id, sheet_name, input$comment, Sys.time())
            googlesheets4::sheet_append(ss = sheet_id, data = data_collected, sheet = paste0("q", as.character(question_counter() - 1)))
          }

            leaflet::leafletProxy("map") |>
              leaflet::clearGroup(group = "point selection")
        }

        if (input$submit_button > 0 & question_counter() <= num_questions) {
          leaflet::leafletProxy("map") |>
            leaflet::addProviderTiles("Stadia.StamenTonerLite", layerId = "map_click", group = "point selection")
        }
      },
      priority = 1
    ) |> shiny::bindEvent(input$submit_button, input$respond_again, ignoreNULL = TRUE)

    shiny::observe({
      if (input$points_input != "none") {
        shiny::observe(
          {
            if (input$points_input != "none") {
              sheet_num <- which(response_questions == input$points_input)

              if (input$submit_button == 0) {
                if (select_on_click[sheet_num] == FALSE) {
                selected_sheet <- collected_points_initial[collected_points_initial$sheet == paste0("q", as.character(sheet_num)), ]
                }
                if (select_on_click[sheet_num] == TRUE) {
                  selected_sheet <- collected_shapes_initial[collected_shapes_initial$sheet == paste0("q", as.character(sheet_num)), ]
                }
              } else {
                if (select_on_click[sheet_num] == FALSE) {
                selected_sheet <- collected_points()[collected_points()$sheet == paste0("q", as.character(sheet_num)), ]
                }
                if (select_on_click[sheet_num] == TRUE) {
                  selected_sheet <- collected_shapes()[collected_shapes()$sheet == paste0("q", as.character(sheet_num)), ]
                }
              }

              if (nrow(selected_sheet) > 0) {
                community_data_color_display <- dplyr::case_when(substr(community_data_color, 1, 5) == "light" ~ substr(community_data_color, 6, nchar(community_data_color)),
                                                                 substr(community_data_color, 1, 4) == "dark" ~ substr(community_data_color, 5, nchar(community_data_color)),
                                                                 substr(community_data_color, 1, 5) == "cadet" ~ substr(community_data_color, 6, nchar(community_data_color)),
                                                                 .default = community_data_color)

                if (select_on_click[sheet_num] == FALSE) {
                  output$community_data_output <- shiny::renderText({
                    paste("Each", community_data_color_display, "marker represents a location about which a previous respondent left a comment corresponding to the question selected. Hover over each marker to see the corresponding comment.")
                  })
                  
                leaflet::leafletProxy("map") |>
                  leaflet::clearGroup(group = c("collected points")) |>
                  leaflet::addAwesomeMarkers(data = selected_sheet, lat = ~latitude, lng = ~longitude, group = "collected points", label = ~lapply(comment, htmltools::HTML), icon = leaflet::awesomeIcons(icon = "map-marker", markerColor = community_data_color, iconColor = "black"))
                }
                
                if (select_on_click[sheet_num] == TRUE) {
                  
                  output$community_data_output <- shiny::renderText({
                    paste("All features with community-collected data available for the selected question are overlaid with", community_data_color_display, "on the map. Additionally, these features are marked with", community_data_color_display, "markers. Hover over each marker to see other people's comments about that location.")
                  })
                  
                  shapes_sf_vars_exists <- FALSE
                  shapes_sf_features_exists <- FALSE
                  
                  for (n in 1:nrow(selected_sheet)) {
                    if (!is.null(shapefile_vars)) {
                    if (selected_sheet$id[n] %in% shapefile_vars$id) {
                      comment_all <- paste(as.vector(selected_sheet[selected_sheet$id == selected_sheet$id[n],]$comment), collapse = "<br>")
                      
                      if (shapes_sf_vars_exists == FALSE) {
                        shapes_sf_vars <- shapefile_vars[shapefile_vars$id == selected_sheet$id[n],] |>
                          dplyr::select(name, geometry, id) |>
                          dplyr::mutate(comment = paste0(toupper(name), "<br>", comment_all))
                        shapes_sf_vars_exists <- TRUE
                      }
                      if (shapes_sf_vars_exists == TRUE) {
                        if (!(selected_sheet$id[n] %in% shapes_sf_vars$id)) {
                        shapes_sf_vars_1 <- shapefile_vars[shapefile_vars$id == selected_sheet$id[n],] |>
                          dplyr::select(name, geometry, id) |>
                          dplyr::mutate(comment = paste0(toupper(name), "<br>", comment_all))
                        shapes_sf_vars <- rbind(shapes_sf_vars, shapes_sf_vars_1)
                      }
                      }
                    }
                    }
                    if (!is.null(shapefile_features)) {
                      if (selected_sheet$id[n] %in% shapefile_features$id) {
                        comment_all <- paste(as.vector(selected_sheet[selected_sheet$id == selected_sheet$id[n],]$comment), collapse = "<br>")
                        
                        if (shapes_sf_features_exists == FALSE) {
                          shapes_sf_features <- shapefile_features[shapefile_features$id == selected_sheet$id[n],] |>
                            dplyr::select(name, geometry, id, features) |>
                            dplyr::mutate(comment = paste0(toupper(features), ": ", toupper(name), "<br>", comment_all))
                          shapes_sf_features_exists <- TRUE
                        }
                        if (shapes_sf_features_exists == TRUE) {
                          if (!(selected_sheet$id[n] %in% shapes_sf_features$id)) {
                            shapes_sf_features_1 <- shapefile_features[shapefile_features$id == selected_sheet$id[n],] |>
                              dplyr::select(name, geometry, id, features) |>
                              dplyr::mutate(comment = paste0(toupper(features), ": ", toupper(name), "<br>", comment_all))
                            shapes_sf_features <- rbind(shapes_sf_features, shapes_sf_features_1)
                          }
                        }
                      }
                    }
                  }
                  
                  leaflet::leafletProxy("map") |>
                    leaflet::clearGroup(group = c("collected points"))
                  
                  if (shapes_sf_features_exists == TRUE) {
                    shapes_sf_features <- shapes_sf_features |>
                      dplyr::mutate(geom_type = as.character(sf::st_geometry_type(shapes_sf_features)))
                    
                    if ("POLYGON" %in% shapes_sf_features$geom_type) {
                      polygon_features <- shapes_sf_features |>
                        dplyr::filter(geom_type == "POLYGON")
                      
                      leaflet::leafletProxy("map", data = polygon_features) |>
                        leaflet::addPolygons(color = community_data_color, fillOpacity = 0.9, label = ~name, group = "collected points", highlightOptions = leaflet::highlightOptions(weight = 10, fillOpacity = 0.95, bringToFront = TRUE))
                    }
                    
                    if ("LINESTRING" %in% shapes_sf_features$geom_type) {
                      line_features <- shapes_sf_features |>
                        dplyr::filter(geom_type == "LINESTRING")
                      
                      leaflet::leafletProxy("map", data = line_features) |>
                        leaflet::addPolylines(color = community_data_color, fillOpacity = 0.9, opacity = 0.9, label = ~name, group = "collected points", highlightOptions = leaflet::highlightOptions(weight = 10, fillOpacity = 0.95, opacity = 0.95, bringToFront = TRUE))
                    }
                    
                    if (("LINESTRING" %in% shapes_sf_features$geom_type) | ("POLYGON" %in% shapes_sf_features$geom_type)) {
                      
                      line_polygon_features <- shapes_sf_features |>
                        dplyr::filter(geom_type %in% c("POLYGON", "LINESTRING"))
                      
                      for (i in 1:nrow(line_polygon_features)) {
                        line_polygon_features$geometry[[i]] <- sf::st_point_on_surface(line_polygon_features$geometry[[i]])
                      }
                      
                      leaflet::leafletProxy("map", data = line_polygon_features) |>
                        leaflet::addAwesomeMarkers(group = "collected points", label = ~lapply(comment, htmltools::HTML), icon = leaflet::awesomeIcons(icon = "map-marker", markerColor = community_data_color, iconColor = "black"))
                    }
                    
                    if ("POINT" %in% shapes_sf_features$geom_type) {
                      point_features <- shapes_sf_features |>
                        dplyr::filter(geom_type == "POINT")
                      
                      leaflet::leafletProxy("map", data = point_features) |>
                        leaflet::addCircles(color = community_data_color, fillOpacity = 0.9, label = ~name, group = "collected points", highlightOptions = leaflet::highlightOptions(weight = 10, fillOpacity = 0.95, bringToFront = TRUE)) |>
                        leaflet::addAwesomeMarkers(group = "collected points", label = ~lapply(comment, htmltools::HTML), icon = leaflet::awesomeIcons(icon = "map-marker", markerColor = community_data_color, iconColor = "black"))
                    }
                  }
                  
                  if (shapes_sf_vars_exists == TRUE) {
                      
                      for (i in 1:nrow(shapes_sf_vars)) {
                        shapes_sf_vars$geometry[[i]] <- sf::st_point_on_surface(shapes_sf_vars$geometry[[i]])
                      }
                      
                      leaflet::leafletProxy("map", data = shapes_sf_vars) |>
                        leaflet::addAwesomeMarkers(group = "collected points", label = ~lapply(comment, htmltools::HTML), icon = leaflet::awesomeIcons(icon = "map-marker", markerColor = community_data_color, iconColor = "black"))
                    }
                  
                }
              } else {
                output$community_data_output <- shiny::renderText({
                  "No data available yet for this selection."
                })

                leaflet::leafletProxy("map") |>
                  leaflet::clearGroup(group = c("collected points"))
              }
            }
          },
          priority = 0
        ) |>
          shiny::bindEvent(input$submit_button, input$respond_again, ignoreNULL = FALSE)
      } else {
        output$community_data_output <- shiny::renderText({
          ""
        })

        leaflet::leafletProxy("map") |>
          leaflet::clearGroup(group = c("collected points"))
      }
    })
    
    if (!is.null(shapefile_vars)) {

      shiny::observe({
        pal <- vars_pal()
  
        leaflet::leafletProxy("map") |>
          leaflet::clearGroup(group = "vars polygons") |>
          leaflet::removeControl(layerId = "vars legend")
  
  
        if (vars_none() == FALSE) {
          # setting layerId makes legend reload itself when addLegend is run again rather than creating an additional legend
          leaflet::leafletProxy("map") |>
            leaflet::addPolygons(data = filtered_vars(), color = ~ pal(estimate), fillOpacity = 0.65, label = ~ paste(name, estimate), group = "vars polygons", layerId = filtered_vars()$id, highlightOptions = leaflet::highlightOptions(weight = 10, fillOpacity = 0.8, bringToFront = TRUE)) |>
            leaflet::addLegend(data = filtered_vars(), pal = pal, values = ~ filtered_vars_pal(), title = "Estimate", layerId = "vars legend")
        }
      })
    }
    
    if (!is.null(shapefile_features)) {
  
        shiny::observe({
          
          if (features_yr == FALSE) {
            
            for (x in unique(shapefile_features$features)) {
              if (!(x %in% unique(filtered_features()$features))) {
                leaflet::leafletProxy("map") |>
                  leaflet::clearGroup(group = x)
              }
            }
      
            for (i in unique(filtered_features()$features)) {
              if (unique(sf::st_geometry_type(filtered_features()[filtered_features()$features == i, ])) == "POLYGON") {
                leaflet::leafletProxy("map", data = filtered_features()[filtered_features()$features == i, ]) |>
                  leaflet::addPolygons(group = i, label = ~name, color = ~ features_pal(features), fillOpacity = 0.8, layerId = ~filtered_features()[filtered_features()$features == i, ]$id, highlightOptions = leaflet::highlightOptions(weight = 10, fillOpacity = 0.95, bringToFront = TRUE))
              }
      
              if (unique(sf::st_geometry_type(filtered_features()[filtered_features()$features == i, ])) == "POINT") {
                leaflet::leafletProxy("map", data = filtered_features()[filtered_features()$features == i, ]) |>
                  leaflet::addCircles(group = i, label = ~name, color = ~ features_pal(features), fillOpacity = 0.8, layerId = ~filtered_features()[filtered_features()$features == i, ]$id, highlightOptions = leaflet::highlightOptions(weight = 10, fillOpacity = 0.95, bringToFront = TRUE))
              }
              
              if (unique(sf::st_geometry_type(filtered_features()[filtered_features()$features == i, ])) == "LINESTRING") {
                leaflet::leafletProxy("map", data = filtered_features()[filtered_features()$features == i, ]) |>
                  leaflet::addPolylines(group = i, label = ~name, color = ~ features_pal(features), opacity = 0.8, fillOpacity = 0.8, layerId = ~filtered_features()[filtered_features()$features == i, ]$id, highlightOptions = leaflet::highlightOptions(weight = 10, opacity = 0.95, fillOpacity = 0.95, bringToFront = TRUE))
              }
            }
          }
          
          if (features_yr == TRUE) {
            
            for (y in unique(shapefile_features$year)) {
              
              if (!(y %in% unique(filtered_features()$year))) {
                for (x in unique(shapefile_features$features)) {
                  leaflet::leafletProxy("map") |>
                    leaflet::clearGroup(group = paste0(as.character(y), x))
                  }
              } else {
                for (x in unique(shapefile_features$features)) {
                  if (!(x %in% unique(filtered_features()$features))) {
                    leaflet::leafletProxy("map") |>
                      leaflet::clearGroup(group = paste0(as.character(y), x))
                  }
                }
              }
            }
            
            for (z in unique(filtered_features()$year)) {
            for (i in unique(filtered_features()$features)) {
              if (unique(sf::st_geometry_type(filtered_features()[filtered_features()$features == i, ][filtered_features()$year[filtered_features()$features == i] %in% input$features_year_input,])) == "POLYGON") {
                leaflet::leafletProxy("map", data = filtered_features()[filtered_features()$features == i, ]) |>
                  leaflet::addPolygons(group = paste0(as.character(z), i), label = ~name, color = ~ features_pal(features), fillOpacity = 0.8, layerId = ~filtered_features()[filtered_features()$features == i, ]$id, highlightOptions = leaflet::highlightOptions(weight = 10, fillOpacity = 0.95, bringToFront = TRUE))
              }
              
              if (unique(sf::st_geometry_type(filtered_features()[filtered_features()$features == i, ][filtered_features()$year[filtered_features()$features == i] %in% input$features_year_input,])) == "POINT") {
                leaflet::leafletProxy("map", data = filtered_features()[filtered_features()$features == i, ]) |>
                  leaflet::addCircles(group = paste0(as.character(z), i), label = ~name, color = ~ features_pal(features), fillOpacity = 0.8, layerId = ~filtered_features()[filtered_features()$features == i, ]$idid, highlightOptions = leaflet::highlightOptions(weight = 10, fillOpacity = 0.95, bringToFront = TRUE))
              }
              
              if (unique(sf::st_geometry_type(filtered_features()[filtered_features()$features == i, ][filtered_features()$year[filtered_features()$features == i] %in% input$features_year_input,])) == "LINESTRING") {
                leaflet::leafletProxy("map", data = filtered_features()[filtered_features()$features == i, ]) |>
                  leaflet::addPolylines(group = paste0(as.character(z), i), label = ~name, color = ~ features_pal(features), opacity = 0.8, fillOpacity = 0.8, layerId = ~filtered_features()[filtered_features()$features == i, ]$idid, highlightOptions = leaflet::highlightOptions(weight = 10, opacity = 0.95, fillOpacity = 0.95, bringToFront = TRUE))
              }
            }
            }
          }
            
    
        # setting layerId makes legend reload itself when addLegend is run again rather than creating an additional legend
        leaflet::leafletProxy("map") |>
          leaflet::addLegend(data = filtered_features(), pal = features_pal, values = ~ shapefile_features$features, title = "Feature", layerId = "features legend")
  
        if (nrow(filtered_features()) == 0) {
          leaflet::leafletProxy("map") |>
            leaflet::removeControl(layerId = "features legend")
        }
      })
    }

    shiny::observe({
      if (input$submit_button > 0 & question_counter() <= num_questions) {
        
      if (select_on_click[question_counter()] == FALSE) {

          click <- input$map_click
          latitude <- click$lat
          longitude <- click$lng
          
          map_click_true <- TRUE
          
          shiny::updateActionButton(inputId = "submit_button", disabled = FALSE)
          shiny::updateActionButton(inputId = "respond_again", disabled = FALSE)
          
          leaflet::leafletProxy("map") |>
            leaflet::addPopups(longitude, latitude, "This point is selected.", group = "point selection", options = leaflet::popupOptions(closeButton = FALSE))
      }
      }
    }) |>
      shiny::bindEvent(input$map_click)
    
    shiny::observe({
      if (input$submit_button > 0 & question_counter() <= num_questions) {
      if (select_on_click[question_counter()] == TRUE) {
        
        event <- input$map_shape_click
        latitude <- event$lat
        longitude <- event$lng
        
        map_click_true <- TRUE
        
        shiny::updateActionButton(inputId = "submit_button", disabled = FALSE)
        shiny::updateActionButton(inputId = "respond_again", disabled = FALSE)
        
        if (!is.null(shapefile_vars)) {
        if (event$id %in% filtered_vars()$id) {
          clicked_name <- filtered_vars()$name[filtered_vars()$id == event$id]
        }
        }
        
        if (!is.null(shapefile_features)) {
        if (event$id %in% filtered_features()$id) {
          clicked_name <- filtered_features()$name[filtered_features()$id == event$id]
        }
        }
        
        if (is.na(clicked_name)) {
          clicked_name <- "An unnamed shape"
        }
        
        leaflet::leafletProxy("map") |>
          leaflet::addPopups(longitude, latitude, paste(clicked_name, "is selected."), group = "point selection", options = leaflet::popupOptions(closeButton = FALSE))
      }
      }
    }) |>
      shiny::bindEvent(input$map_shape_click)
  }
  shiny::shinyApp(ui = ui, server = server)
}
