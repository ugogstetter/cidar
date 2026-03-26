globalVariables(c("geometry_type", "features"))

#' Returns table of geospatial feature sets retrievable through [retrieve_point_polygon()]
#' @description Returns a table with all point and polygon feature sets 
#' retrievable through the [retrieve_point_polygon()] function. For each set of 
#' features, the table lists the geometry type (point or polygon), whether 
#' an API key is required for data retrieval, what the data source is, what 
#' subtypes the feature set is categorized into ("types"), what year the feature set is 
#' from (this will either be the most recent year available, or say that the 
#' dataset is updated regularly), and what geography types
#'  the feature set can be filtered by. If an API key is required
#' to retrieve a certain variable, run the indicated function for setting the
#' API key before attempting to retrieve the variable for the first time
#' using [retrieve_point_polygon()]. Once set, the API key will be saved across R
#' sessions.
#' @export

retrieve_point_polygon_features <- function() {
  features_df <- data.frame(
    features = c("parks", "transit stops", "schools", "libraries"),
    geometry_type = c("polygon", rep("point", 3)),
    api_key_required = c(rep("no", 4)),
    source = c("TomTom, ArcGIS Data and Maps - ArcGIS Online", "National Transportation Atlas Database, Bureau of Transportation Statistics, U.S. Department of Transportation - ArcGIS Online", "National Center for Education Statistics - ArcGIS Online", "Institute of Museum and Library Services"),
    types = c("local park; national park or forest; regional park; state park or forest; county park", "bus; tram, streetcar, light rail; rail; ferry; subway, metro; trolleybus; funicular; aerial lift, suspended cable car; cable tram; monorail", "public; private", "central library; branch library; bookmobile(s); books-by-mail only"),
    year = c("updated annually", "updated regularly", "public schools updated annually; private schools updated bi-annually", "2023"),
    geogs_available = c(rep("state, county, cbsa, place", 2), rep("state, county, cbsa, place, zip code", 2))
  ) |>
    dplyr::arrange(geometry_type, features)
  
  return(features_df)
}