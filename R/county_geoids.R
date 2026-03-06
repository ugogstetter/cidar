globalVariables(c("STATEFP", "NAMELSAD", "STUSPS"))

#' Retrieves county GEOIDs for use in [retrieve_data()] and [travel_time()]
#' @description Using the tigris 
#' [counties()][tigris::counties] and [states()][tigris::states]
#' functions, returns a dataset of counties with their 
#' corresponding states and GEOIDs, as designated by the U.S. Census Bureau for
#'  the year 2023, for use in querying the [retrieve_data()] and [travel_time()]
#'   functions.
#' @param state Optionally, specify a vector of state abbreviations to filter
#' to. By default, all Census-designated counties in U.S. states and territories
#'  will be returned by the function.
#' @param sf If `FALSE` (default value), function returns a data frame without 
#'  geospatial polygon geometry. If `TRUE`, function returns a shapefile with 
#'  geospatial polygon geometry.
#' @export

county_geoids <- function(state = NULL, sf = FALSE) {
  
  geog_county <- tigris::counties(year = 2023, state = state) |>
    dplyr::select(c(STATEFP, GEOID, NAMELSAD))
  
  if (sf == FALSE) {geog_county <- sf::st_drop_geometry(geog_county)}
  
  geog_state <- tigris::states(year = 2023) |>
    dplyr::select(c(STATEFP, NAME, STUSPS))
  
  geog_state <- sf::st_drop_geometry(geog_state)
  
  geog_county <- dplyr::inner_join(geog_county, geog_state, by = dplyr::join_by(STATEFP)) |>
    dplyr::mutate(county = paste0(NAMELSAD, ", ", NAME)) |>
    dplyr::rename(state_abbr = STUSPS) |>
    dplyr::select(!c(NAMELSAD, NAME, STATEFP)) |>
    dplyr::arrange(GEOID)
  
    return(geog_county)
}