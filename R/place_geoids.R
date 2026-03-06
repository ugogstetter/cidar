globalVariables(c("STATEFP", "STUSPS", "NAMELSAD"))

#' Retrieves place GEOIDs for use in [retrieve_data()]
#' @description Using the tigris 
#' [places()][tigris::places] and [states()][tigris::states]
#' functions, returns a dataset of places with their 
#' corresponding states and GEOIDs, as designated by the U.S. Census Bureau for 
#' the year 2023, for use in querying the [retrieve_data()] function.
#' @param state Optionally, specify a vector of state abbreviations to filter
#' to. By default, all Census places (cities/towns/villages/census-designated 
#' places) in U.S. states and territories will be returned by the function.
#' @param sf If `FALSE` (default value), function returns a data frame without 
#'  geospatial polygon geometry. If `TRUE`, function returns a shapefile with 
#'  geospatial polygon geometry.
#' @export

place_geoids <- function(state = NULL, sf = FALSE) {
  
  geog_state <- tigris::states(year = 2023) |>
    dplyr::select(c(STATEFP, NAME, STUSPS))
  
  geog_state <- sf::st_drop_geometry(geog_state)
  
  if (sf == FALSE) {
  
  geog_place <- tigris::places(year = 2023, state = state, cb = TRUE) |>
    dplyr::select(c(STATEFP, GEOID, NAMELSAD))
  
  geog_place <- sf::st_drop_geometry(geog_place)
  }
  
  # if I do want to produce a shapefile, for places, I have to call each state separately
  if (sf == TRUE) {
    
    if (!is.null(state)) {
      
      state_sf <- state
    }
    
    if (is.null(state)) {
      
      state_sf <- geog_state$STUSPS
    }
    
    # I checked and all the states/territories that would be returned do have the same CRS
    geog_place <- sf::st_sf(GEOID = character(), STATEFP = character(), NAMELSAD = character(), geometry = list(), crs = "NAD83")
    
    for (i in state_sf) {
    
    geog_place_1 <- tigris::places(year = 2023, state = i) |>
      dplyr::select(c(STATEFP, GEOID, NAMELSAD))
    
    geog_place <- rbind(geog_place, geog_place_1)
    }
  }
  
  geog_place <- dplyr::inner_join(geog_place, geog_state, by = dplyr::join_by(STATEFP)) |>
    dplyr::mutate(place = paste0(NAMELSAD, ", ", NAME)) |>
    dplyr::rename(state_abbr = STUSPS) |>
    dplyr::select(!c(NAMELSAD, NAME, STATEFP)) |>
    dplyr::arrange(GEOID)
  
  return(geog_place)
}