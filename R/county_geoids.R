globalVariables(c("STATEFP", "NAMELSAD", "STUSPS"))

#' Retrieves county GEOIDs for use in [retrieve_aggregated()]
#' @description Using the tigris
#' [counties()][tigris::counties] and [states()][tigris::states]
#' functions, returns a dataset of counties with their
#' corresponding states and GEOIDs, as designated by the U.S. Census Bureau for
#'  the year 2023, for use in querying the [retrieve_aggregated()] function.
#' @param state Optionally, specify a vector of two-letter state abbreviations to filter
#' to. By default, all Census-designated counties in U.S. states and territories
#'  will be returned by the function.
#' @param sf If `FALSE` (default value), function returns a data frame without
#'  geospatial polygon geometry. If `TRUE`, function returns a shapefile with
#'  geospatial polygon geometry.
#' @param namelsad If `TRUE` (default value), the returned dataset's `name`
#'  variable will use a longer version of county names (e.g.
#'  `"Autauga County, Alabama"`) including each county's LSAD (legal/
#'  statistical area description). If `FALSE`, the returned dataset's `name`
#'  variable will use a shorter version of county names (e.g.
#'  `"Autauga, Alabama"`) excluding each county's LSAD.
#' @param land_area If `FALSE` (default value), the returned dataset does not 
#'  include a land area variable. If `TRUE`, the returned dataset provides land 
#'  areas in square miles.
#' @export

county_geoids <- function(state = NULL, sf = FALSE, namelsad = TRUE, land_area = FALSE) {
  if (namelsad == TRUE) {
    geog_county <- tigris::counties(year = 2023, state = state) |>
      dplyr::select(c(STATEFP, GEOID, NAMELSAD, ALAND)) |>
      dplyr::rename(county = NAMELSAD)
  }

  if (namelsad == FALSE) {
    geog_county <- tigris::counties(year = 2023, state = state) |>
      dplyr::select(c(STATEFP, GEOID, NAME, ALAND)) |>
      dplyr::rename(county = NAME)
  }
  
  if (land_area == FALSE) {
    geog_county <- geog_county |>
      dplyr::select(!ALAND)
  } else {
    geog_county <- geog_county |>
      dplyr::mutate(land_sq_mi = ALAND*0.0000003861) |>
      dplyr::select(!ALAND)
  }

  if (sf == FALSE) {
    geog_county <- sf::st_drop_geometry(geog_county)
  }

  if (sf == TRUE) {
    geog_county <- sf::st_transform(geog_county, crs = 4326)
  }

  geog_state <- tigris::states(year = 2023) |>
    dplyr::select(c(STATEFP, NAME, STUSPS))

  geog_state <- sf::st_drop_geometry(geog_state)

  geog_county <- dplyr::inner_join(geog_county, geog_state, by = dplyr::join_by(STATEFP)) |>
    dplyr::mutate(name = paste0(county, ", ", NAME)) |>
    dplyr::rename(state_abbr = STUSPS) |>
    dplyr::select(!c(county, NAME, STATEFP)) |>
    dplyr::arrange(GEOID)

  return(geog_county)
}
