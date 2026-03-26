globalVariables(c("NAMELSAD"))

#' Retrieves CBSA GEOIDs for use in [retrieve_aggregated()] and [travel_time()]
#' @description Using the tigris
#' [core_based_statistical_areas()][tigris::core_based_statistical_areas]
#' function, returns a dataset of CBSAs (Core Based Statistical
#' Areas) with their corresponding GEOIDs, as designated by the U.S.
#' Census Bureau for the year 2023, for use in querying the [retrieve_aggregated()]
#' and [travel_time()] functions.
#' @param sf If `FALSE` (default value), function returns a data frame without
#'  geospatial polygon geometry. If `TRUE`, function returns a shapefile with
#'  geospatial polygon geometry.
#' @export

cbsa_geoids <- function(sf = FALSE) {
  geog_cbsa <- tigris::core_based_statistical_areas(year = 2023) |>
    dplyr::select(c(GEOID, NAMELSAD)) |>
    dplyr::rename(name = NAMELSAD) |>
    dplyr::arrange(GEOID)

  if (sf == FALSE) {
    geog_cbsa <- sf::st_drop_geometry(geog_cbsa)
  }

  return(geog_cbsa)
}
