globalVariables(c("NAMELSAD"))

#' Retrieves CBSA GEOIDs for use in [retrieve_aggregated()]
#' @description Using the tigris
#' [core_based_statistical_areas()][tigris::core_based_statistical_areas]
#' function, returns a dataset of CBSAs (Core Based Statistical
#' Areas) with their corresponding GEOIDs, as designated by the U.S.
#' Census Bureau for the year 2023, for use in querying the [retrieve_aggregated()]
#' function.
#' @param sf If `FALSE` (default value), function returns a data frame without
#'  geospatial polygon geometry. If `TRUE`, function returns a shapefile with
#'  geospatial polygon geometry.
#'  @param namelsad If `TRUE` (default value), the returned dataset's `name`
#'  variable will use a longer version of CBSA names (e.g.
#'  `"Arkadelphia, AR Micro Area"`) including each CBSA's LSAD (legal/
#'  statistical area description). If `FALSE`, the returned dataset's `name`
#'  variable will use a shorter version of CBSA names (e.g.
#'  `"Arkadelphia, AR"`) excluding each CBSA's LSAD.
#' @export

cbsa_geoids <- function(sf = FALSE, namelsad = TRUE) {
  if (namelsad == TRUE) {
    geog_cbsa <- tigris::core_based_statistical_areas(year = 2023) |>
      dplyr::select(c(GEOID, NAMELSAD)) |>
      dplyr::rename(name = NAMELSAD) |>
      dplyr::arrange(GEOID)
  }

  if (namelsad == FALSE) {
    geog_cbsa <- tigris::core_based_statistical_areas(year = 2023) |>
      dplyr::select(c(GEOID, NAME)) |>
      dplyr::rename(name = NAME) |>
      dplyr::arrange(GEOID)
  }

  if (sf == FALSE) {
    geog_cbsa <- sf::st_drop_geometry(geog_cbsa)
  }

  if (sf == TRUE) {
    geog_cbsa <- sf::st_transform(geog_cbsa, crs = 4326)
  }

  return(geog_cbsa)
}
