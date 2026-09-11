globalVariables(c("STATEFP", "STUSPS", "NAMELSAD", "place", "ALAND"))

#' Retrieves place GEOIDs for use in [retrieve_aggregated()]
#' @description Using the tigris
#' [places()][tigris::places] and [states()][tigris::states]
#' functions, returns a dataset of places with their
#' corresponding states and GEOIDs, as designated by the U.S. Census Bureau for
#' the year 2023, for use in querying the [retrieve_aggregated()] function.
#' @param state Optionally, specify a vector of two-letter state abbreviations to filter
#' to. By default, all Census places (cities/towns/villages/census-designated
#' places) in U.S. states and territories will be returned by the function.
#' @param sf If `FALSE` (default value), function returns a data frame without
#'  geospatial polygon geometry. If `TRUE`, function returns a shapefile with
#'  geospatial polygon geometry.
#' @param namelsad If `TRUE` (default value), the returned dataset's `name`
#'  variable will use a longer version of place names (e.g.
#'  `"Hurtsboro town, Alabama"`) including each place's LSAD (legal/statistical
#'  area description). If `FALSE`, the returned dataset's `name`
#'  variable will use a shorter version of place names (e.g.
#'  `"Hurtsboro, Alabama"`) excluding each place's LSAD.
#' @param land_area If `FALSE` (default value), the returned dataset does not
#'  include a land area variable. If `TRUE`, the returned dataset provides land
#'  areas in square miles.
#' @export

place_geoids <- function(state = NULL, sf = FALSE, namelsad = TRUE, land_area = FALSE) {
  if (!is.null(state)) {
    state_test <- state %in% c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")

    if (FALSE %in% state_test) {
      state_vals <- which(state_test == FALSE)

      state_vec <- rep(NA, length(state_vals))

      for (i in 1:length(state_vals)) {
        state_val <- state_vals[i]
        state_vec[i] <- state[state_val]
      }

      stop(paste0("`state` contains the following invalid state abbreviation(s): ", paste(state_vec, collapse = ", "), ". Make sure `state` is set equal to a character vector of two-letter abbreviations for any of the 50 states or DC."))
    }
  }

  if (is.null(sf)) {
    print("`sf` cannot be `NULL`.")
  }

  if (length(sf) != 1) {
    stop("`sf` must have length 1.")
  }

  sf <- as.logical(sf)

  if (!(sf %in% c(TRUE, FALSE))) {
    stop("`sf` must be coercible to logical (`TRUE` or `FALSE`).")
  }

  if (is.null(namelsad)) {
    print("`namelsad` cannot be `NULL`.")
  }

  if (length(namelsad) != 1) {
    stop("`namelsad` must have length 1.")
  }

  namelsad <- as.logical(namelsad)

  if (!(namelsad %in% c(TRUE, FALSE))) {
    stop("`namelsad` must be coercible to logical (`TRUE` or `FALSE`).")
  }

  if (is.null(land_area)) {
    print("`land_area` cannot be `NULL`.")
  }

  if (length(land_area) != 1) {
    stop("`land_area` must have length 1.")
  }

  land_area <- as.logical(land_area)

  if (!(land_area %in% c(TRUE, FALSE))) {
    stop("`land_area` must be coercible to logical (`TRUE` or `FALSE`).")
  }

  geog_state <- tigris::states(year = 2023) |>
    dplyr::select(c(STATEFP, NAME, STUSPS))

  geog_state <- sf::st_drop_geometry(geog_state)

  if (sf == FALSE) {
    if (namelsad == TRUE) {
      geog_place <- tigris::places(year = 2023, state = state, cb = TRUE) |>
        dplyr::select(c(STATEFP, GEOID, NAMELSAD, ALAND)) |>
        dplyr::rename(place = NAMELSAD)
    }

    if (namelsad == FALSE) {
      geog_place <- tigris::places(year = 2023, state = state, cb = TRUE) |>
        dplyr::select(c(STATEFP, GEOID, NAME, ALAND)) |>
        dplyr::rename(place = NAME)
    }

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
    geog_place <- sf::st_sf(GEOID = character(), STATEFP = character(), NAMELSAD = character(), ALAND = numeric(), geometry = list(), crs = "NAD83")

    if (namelsad == TRUE) {
      for (i in state_sf) {
        geog_place_1 <- tigris::places(year = 2023, state = i) |>
          dplyr::select(c(STATEFP, GEOID, NAMELSAD, ALAND)) |>
          dplyr::rename(place = NAMELSAD)

        geog_place <- rbind(geog_place, geog_place_1)
      }
    }

    if (namelsad == FALSE) {
      for (i in state_sf) {
        geog_place_1 <- tigris::places(year = 2023, state = i) |>
          dplyr::select(c(STATEFP, GEOID, NAME, ALAND)) |>
          dplyr::rename(place = NAME)

        geog_place <- rbind(geog_place, geog_place_1)
      }
    }

    geog_place <- sf::st_transform(geog_place, crs = 4326)
  }

  geog_place <- dplyr::inner_join(geog_place, geog_state, by = dplyr::join_by(STATEFP)) |>
    dplyr::mutate(name = paste0(place, ", ", NAME)) |>
    dplyr::rename(state_abbr = STUSPS) |>
    dplyr::select(!c(place, NAME, STATEFP)) |>
    dplyr::arrange(GEOID)

  if (land_area == FALSE) {
    geog_place <- geog_place |>
      dplyr::select(!ALAND)
  } else {
    geog_place <- geog_place |>
      dplyr::mutate(land_sq_mi = ALAND * 0.0000003861) |>
      dplyr::select(!ALAND)
  }

  return(geog_place)
}
