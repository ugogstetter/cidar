#' Establishes a U.S. Census Bureau API key for [retrieve_data()]
#' @description If you haven't yet registered for a Census Bureau API key, follow the instructions at https://api.census.gov/data/key_signup.html.
#' @param key Supply your Census Bureau API key.
#' @export

census_key <- function(key) {
  assign("census_key_value", key, 1, envir = .GlobalEnv)
}