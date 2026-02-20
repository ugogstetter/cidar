#' Retrieves CBSA GEOIDs for use in [retrieve_data()] and [travel_time()]
#' @description Returns a table of U.S. CBSAs (Core Based Statistical Areas) 
#' with their corresponding GEOIDs, for use in querying the [retrieve_data()] 
#' and [travel_time()] functions. If you have not already set a U.S. Census 
#' Bureau API key using [census_key()], do so before running this function.
#' @export

cbsa_geoids <- function() {
  
  tidycensus::census_api_key(Sys.getenv("census_key_value"))
  
  geog_cbsa <- tidycensus::get_acs(geography = "cbsa", variables = "DP03_0005P", year = 2023) |>
    dplyr::select(c(GEOID, NAME)) |>
    dplyr::rename(cbsa = NAME) |>
    dplyr::distinct()
  
  return(geog_cbsa)
}