#' Retrieves county GEOIDs for use in [retrieve_data()] and [travel_time()]
#' @description Returns a table of U.S. counties with their corresponding 
#' GEOIDs, for use in querying the [retrieve_data()] and [travel_time()] 
#' functions. If you have not already set a U.S. Census Bureau API key using 
#' [census_key()], do so before running this function.
#' @param state Optionally, specify a vector of state abbreviations to filter
#' to. By default, counties in all U.S. states will be returned by the function.
#' @export

county_geoids <- function(state = NULL) {
  
  tidycensus::census_api_key(Sys.getenv("census_key_value"))
  
    geog_county <- tidycensus::fips_codes |>
      dplyr::mutate(
        GEOID = paste0(state_code, county_code),
        county = paste0(county, ", ", state_name)
      ) |>
      dplyr::rename(state_abbr = state) |>
      dplyr::select(c(GEOID, county, state_abbr))
    
    if (!is.null(state)) {
      
      geog_county <- geog_county |>
        dplyr::filter(state_abbr %in% state)
    }
  
    return(geog_county)
}