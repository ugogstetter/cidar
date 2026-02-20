#' Assesses travel times to various destination types
#' @description Provides travel times by foot, bicycle, or car to destination 
#' types, including schools, (will add more), in a specified area. Utilizes the
#'  TravelTime API, called through the traveltimeR package. Requires your 
#'  TravelTime application ID and application key to already be saved, which can
#'   be done using [travel_time_key()].
#' @param destination Destination type to which to calculate travel time. 
#' Available destination types include: `"schools"`, `"public schools"`, and 
#' `"private schools"`.
#' @param geog Type of area in which to compute travel time radii from selected
#'  destination type. Specify `"county"`, `"cbsa"`, or `"zip code"`.
#' @param geoselect Supply a specific county GEOID (if `geog` is set to 
#' `"county"`), CBSA GEOID (if `geog` is set to `"cbsa"`), or zip code (if 
#' `geog` is set to `"zip code"`) within which to compute travel time radii from
#'  selected destination type. To determine the corresponding GEOID for a
#'   particular county or CBSA, use [county_geoids()] or [cbsa_geoids()], 
#'   respectively.
#' @param mins Number of minutes traveling.
#' @param mode Mode of travel. Available modes include: `"driving"`, `"cycling"`
#' , and `"walking"`.
#' @export

travel_time <- function(destination, geog, geoselect, mins, mode) {
  
  if (destination == "schools") {

    temp <- tempfile()
    
    utils::download.file("https://nces.ed.gov/programs/edge/data/EDGE_GEOCODE_PUBLICSCH_2324.zip", temp)
    
    temp2 <- tempfile()
    
    public_schools <- readxl::read_xlsx(utils::unzip(temp, files = "EDGE_GEOCODE_PUBLICSCH_2324/EDGE_GEOCODE_PUBLICSCH_2324.xlsx", exdir = temp2)) |>
      dplyr::select(c(NAME, STREET, CITY, STATE, ZIP, CNTY, LAT, LON, CBSA)) |>
      dplyr::mutate(school = paste0(NAME, ", ", STREET, ", ", CITY, ", ", STATE, " ", ZIP, " (public school)"), county = CNTY, zip_code = ZIP, lat = LAT, lng = LON, cbsa = CBSA) |>
      dplyr::select(!c(NAME, STREET, CITY, STATE, ZIP, CNTY, LAT, LON, CBSA))
    
    unlink(temp)
    
    unlink(temp2)
    
    
    temp <- tempfile()
    
    utils::download.file("https://nces.ed.gov/programs/edge/data/EDGE_GEOCODE_PRIVATESCH_2324.zip", temp)
    
    temp2 <- tempfile()
    
    private_schools <- readxl::read_xlsx(utils::unzip(temp, files = "EDGE_GEOCODE_PRIVATESCH_2324.xlsx", exdir = temp2)) |>
      dplyr::select(c(NAME, STREET, CITY, STATE, ZIP, CNTY, LAT, LON, CBSA)) |>
      dplyr::mutate(school = paste0(NAME, ", ", STREET, ", ", CITY, ", ", STATE, " ", ZIP, " (private school)"), county = CNTY, zip_code = ZIP, lat = LAT, lng = LON, cbsa = CBSA) |>
      dplyr::select(!c(NAME, STREET, CITY, STATE, ZIP, CNTY, LAT, LON, CBSA))
    
    unlink(temp)
    
    unlink(temp2)
    
    # there are some schools which have the same name and address, and the same latitude and longitude within at least 1 decimal place. so I am combining those into one observation
    public_schools <- dplyr::distinct(public_schools, school, .keep_all = TRUE)
    private_schools <- dplyr::distinct(private_schools, school, .keep_all = TRUE)
    
    destinations <- rbind(public_schools, private_schools)
  }
  
  
  if (destination == "public schools") {
    
    temp <- tempfile()
    
    utils::download.file("https://nces.ed.gov/programs/edge/data/EDGE_GEOCODE_PUBLICSCH_2324.zip", temp)
    
    temp2 <- tempfile()
    
    public_schools <- readxl::read_xlsx(utils::unzip(temp, files = "EDGE_GEOCODE_PUBLICSCH_2324/EDGE_GEOCODE_PUBLICSCH_2324.xlsx", exdir = temp2)) |>
      dplyr::select(c(NAME, STREET, CITY, STATE, ZIP, CNTY, LAT, LON, CBSA)) |>
      dplyr::mutate(school = paste0(NAME, ", ", STREET, ", ", CITY, ", ", STATE, " ", ZIP), county = CNTY, zip_code = ZIP, lat = LAT, lng = LON, cbsa = CBSA) |>
      dplyr::select(!c(NAME, STREET, CITY, STATE, ZIP, CNTY, LAT, LON, CBSA))
    
    unlink(temp)
    
    unlink(temp2)
    
    # there are some schools which have the same name and address, and the same latitude and longitude within at least 1 decimal place. so I am combining those into one observation
    destinations <- dplyr::distinct(public_schools, school, .keep_all = TRUE)
  }
  
  
  if (destination == "private schools") {
    
    temp <- tempfile()
    
    utils::download.file("https://nces.ed.gov/programs/edge/data/EDGE_GEOCODE_PRIVATESCH_2324.zip", temp)
    
    temp2 <- tempfile()
    
    private_schools <- readxl::read_xlsx(utils::unzip(temp, files = "EDGE_GEOCODE_PRIVATESCH_2324.xlsx", exdir = temp2)) |>
      dplyr::select(c(NAME, STREET, CITY, STATE, ZIP, CNTY, LAT, LON, CBSA)) |>
      dplyr::mutate(school = paste0(NAME, ", ", STREET, ", ", CITY, ", ", STATE, " ", ZIP), county = CNTY, zip_code = ZIP, lat = LAT, lng = LON, cbsa = CBSA) |>
      dplyr::select(!c(NAME, STREET, CITY, STATE, ZIP, CNTY, LAT, LON, CBSA))
    
    unlink(temp)
    
    unlink(temp2)
    
    # there are some schools which have the same name and address, and the same latitude and longitude within at least 1 decimal place. so I am combining those into one observation
    destinations <- dplyr::distinct(private_schools, school, .keep_all = TRUE)
  }

  
  if (geog == "county") {
    
    destinations_geog <- destinations |>
      dplyr::filter(county == geoselect)
  }
  
  if (geog == "cbsa") {
    
    destinations_geog <- destinations |>
      dplyr::filter(cbsa == geoselect)
  }
  
  if (geog == "zip code") {
    
    destinations_geog <- destinations |>
      dplyr::filter(zip_code == geoselect)
  }

  
  # initializing shapefile for use in for loop below
  sf_all <- sf::st_sf(search_id = character(), geometry = list(), crs = 4326)
  
  # I decided to use the travel_map_fast function rather than the normal travel mapping function because the fast function allows me to put in a general timeframe of arrival rather than a specific time and date(and the general timeframe is only allowed to be weekday morning). also, the normal travel mapping function does count each individual departure/arrival search as one hit (even when run together in the time_map function), so it doesn't seem like I'd be able to do more before hitting the limit with the normal function compared to the fast function. the limit for the free API version is (after the initial trial period) 5 hits per minute
  for (n in 1:ceiling(nrow(destinations_geog)/5)) {
  
    for (i in ((n*5) - 4):ifelse(n*5 > nrow(destinations_geog), nrow(destinations_geog), n*5)) {
      
      arrival_search <-
        traveltimeR::make_search(id = destinations_geog[[i, 1]],
                    travel_time = 60*mins,
                    coords = list(lat = destinations_geog[[i, 4]], lng = destinations_geog[[i, 5]]),
                    arrival_time_period = "weekday_morning",
                    transportation = list(type = mode))
      
      result <-
        traveltimeR::time_map_fast(
          arrival_many_to_one = arrival_search,
          format = "application/geo+json"
        )
      
      sf_1 <- sf::st_read(result$contentJSON)
      
      sf_all <- rbind(sf_all, sf_1)
    }
    
    if (n < ceiling(nrow(destinations_geog)/5)) {Sys.sleep(60)}
  }
  
  sf_all <- sf_all |>
    dplyr::rename(destination = search_id)
  
  return(sf_all)
}