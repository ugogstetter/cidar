#' Retrieves shapefile of point or polygon features from chosen source
#' @description Retrieves shapefile of point or polygon features of user-specified data category. (discuss data sources)
#' @param features Specify a vector of feature sets to retrieve. Before supplying this argument, 
#' ensure you have read the (link to retrieve_point_polygon_features) function description, and have run that function to
#' return the data frame of retrievable features.
#' @param geog Optionally, specify a geographic area to filter to. Can be 
#' `"state"`, `"county"`, `"cbsa"` (core based statistical
#' area), `"place"`, or `"zip code"`. If no value is provided, a nationwide 
#' dataset will be returned. Please see (link to retrieve_point_polygon_features) to
#'  determine whether a particular feature set is able to be filtered to a particular 
#'  geographic area, and only specify a geographic area that all selected feature
#'   sets can be filtered to.
#' @param geoselect If a value is supplied for `geog`, supply a vector of state abbreviations, county,
#'  CBSA, or place GEOIDS, or zip codes (depending on whether `geog` is set to `"state"`, 
#'  `"county"`, `"cbsa"` or `"place"`, or `"zip code"`) to filter to. By default, all possible
#'   values will be returned by the
#' function. To determine the corresponding GEOID for a particular county, CBSA,
#'  or place, use [county_geoids()], [cbsa_geoids()], or [place_geoids()],
#'  respectively. Note that for polygon features, all features intersecting the 
#'  specified geographic area will be returned, not just those fully inside of it.
#' @param dataset_info Specify whether to append columns containing
#'  category, source, and type information for the dataset in this function's
#'   output. Default value is `TRUE`. The appended information will
#'   be the same as that contained in the (link to retrieve_point_polygon_features) data frame.
#' @export

retrieve_point_polygon <- function(features, geog = NULL, geoselect = NULL, dataset_info = TRUE) {
  
  empty_sf <- sf::st_sf(features = character(), name = character(), type = character(), address = character(), geometry = list(), crs = 4326)
  
  if ("parks" %in% features) {
    
    parks <- arcgislayers::arc_select(arcgislayers::arc_open("https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Detailed_Parks/FeatureServer/0"), fields = c("NAME", "FEATTYPE")) |>
      dplyr::rename(name = NAME, type = FEATTYPE)
    
    if (geog == "state") {
      
      geog_state <- tigris::states(year = 2023) |>
        dplyr::filter(STUSPS %in% geoselect)
      
      geog_state <- sf::st_transform(geog_state, crs = 4326)
      
      parks_intersect <- sf::st_intersection(parks, geog_state)
      
      parks <- parks |>
        dplyr::filter(row.names(parks) %in% rownames(parks_intersect))
    }
    
    if (geog == "county") {
      
      # creating vector of state abbreviations just for requested counties so tigris call will run faster
      county_states <- as.vector(substr(geoselect, 1, 2))
      
      geog_county <- tigris::counties(year = 2023, state = county_states) |>
        dplyr::filter(GEOID %in% geoselect)
      
      geog_county <- sf::st_transform(geog_county, crs = 4326)
      
      parks_intersect <- sf::st_intersection(parks, geog_county)
      
      parks <- parks |>
        dplyr::filter(row.names(parks) %in% rownames(parks_intersect))
    }
    
    if (geog == "cbsa") {
      
      geog_cbsa <- tigris::core_based_statistical_areas(year = 2023) |>
        dplyr::filter(GEOID %in% geoselect)
      
      geog_cbsa <- sf::st_transform(geog_cbsa, crs = 4326)
      
      parks_intersect <- sf::st_intersection(parks, geog_cbsa)
      
      parks <- parks |>
        dplyr::filter(row.names(parks) %in% rownames(parks_intersect)) 
    }
    
    if (geog == "place") {
      
      # creating vector of state abbreviations just for requested places so tigris call will run faster
      place_states <- as.vector(substr(geoselect, 1, 2))
      
      # I checked and all the states/territories that would be returned do have the same CRS
      geog_place <- sf::st_sf(GEOID = character(), STATEFP = character(), NAMELSAD = character(), geometry = list(), crs = "NAD83")
      
      for (i in place_states) {
        geog_place_1 <- tigris::places(year = 2023, state = i)
        
        geog_place <- rbind(geog_place, geog_place_1)
      }
      
      geog_place <- geog_place |>
        dplyr::filter(GEOID %in% geoselect)
      
      geog_place <- sf::st_transform(geog_place, crs = 4326)
      
      parks_intersect <- sf::st_intersection(parks, geog_place)
      
      parks <- parks |>
        dplyr::filter(row.names(parks) %in% rownames(parks_intersect))
    }
    parks <- parks |>
      dplyr::mutate(features = "parks", address = NA)
    
  } else {parks <- empty_sf}
  
  if ("schools" %in% features) {
    
    if (geog == "state") {where_clause <- paste0("STATE IN ('", paste(geoselect, collapse = "', '"), "')")}
    
    if (geog == "county") {where_clause <- paste0("CNTY IN ('", paste(geoselect, collapse = "', '"), "')")}
    
    if (geog == "cbsa") {where_clause <- paste0("CBSA IN ('", paste(geoselect, collapse = "', '"), "')")}
    
    if (geog == "place") {
      
      # creating vector of state abbreviations just for requested places so function will run faster
      place_states <- as.vector(substr(geoselect, 1, 2))
      
      where_clause <- paste0("STFIP IN ('", paste(place_states, collapse = "', '"), "')")
    }
    
    if (geog == "zip code") {where_clause <- paste0("ZIP IN ('", paste(geoselect, collapse = "', '"), "')")}
    
    public_schools <- arcgislayers::arc_select(arcgislayers::arc_open("https://services1.arcgis.com/Ua5sjt3LWTPigjyD/ArcGIS/rest/services/Public_School_Locations_Current/FeatureServer/0"), fields = c("NAME", "STREET", "CITY", "STATE", "ZIP"), where = where_clause) |>
      dplyr::mutate(name = NAME, type = "public", address = paste0(STREET, ", ", CITY, ", ", STATE, " ", ZIP))
    
    private_schools <- arcgislayers::arc_select(arcgislayers::arc_open("https://services1.arcgis.com/Ua5sjt3LWTPigjyD/ArcGIS/rest/services/Private_School_Locations_Current/FeatureServer/0"), fields = c("NAME", "STREET", "CITY", "STATE", "ZIP"), where = where_clause) |>
      dplyr::mutate(name = NAME, type = "private", address = paste0(STREET, ", ", CITY, ", ", STATE, " ", ZIP))
    
    schools <- rbind(public_schools, private_schools)
    
    schools <- sf::st_transform(schools, crs = 4326)
    
    if (geog == "place") {
      
      # I checked and all the states/territories that would be returned do have the same CRS
      geog_place <- sf::st_sf(GEOID = character(), STATEFP = character(), NAMELSAD = character(), geometry = list(), crs = "NAD83")
      
      for (i in place_states) {
        geog_place_1 <- tigris::places(year = 2023, state = i)
        
        geog_place <- rbind(geog_place, geog_place_1)
      }
      
      geog_place <- geog_place |>
        dplyr::filter(GEOID %in% geoselect)
      
      geog_place <- sf::st_transform(geog_place, crs = 4326)
      
      schools_intersect <- sf::st_intersection(schools, geog_place)
      
      schools <- schools |>
        dplyr::filter(row.names(schools) %in% rownames(schools_intersect)) 
    }
    
    schools <- schools |>
      dplyr::select(!c(NAME, STREET, CITY, STATE, ZIP)) |>
      dplyr::mutate(features = "schools")
    
  } else {schools <- empty_sf}
  
  sf_all <- rbind(schools, parks)
  
  return(sf_all)
}
