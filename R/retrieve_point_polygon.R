globalVariables(c("FEATTYPE", "name", "stop_name", "stop_type_text", "STREET", "CITY", "STATE", "ZIP", "STABR", "CENTRACT", "LIBNAME", "ADDRESS", "address", "geometry"))

#' Retrieves shapefile of point or polygon features from chosen source
#' @description Retrieves shapefile of point or polygon features of user-specified data category,
#'  from data sources such as ArcGIS Online and the Institute of Museum and Library Services.
#' @param features Specify a vector of feature sets to retrieve. Before supplying this argument, 
#' ensure you have read the [retrieve_point_polygon_features()] function description, and have run that function to
#' return the data frame of retrievable features.
#' @param geog Optionally, specify a geographic area to filter to. Can be 
#' `"state"`, `"county"`, `"cbsa"` (core based statistical
#' area), `"place"`, or `"zip code"`. If no value is provided, a nationwide 
#' dataset will be returned. Please see [retrieve_point_polygon_features()] to
#'  determine whether a particular feature set is able to be filtered to a particular 
#'  geographic area, and only specify a geographic area that all selected feature
#'   sets can be filtered to.
#' @param geoselect If a value is supplied for `geog`, supply a vector (in character format) of state abbreviations, county,
#'  CBSA, or place GEOIDS, or zip codes (depending on whether `geog` is set to `"state"`, 
#'  `"county"`, `"cbsa"` or `"place"`, or `"zip code"`) to filter to. By default, all possible
#'   values will be returned by the
#' function. To determine the corresponding GEOID for a particular county, CBSA,
#'  or place, use [county_geoids()], [cbsa_geoids()], or [place_geoids()],
#'  respectively. Note that for polygon features, all features intersecting the 
#'  specified geographic area will be returned, not just those fully inside of it.
#' @param dataset_info Specify whether to append columns containing
#'  source and year information for the dataset in this function's
#'   output. Default value is `TRUE`. The appended information will
#'   be the same as that contained in the [retrieve_point_polygon_features()] data frame.
#' @export

retrieve_point_polygon <- function(features, geog = NULL, geoselect = NULL, dataset_info = TRUE) {
  
  empty_sf <- sf::st_sf(features = character(), name = character(), type = character(), address = character(), geometry = list(), crs = 4326)
  
  if ("parks" %in% features | "transit stops" %in% features) {
    
    sf_1 <- empty_sf
    
    if ("parks" %in% features) {
    
    parks <- arcgislayers::arc_select(arcgislayers::arc_open("https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Detailed_Parks/FeatureServer/0"), fields = c("NAME", "FEATTYPE")) |>
      dplyr::rename(name = NAME, type = FEATTYPE) |>
      dplyr::mutate(type = tolower(type), features = "parks", address = NA, name = ifelse(name == " ", NA, name))
    
    sf_1 <- rbind(sf_1, parks)
    }
    
    if ("transit stops" %in% features) {
      
      transit_stops <- arcgislayers::arc_select(arcgislayers::arc_open("https://services.arcgis.com/xOi1kZaI0eWDREZv/ArcGIS/rest/services/NTAD_National_Transit_Map_Stops/FeatureServer/0"), fields = c("stop_name", "stop_type_text")) |>
        dplyr::rename(name = stop_name, type = stop_type_text) |>
        dplyr::mutate(type = ifelse(type == "", NA, tolower(gsub("\"", "", gsub(";", "; ", type)))), features = "transit stops", address = NA, name = ifelse(name == "-", NA, name))
      
      sf_1 <- rbind(sf_1, transit_stops)
    }
    
    if (!(is.null(geog))) {
    
    if (geog == "state") {
      
      geog_state <- tigris::states(year = 2023) |>
        dplyr::filter(STUSPS %in% geoselect)
      
      geog_state <- sf::st_transform(geog_state, crs = 4326)
      
      sf_1_intersect <- sf::st_intersection(sf_1, geog_state)
      
      sf_1 <- sf_1 |>
        dplyr::filter(row.names(sf_1) %in% rownames(sf_1_intersect))
    }
    
    if (geog == "county") {
      
      # creating vector of state abbreviations just for requested counties so tigris call will run faster
      county_states <- as.vector(substr(geoselect, 1, 2))
      
      geog_county <- tigris::counties(year = 2023, state = county_states) |>
        dplyr::filter(GEOID %in% geoselect)
      
      geog_county <- sf::st_transform(geog_county, crs = 4326)
      
      sf_1_intersect <- sf::st_intersection(sf_1, geog_county)
      
      sf_1 <- sf_1 |>
        dplyr::filter(row.names(sf_1) %in% rownames(sf_1_intersect))
    }
    
    if (geog == "cbsa") {
      
      geog_cbsa <- tigris::core_based_statistical_areas(year = 2023) |>
        dplyr::filter(GEOID %in% geoselect)
      
      geog_cbsa <- sf::st_transform(geog_cbsa, crs = 4326)
      
      sf_1_intersect <- sf::st_intersection(sf_1, geog_cbsa)
      
      sf_1 <- sf_1 |>
        dplyr::filter(row.names(sf_1) %in% rownames(sf_1_intersect)) 
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
      
      sf_1_intersect <- sf::st_intersection(sf_1, geog_place)
      
      sf_1 <- sf_1 |>
        dplyr::filter(row.names(sf_1) %in% rownames(sf_1_intersect))
    }
    }
    
  } else {sf_1 <- empty_sf}
  
  if ("schools" %in% features) {
    
    if (!(is.null(geog))) {
    
    if (geog == "state") {where_clause <- paste0("STATE IN ('", paste(geoselect, collapse = "', '"), "')")}
    
    if (geog == "cbsa") {where_clause <- paste0("CBSA IN ('", paste(geoselect, collapse = "', '"), "')")}
    
    if (geog == "place") {
      
      # creating vector of state abbreviations just for requested places so function will run faster
      place_states <- as.vector(substr(geoselect, 1, 2))
      
      where_clause <- paste0("STFIP IN ('", paste(place_states, collapse = "', '"), "')")
    }
    
    if (geog == "zip code") {where_clause <- paste0("ZIP IN ('", paste(geoselect, collapse = "', '"), "')")}
    }
    
    public_schools <- arcgislayers::arc_select(arcgislayers::arc_open("https://services1.arcgis.com/Ua5sjt3LWTPigjyD/ArcGIS/rest/services/Public_School_Locations_Current/FeatureServer/0"), fields = c("NAME", "STREET", "CITY", "STATE", "ZIP"), where = if (!(is.null(geog))) {ifelse(geog == "county", paste0("CNTY IN ('", paste(geoselect, collapse = "', '"), "')"), where_clause)} else {NULL}) |>
      dplyr::mutate(name = NAME, type = "public", address = paste0(STREET, ", ", CITY, ", ", STATE, " ", ZIP))
    
    private_schools <- arcgislayers::arc_select(arcgislayers::arc_open("https://services1.arcgis.com/Ua5sjt3LWTPigjyD/ArcGIS/rest/services/Private_School_Locations_Current/FeatureServer/0"), fields = c("NAME", "STREET", "CITY", "STATE", "ZIP"), where = if (!(is.null(geog))) {ifelse(geog == "county", paste0("CONCAT(STFIP, CNTY) IN ('", paste(geoselect, collapse = "', '"), "')"), where_clause)} else {NULL}) |>
      dplyr::mutate(name = NAME, type = "private", address = paste0(STREET, ", ", CITY, ", ", STATE, " ", ZIP))
    
    schools <- rbind(public_schools, private_schools)
    
    schools <- sf::st_transform(schools, crs = 4326)
    
    if (!(is.null(geog))) {
    
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
    }
    
    schools <- schools |>
      dplyr::select(!c(NAME, STREET, CITY, STATE, ZIP)) |>
      dplyr::mutate(features = "schools")
    
  } else {schools <- empty_sf}
  
  if ("libraries" %in% features) {
    
    temp <- tempfile()
    
    utils::download.file("https://www.imls.gov/sites/default/files/2025-08/pls_fy2023_csv.zip", temp)
    
    temp2 <- tempfile()
    
    libraries <- utils::read.csv(utils::unzip(temp, files = "CSV/pls_fy23_outlet_pud23i.csv", exdir = temp2))
    
    unlink(temp)
    
    unlink(temp2)
    
    if (!(is.null(geog))) {
    
    if (geog == "state") {
      
      libraries <- libraries |>
        dplyr::filter(STABR %in% geoselect)
        
    }
    
    if (geog == "county") {
      
      libraries <- libraries |>
        dplyr::filter(substr(as.character(CENTRACT), 1, 5) %in% geoselect)
    }
    
    if (geog == "cbsa") {
      
      libraries <- libraries |>
        dplyr::filter(as.character(CBSA) %in% geoselect)
    }
    
    if (geog == "zip code") {
      
      libraries <- libraries |>
        dplyr::filter(as.character(ZIP) %in% geoselect)
    }
    }
    
    libraries <- sf::st_as_sf(libraries, coords = c("LONGITUD", "LATITUDE"), crs = 4326)
    
    libraries <- libraries |>
      dplyr::mutate(
        name = LIBNAME,
        features = "libraries",
        address = paste0(ADDRESS, ", ", CITY, ", ", STABR, " ", ZIP),
        type = dplyr::case_when(C_OUT_TY == "CE" ~ "central library",
                         C_OUT_TY == "BR" ~ "branch library",
                         C_OUT_TY == "BS" ~ "bookmobile(s)",
                         C_OUT_TY == "BM" ~ "books-by-mail only")) |>
      dplyr::select(c(name, features, address, type, geometry))
    
    if (!(is.null(geog))) {
    
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
      
      libraries_intersect <- sf::st_intersection(libraries, geog_place)
      
      libraries <- libraries |>
        dplyr::filter(row.names(libraries) %in% rownames(libraries_intersect)) 
    }
    }
    
  } else {libraries <- empty_sf}
  
  sf_all <- rbind(sf_1, schools, libraries)
  
  if (dataset_info == TRUE) {
    dataset_info_df <- retrieve_point_polygon_features() |>
      dplyr::select(c(features, source, year))
    
    sf_all <- dplyr::inner_join(sf_all, dataset_info_df, by = dplyr::join_by(features))
  }

  return(sf_all)
}
