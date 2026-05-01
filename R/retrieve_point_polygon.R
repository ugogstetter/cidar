globalVariables(c("FEATTYPE", "name", "stop_name", "stop_type_text", "STREET", "CITY", "STATE", "ZIP", "STABR", "CENTRACT", "LIBNAME", "ADDRESS", "address", "geometry", "LSTREE", "LCITY", "LSTATE", "LZIP", "PINST", "PL_ADD", "PL_CIT", "PL_STABB", "PL_ZIP", "FIPSST", "FIPSCO", "INCITSST", "INCITSCO", "FIPSST", "FIPSPLAC", "ORG_ADDR_MATCH", "GEOCODER_SCORE", "F990_ORG_ADDR_ZIP", "CENSUS_COUNTY_NAME", "CENSUS_STATE_NAME", "CENSUS_STATE_ABBR", "CENSUS_CBSA_FIPS", "ORG_ADDR_MATCH", "NTEEV2", "ORG_NAME_CURRENT", "LATITUDE", "LONGITUDE", "nonprofits_geoselect", "ORG_ADDR_FULL", "NCCS_LEVEL_3", "ORG_YEAR_FIRST", "ORG_YEAR_LAST", "COUNTY", "YEAR", "LONGITUD", "MAN_COLL"))

# defining function to add leading zeroes and convert numeric values to character as needed. x is the value, digits is the desired number of digits
leading_zeroes <- function(x, digits) {
  x <- as.character(x)
  
  if (nchar(x) < digits) {
    num_zeroes <- (digits - nchar(x))
    
    return(paste0(paste0(rep("0", num_zeroes), collapse = ""), x))
  } else {
    (return(x))
  }
}
leading_zeroes <- Vectorize(leading_zeroes)

retrieve_sf_1 <- function(features, yrs, geog, geoselect, state, types) {
  
  empty_sf <- sf::st_sf(features = character(), year = numeric(), name = character(), type = character(), address = character(), geometry = list(), crs = 4326)
  
  sf_1 <- empty_sf
  
  if ("parks" %in% features) {
    parks <- arcgislayers::arc_select(arcgislayers::arc_open("https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Detailed_Parks/FeatureServer/0"), fields = c("NAME", "FEATTYPE"), where = if (!(is.null(types$parks))) {
      paste0("LOWER(FEATTYPE) IN ('", paste(types$parks, collapse = "', '"), "')")
    } else {
      NULL
    }) |>
      dplyr::rename(name = NAME, type = FEATTYPE) |>
      dplyr::mutate(type = tolower(type), features = "parks", year = NA, address = NA, name = ifelse(name == " ", NA, name))
    
    sf_1 <- rbind(sf_1, parks)
  }
  
  if ("transit stops" %in% features) {
    transit_stops <- arcgislayers::arc_select(arcgislayers::arc_open("https://services.arcgis.com/xOi1kZaI0eWDREZv/ArcGIS/rest/services/NTAD_National_Transit_Map_Stops/FeatureServer/0"), fields = c("stop_name", "stop_type_text"), where = if (!(is.null(types$`transit stops`))) {
      # filtering out observations where the value is equal to types not selected or is empty. looks like with the where clause limitations that I can't filter out multi-transit stops not including the user-provided types, so I do that later in my code
      filter_out <- c("bus", "tram, streetcar, light rail", "rail", "ferry", "subway, metro", "trolleybus", "funicular", "aerial lift, suspended cable car", "cable tram", "monorail")
      filter_out <- filter_out[!(filter_out %in% types$`transit stops`)]
      for (i in 1:length(filter_out)) {filter_out[i] <- paste0("\"", filter_out[i], "\"")}
      paste0("LOWER(stop_type_text) NOT IN ('", paste(filter_out, collapse = "', '"), "', '')")
    } else {
      # filtering out observations where the value is empty
      paste0("stop_type_text NOT IN ('')")
      }) |>
      dplyr::rename(name = stop_name, type = stop_type_text) |>
      dplyr::mutate(type = tolower(gsub("\"", "", gsub(";", "; ", type))), features = "transit stops", year = NA, address = NA, name = ifelse(name == "-", NA, name))
    
    if (!is.null(types$`transit stops`)) {
      
      transit_stops_1 <- sf::st_sf(features = character(), year = numeric(), name = character(), type = character(), address = character(), geometry = list(), crs = 4326)
      
      # removing any values in transit_stops where the types do not include the user-requested type(s)
      
      for (i in 1:nrow(transit_stops)) {
        
        row_types <- transit_stops$type[i]
        
        row_types <- strsplit(row_types, "; ")[[1]]
      
      match <- FALSE
      
      for (n in row_types) {
        
        if (n %in% types$`transit stops`) {
          
          match <- TRUE
        }
      }
      
      if (match == TRUE) {
        
        transit_stops_1 <- rbind(transit_stops_1, transit_stops[i,])
      }
      }
      
      transit_stops <- transit_stops_1
      
      row.names(transit_stops) <- 1:nrow(transit_stops)
      
    }
    
    sf_1 <- rbind(sf_1, transit_stops) |>
      # creating a placeholder GEOID column and land area column for if geog is NULL or state
      dplyr::mutate(GEOID = NA, land_sq_mi = NA)
  }
  
  if (!is.null(state)) {
    
    geog_state <- tigris::states(year = 2023) |>
      dplyr::filter(STUSPS %in% state)
    
    geog_state <- sf::st_transform(geog_state, crs = 4326)
    
    sf_1_intersect <- sf::st_intersection(sf_1, geog_state)
    
    sf_1 <- sf_1 |>
      dplyr::filter(row.names(sf_1) %in% rownames(sf_1_intersect))
  }
  
  if (!(is.null(geog))) {
  
    if (geog == "state") {
      # geog can only be state if state is NULL, since state is always NULL for retrieve_point_polygon and geog is never state for retrieve_aggregated, so there's no risk of this repeating the code above again, I just don't think I can run them together as one thing since testing for NULL gets weird in those sorts of contexts
      # geoselect cannot be NULL in this case, and GEOID is never needed
      geog_state <- tigris::states(year = 2023) |>
        dplyr::filter(STUSPS %in% geoselect)
      
      geog_state <- sf::st_transform(geog_state, crs = 4326)
      
      sf_1_intersect <- sf::st_intersection(sf_1, geog_state)
      
      sf_1 <- sf_1 |>
        dplyr::filter(row.names(sf_1) %in% rownames(sf_1_intersect))
    }
    
    if (geog == "county") {

      # removing GEOID and land area variables (which are currently all NAs) so there won't be a duplicate GEOID and area column in the output of st_intersection in addition to the one associated with county_geoids, which will be added into sf_1_intersect during st_intersection
      sf_1 <- sf_1 |>
        dplyr::select(!c(GEOID, land_sq_mi))
      
      if (!(is.null(geoselect))) {
        
        # creating vector of state abbreviations just for requested counties so tigris call will run faster
        county_states <- as.vector(substr(geoselect, 1, 2))
        
        geog_county <- county_geoids(state = county_states, sf = TRUE, land_area = TRUE) |>
          dplyr::filter(GEOID %in% geoselect)
      }
      
      if (is.null(geoselect)) {
        
        geog_county <- county_geoids(sf = TRUE, land_area = TRUE)
      }
      
      sf_1_intersect <- sf::st_intersection(sf_1, geog_county)
      
      sf_1 <- sf_1 |>
        dplyr::filter(row.names(sf_1) %in% rownames(sf_1_intersect))
      
      # must order the rows properly for cbind to work correctly
      sf_1_intersect <- sf_1_intersect[ order(as.numeric(row.names(sf_1_intersect))), ]
      
      sf_1 <- cbind(sf_1, GEOID = sf_1_intersect$GEOID, land_sq_mi = sf_1_intersect$land_sq_mi)
    }
    
    if (geog == "cbsa") {
      
      # removing GEOID and land area variables (which are currently all NAs) so there won't be a duplicate GEOID column in the output of st_intersection in addition to the one associated with cbsa_geoids, which will be added into sf_1_intersect during st_intersection
      sf_1 <- sf_1 |>
        dplyr::select(!c(GEOID, land_sq_mi))
      
      if (!(is.null(geoselect))) {
        
        geog_cbsa <- cbsa_geoids(sf = TRUE, land_area = TRUE) |>
          dplyr::filter(GEOID %in% geoselect)
      }
      
      if (is.null(geoselect)) {
        
        geog_cbsa <- cbsa_geoids(sf = TRUE, land_area = TRUE)
      }
      
      sf_1_intersect <- sf::st_intersection(sf_1, geog_cbsa)
      
      sf_1 <- sf_1 |>
        dplyr::filter(row.names(sf_1) %in% rownames(sf_1_intersect))
      
      # must order the rows properly for cbind to work correctly
      sf_1_intersect <- sf_1_intersect[ order(as.numeric(row.names(sf_1_intersect))), ]
      
      sf_1 <- cbind(sf_1, GEOID = sf_1_intersect$GEOID, land_sq_mi = sf_1_intersect$land_sq_mi)
    }
    
    if (geog == "place") {
      
      # removing GEOID and land area variables (which are currently all NAs) so there won't be a duplicate GEOID column in the output of st_intersection in addition to the one associated with place_geoids, which will be added into sf_1_intersect during st_intersection
      sf_1 <- sf_1 |>
        dplyr::select(!c(GEOID, land_sq_mi))
      
      if (!(is.null(geoselect))) {
        
        # creating vector of state abbreviations just for requested places so tigris call in place_geoids will run faster
        place_states <- tigris::states(year = 2023) |>
          dplyr::filter(GEOID %in% as.vector(substr(geoselect, 1, 2)))
        
        place_states <- as.vector(place_states$STUSPS)
        
        geog_place <- place_geoids(state = place_states, sf = TRUE, land_area = TRUE)
        
        geog_place <- geog_place |>
          dplyr::filter(GEOID %in% geoselect)
      }
      
      if (is.null(geoselect)) {
        
        geog_place <- place_geoids(sf = TRUE, land_area = TRUE)
      }
      
      sf_1_intersect <- sf::st_intersection(sf_1, geog_place)
      
      sf_1 <- sf_1 |>
        dplyr::filter(row.names(sf_1) %in% rownames(sf_1_intersect))
      
      # must order the rows properly for cbind to work correctly
      sf_1_intersect <- sf_1_intersect[ order(as.numeric(row.names(sf_1_intersect))), ]
      
      sf_1 <- cbind(sf_1, GEOID = sf_1_intersect$GEOID, land_sq_mi = sf_1_intersect$land_sq_mi)
    }
  }
  
  return(sf_1)
}


retrieve_schools <- function(schools_yrs, geog, geoselect, state, types) {
  
  empty_sf <- sf::st_sf(features = character(), year = numeric(), name = character(), type = character(), address = character(), geometry = list(), crs = 4326)

  # value is what will be used to select column(s) providing the geographic area of school locations, in accordance with the value given by geog, while where_clause is what will be used to filter the data if a state or geoselect value is given
  
  # setting value to return empty vector if none of the below functions to set its value differently apply
  value <- function(year, type) {return(c())}
  
  # setting where_clause to return NULL if none of the below functions to set its value differently apply
  where_clause <- function(year, type) {return(NULL)}
  
  if (!(is.null(geog))) {
    
    if (geog == "state") {
      
      # don't need to set value or check if geoselect is NULL since geog can only be state in retrieve_point_polygon, which always includes a geoselect value
        
        where_clause <- function(year, type) {
          
        clause_value <- "STATE"
        
        if (type == "public" & year == 2015) {
          clause_value <- "LSTATE"
        }
        
        if (type == "private" & year %in% 2015:2016) {
          clause_value <- "PL_STABB"
        }
        return(paste0(clause_value, " IN ('", paste(geoselect, collapse = "', '"), "')"))
        }
    }
    
    if (!is.null(state)) {
      
      where_clause <- function(year, type) {
        clause_value <- "STATE"
        
        if (type == "public" & year == 2015) {
          clause_value <- "LSTATE"
        }
        
        if (type == "private" & year %in% 2015:2016) {
          clause_value <- "PL_STABB"
        }
        return(paste0(clause_value, " IN ('", paste(state, collapse = "', '"), "')"))
      }
    }
    
    if (geog == "cbsa") {
      
      value <- function(year, type) {
      
      val <- "CBSA"
      
      if ((type == "public" & year == 2015) | (type == "private" & year %in% 2015:2016)) {
        val <- "CBSA15"
      }
      return(val)
      }
      
      if (!is.null(geoselect)) {
      where_clause <- function(year, type) {
        
        if (type == "private" & year %in% 2015:2016) {
          
          return(paste0(value(year, type), " IN (", paste(geoselect, collapse = ", "), ")"))
          
        } else {
          
          return(paste0(value(year, type), " IN ('", paste(geoselect, collapse = "', '"), "')"))
        }
      }
      }
    }
    
    if (geog == "place") {
      
      # am not setting value since this won't return place codes, just state codes
      
        if (!(is.null(geoselect))) {
        where_clause <- function(year, type) {
          
      # creating vector of state abbreviations just for requested places so function will run faster
      place_state_geoids <- as.vector(substr(geoselect, 1, 2))
      
      clause_value <- "STFIP"
      
      if ((type == "public" & year == 2015) | (type == "private" & year %in% 2015:2016)) {
        clause_value <- "STFIP15"
      }
        
        return(paste0(clause_value, " IN ('", paste(place_state_geoids, collapse = "', '"), "')"))
      }
        }
    }
    
    if (geog == "zip code") {
      
      value <- function(year, type) {
      val <- "ZIP"
      
      if (type == "public" & year == 2015) {
        val <- "LZIP"
      }
      
      if (type == "private" & year %in% 2015:2016) {
        val <- "PL_ZIP"
      }
      return(val)
      }
      
      if (!(is.null(geoselect))) {
      where_clause <- function(year, type) {
        
        return(paste0(value(year, type), " IN ('", paste(geoselect, collapse = "', '"), "')"))
      }
    }
    }
    
    if (geog == "county") {
      
      value <- function(year, type) {
        
        if (type == "public") {
          
          if (year == 2015) {
            
            val <- "CNTY15"
            
          } else {
            
          val <- "CNTY"
          
          }
        }
        
        if (type == "private") {
          
          if (year %in% 2015:2016) {
            
            val <- "CNTY15"
            
          } else if (year %in% 2017:2022) {
            
            val <- "CNTY"
            
          } else {
          
          val <- c("STFIP", "CNTY")
          }
        }
        
        return(val)
      }
        
      
      if (!(is.null(geoselect))) {
      where_clause <- function(year, type) {
        
        if (type == "public") {
          to_return <- paste0(value(year, type), " IN ('", paste(geoselect, collapse = "', '"), "')")
        }
        
        if (type == "private") {
          to_return <- paste0(ifelse(year < 2023, value(year, type), "CONCAT(STFIP, CNTY)"), " IN ('", paste(geoselect, collapse = "', '"), "')")
        }
        return(to_return)
      }
      }
    }
  }
  
  schools <- empty_sf
  
  for (i in schools_yrs) {
    
    retrieve_public <- TRUE
    
    retrieve_private <- TRUE
    
    if (!is.null(types$schools)) {
      
      if (!("public" %in% types$schools)) {
        
        retrieve_public <- FALSE
      }
      
      if (!("private" %in% types$schools)) {
        
        retrieve_private <- FALSE
      }
    }
      
    if (retrieve_public == TRUE) {
      
    public_schools <- arcgislayers::arc_select(arcgislayers::arc_open(paste0("https://nces.ed.gov/opengis/rest/services/K12_School_Locations/EDGE_GEOCODE_PUBLICSCH_", substr(as.character(i), 3, 4), substr(as.character(i + 1), 3, 4), "/MapServer/0")), fields = if (i > 2015) {
      c(c("NAME", "STREET", "CITY", "STATE", "ZIP"), value(i, "public"))
    } else {
      c("NAME", "LSTREE", "LCITY", "LSTATE", "LZIP", value(i, "public"))
    }, where = where_clause(i, "public")) |>
      dplyr::mutate(name = NAME, type = "public", address = ifelse(i > 2015, paste0(STREET, ", ", CITY, ", ", STATE, " ", ZIP), paste0(LSTREE, ", ", LCITY, ", ", LSTATE, " ", LZIP)), features = "schools", year = i)
    
    if (length(value(i, "public")) == 1) {
      
      public_schools <- public_schools |>
        dplyr::rename(GEOID = value(i, "public"))
    } else {
      # creating a placeholder column for GEOID when the value function does not produce a particular output as defined above
      public_schools <- public_schools |>
        dplyr::mutate(GEOID = NA)
    }
    
    public_schools <- public_schools |>
      dplyr::select(c(GEOID, features, year, name, type, address, geometry))
    
    
    # I have to transform CRS before merging the datasets because not all the datasets have the same CRS, but if I transform a 4326 dataset with this, it'll still have the same geometry
    public_schools <- sf::st_transform(public_schools, crs = 4326)
    
    } else {
      
      public_schools <- empty_sf
    }
    
    
    if (retrieve_private == TRUE) {
      
    private_schools <- arcgislayers::arc_select(arcgislayers::arc_open(ifelse(i > 2016, paste0("https://nces.ed.gov/opengis/rest/services/K12_School_Locations/EDGE_GEOCODE_PRIVATESCH_", ifelse(i %% 2 == 0, paste0(substr(as.character(i - 1), 3, 4), substr(as.character(i), 3, 4)), paste0(substr(as.character(i), 3, 4), substr(as.character(i + 1), 3, 4))), "/MapServer/0"), "https://nces.ed.gov/opengis/rest/services/K12_School_Locations/EDGE_GEOCODE_PRIVATESCH_15_16/MapServer/0")), fields = if (i > 2016) {
      c(c("NAME", "STREET", "CITY", "STATE", "ZIP"), value(i, "private"))
    } else {
      c(c("PINST", "PL_ADD", "PL_CIT", "PL_STABB", "PL_ZIP"), value(i, "private"))
    }, where = where_clause(i, "private")) |>
      dplyr::mutate(name = ifelse(i > 2016, NAME, PINST), type = "private", address = ifelse(i > 2016, paste0(STREET, ", ", CITY, ", ", STATE, " ", ZIP), paste0(PL_ADD, ", ", PL_CIT, ", ", PL_STABB, " ", PL_ZIP)), features = "schools", year = i)
    
    if (length(value(i, "private")) == 1) {
      
      private_schools <- private_schools |>
        dplyr::rename(GEOID = value(i, "private"))
      
    } else if (length(value(i, "private")) == 2) {
      
      # length of value is 2 when geog is county and school type is private schools. here, we have to concatenate STFIP together with CNTY to get the full county GEOID
      private_schools <- private_schools |>
        dplyr::mutate(GEOID = paste0(STFIP, CNTY))
        
      } else {
      # creating a placeholder column for GEOID when the value function does not produce a particular output as defined above
      private_schools <- private_schools |>
        dplyr::mutate(GEOID = NA)
    }
    
    private_schools <- private_schools |>
      dplyr::select(c(GEOID, features, year, name, type, address, geometry))
    
    private_schools <- sf::st_transform(private_schools, crs = 4326)
    
    } else {
      
      private_schools <- empty_sf
    }
    
    
    schools_1 <- rbind(public_schools, private_schools)
    
    schools <- rbind(schools, schools_1)
  }
  
  if (!(is.null(geog))) {
    
    if (geog == "place") {
      
      # removing GEOID variable (which is currently all NAs) so there won't be a duplicate GEOID column in the output of st_intersection in addition to the one associated with place_geoids, which will be added into schools_intersect during st_intersection
      schools <- schools |>
        dplyr::select(!GEOID)
      
      if (!(is.null(geoselect))) {
        
      # creating vector of state abbreviations just for requested places so tigris call in place_geoids will run faster
      place_states <- tigris::states(year = 2023) |>
        dplyr::filter(GEOID %in% as.vector(substr(geoselect, 1, 2)))
      
      place_states <- as.vector(place_states$STUSPS)
      
      geog_place <- place_geoids(state = place_states, sf = TRUE)
      
      geog_place <- geog_place |>
        dplyr::filter(GEOID %in% geoselect)
      }
      
      if (is.null(geoselect)) {
        
        geog_place <- place_geoids(sf = TRUE)
      }
      
      schools_intersect <- sf::st_intersection(schools, geog_place)
      
      schools <- schools |>
        dplyr::filter(row.names(schools) %in% rownames(schools_intersect))
      
      # must order the rows properly for cbind to work correctly
      schools_intersect <- schools_intersect[ order(as.numeric(row.names(schools_intersect))), ]
      
      schools <- cbind(schools, GEOID = schools_intersect$GEOID)
    }
  }
  
  return(schools)
}


retrieve_libraries <- function(libraries_yrs, geog, geoselect, types) {
  
  empty_sf <- sf::st_sf(features = character(), year = numeric(), name = character(), type = character(), address = character(), geometry = list(), crs = 4326)
  
  if (!is.null(geog)) {
    if (geog == "cbsa" & min(libraries_yrs) < 2011) {
      geog_cbsa <- cbsa_geoids(sf = TRUE) |>
        dplyr::filter(GEOID %in% geoselect)
    }
    
    if (geog == "place" & max(libraries_yrs) > 2014) {
      # creating vector of state abbreviations just for requested places so tigris call in place_geoids will run faster
      place_states <- tigris::states(year = 2023) |>
        dplyr::filter(GEOID %in% as.vector(substr(geoselect, 1, 2)))
      place_states <- as.vector(place_states$STUSPS)
      
      geog_place <- place_geoids(state = place_states, sf = TRUE)
      
      geog_place <- geog_place |>
        dplyr::filter(GEOID %in% geoselect)
    }
  }
  
  libraries <- empty_sf
  
  for (i in libraries_yrs) {
    temp <- tempfile()
    
    dplyr::case_when(i == 2023 ~ "https://www.imls.gov/sites/default/files/2025-08/pls_fy2023_csv.zip",
              i == 2022 ~ "https://www.imls.gov/sites/default/files/2024-06/pls_fy2022_csv.zip",
              i == 2021 ~ "https://www.imls.gov/sites/default/files/2023-06/pls_fy2021_csv.zip",
              i == 2020 ~ "https://www.imls.gov/sites/default/files/2022-07/pls_fy2020_csv.zip",
              i == 2019 ~ "https://www.imls.gov/sites/default/files/2021-05/pls_fy2019_csv.zip",
              i >= 2014 ~ paste0("https://www.imls.gov/sites/default/files/pls_fy", as.character(i), "_data_files_csv.zip"),
              i != 2011 ~ paste0("https://www.imls.gov/sites/default/files/pupld", substr(as.character(i), 3, 4), "a_csv.zip"),
              .default = "https://www.imls.gov/sites/default/files/pupld11b_csv.zip") |>
    utils::download.file(temp)
    
    temp2 <- tempfile()
    
    libraries_1 <- utils::read.csv(utils::unzip(temp, files = dplyr::case_when(i == 2023 ~ "CSV/pls_fy23_outlet_pud23i.csv",
                                                                        i >= 2021 ~ paste0("PLS_FY", as.character(i), " PUD_CSV/pls_fy", substr(as.character(i), 3, 4), "_outlet_pud", substr(as.character(i), 3, 4), "i.csv"),
                                                                        i >= 2019 ~ paste0("PLS_FY", substr(as.character(i), 3, 4), "_Outlet_pud", substr(as.character(i), 3, 4), "i.csv"),
                                                                        i == 2018 ~ "pls_fy18_outlet_pud18i.csv",
                                                                        i == 2017 ~ "PLS_FY2017_Data_Files_CSV/PLS_FY17_Outlet_pud17i.csv",
                                                                        i == 2016 ~ "PLS_FY2016_Outlet_puout16a.csv",
                                                                        i == 2015 ~ "CSV/PLS_FY2015_Outlet_puout15a.csv",
                                                                        i == 2014 ~ "PLS_FY2014_Data-Files_csv/PLS_FY2014_Outlet_puout14a.csv",
                                                                        i >= 2012 ~ paste0("Puout", substr(as.character(i), 3, 4), "a.csv"),
                                                                        i == 2011 ~ "puout11b.csv",
                                                                        .default = paste0("puout", substr(as.character(i), 3, 4), "a.csv")), exdir = temp2))
    
    unlink(temp)
    
    unlink(temp2)
    
    libraries_1 <- libraries_1 |>
      dplyr::mutate(type = dplyr::case_when(
        C_OUT_TY == "CE" ~ "central library",
        C_OUT_TY == "BR" ~ "branch library",
        C_OUT_TY == "BS" ~ "bookmobile(s)",
        C_OUT_TY == "BM" ~ "books-by-mail only"
      ))
    
    if (!is.null(types$libraries)) {
      
      libraries_1 <- libraries_1 |>
        dplyr::filter(type %in% types$libraries)
    }
    
    libraries_1 <- sf::st_as_sf(libraries_1, coords = c("LONGITUD", "LATITUDE"), crs = 4326)
    
    
    if (!(is.null(geog))) {
      if (geog == "state") {
        libraries_1 <- libraries_1 |>
          dplyr::filter(STABR %in% geoselect)
      }
      
      if (geog == "county") {
        if (i <= 2014) {
          libraries_1 <- libraries_1 |>
            dplyr::filter(paste0(leading_zeroes(FIPSST, 2), leading_zeroes(FIPSCO, 3)) %in% geoselect)
        }
        
        if (i %in% 2015:2021) {
          libraries_1 <- libraries_1 |>
            dplyr::filter(paste0(leading_zeroes(INCITSST, 2), leading_zeroes(INCITSCO, 3)) %in% geoselect)
        }
        
        if (i >= 2022) {
          libraries_1 <- libraries_1 |>
            dplyr::filter(substr(leading_zeroes(CENTRACT, 11), 1, 5) %in% geoselect)
        }
      }
      
      if (geog == "cbsa") {
        if (i < 2011) {
          libraries_intersect <- sf::st_intersection(libraries_1, geog_cbsa)
          
          libraries_1 <- libraries_1 |>
            dplyr::filter(row.names(libraries_1) %in% rownames(libraries_intersect))
        }
        
        if (i >= 2011) {
          libraries_1 <- libraries_1 |>
            dplyr::filter(leading_zeroes(CBSA, 5) %in% geoselect)
        }
      }
      
      if (geog == "place") {
        if (i <= 2014) {
          libraries_1 <- libraries_1 |>
            dplyr::filter(paste0(leading_zeroes(FIPSST, 2), leading_zeroes(FIPSPLAC, 5)) %in% geoselect)
        }
        
        if (i > 2014) {
          libraries_intersect <- sf::st_intersection(libraries_1, geog_place)
          
          libraries_1 <- libraries_1 |>
            dplyr::filter(row.names(libraries_1) %in% rownames(libraries_intersect))
        }
      }
      
      if (geog == "zip code") {
        libraries_1 <- libraries_1 |>
          dplyr::filter(as.character(ZIP) %in% geoselect)
      }
    }
    
    libraries_1 <- libraries_1 |>
      dplyr::mutate(
        name = LIBNAME,
        features = "libraries",
        year = i,
        address = paste0(ADDRESS, ", ", CITY, ", ", STABR, " ", ZIP)
      ) |>
      dplyr::select(c(name, features, year, address, type, geometry))
    
    libraries <- rbind(libraries, libraries_1)
  }
  
  return(libraries)
}


retrieve_nonprofits <- function(nonprofits_yrs, geog, geoselect, types) {
  
  empty_sf <- sf::st_sf(features = character(), year = numeric(), name = character(), type = character(), address = character(), geometry = list(), crs = 4326)
  
  if (is.null(geog)) {
    all_states <- TRUE
  } else {
    all_states <- FALSE
    
    if (geog == "state") {
      states <- geoselect
    }
    
    if (geog == "county" | geog == "place") {
      # retrieving numeric GEOIDs for states corresponding to the counties/places in geoselect
      state_geoids <- as.vector(substr(geoselect, 1, 2))
      
      states_match <- tigris::states(year = 2023) |>
        dplyr::filter(GEOID %in% state_geoids)
      
      states <- as.vector(states_match$STUSPS)
    }
    
    if (geog == "cbsa") {
      all_states <- TRUE
    }
  }
  
  if (all_states == TRUE) {
    states <- c("AK", "AL", "AR", "AS", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "GU", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MP", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VI", "VT", "WA", "WI", "WV", "WY")
  }
  
  nonprofits_all <- data.frame(ORG_ADDR_MATCH = character(), NTEEV2 = character(), ORG_NAME_CURRENT = character(), LATITUDE = numeric(), LONGITUDE = numeric(), nonprofits_geoselect = character(), ORG_ADDR_FULL = character(), NCCS_LEVEL_3 = character(), ORG_YEAR_FIRST = numeric(), ORG_YEAR_LAST = numeric())
  
  for (i in states) {
    nonprofits_1 <- utils::read.csv(paste0("https://nccsdata.s3.amazonaws.com/harmonized/bmf/unified/", i, "_BMF_V1.1.csv")) |>
      dplyr::filter( # removing rows with geocoded addresses that appear to start with zip codes
        (!((!grepl("\\D", substr(ORG_ADDR_MATCH, 1, 5))) & (((substr(ORG_ADDR_MATCH, 6, 6) == "-") & (!grepl("\\D", substr(ORG_ADDR_MATCH, 7, 10))) & substr(ORG_ADDR_MATCH, 11, 11) == ",") | (substr(ORG_ADDR_MATCH, 6, 6) == ",")))) &
          # removing rows with geocoded addresses of a match score less than 90 (from my observation, 90 appears to be the approximate threshold for reasonably reliable geocoding)
          (GEOCODER_SCORE >= 90) &
          # removing rows with no zip codes given (as these are generally international addresses)
          (F990_ORG_ADDR_ZIP != "00000-0000")
      ) |>
      dplyr::mutate(type = dplyr::case_when(substr(NTEEV2, 1, 3) == "ART" ~ "arts, culture, and humanities",
                                            substr(NTEEV2, 1, 3) == "EDU" ~ "education",
                                            substr(NTEEV2, 1, 3) == "ENV" ~ "environment and animals",
                                            substr(NTEEV2, 1, 3) == "HEL" ~ "health",
                                            substr(NTEEV2, 1, 3) == "HMS" ~ "human services",
                                            substr(NTEEV2, 1, 3) == "IFS" ~ "international, foreign affairs",
                                            substr(NTEEV2, 1, 3) == "MMB" ~ "mutual/membership benefit",
                                            substr(NTEEV2, 1, 3) == "PSB" ~ "public, societal benefit",
                                            substr(NTEEV2, 1, 3) == "REL" ~ "religion related",
                                            substr(NTEEV2, 1, 3) == "UNI" ~ "university",
                                            substr(NTEEV2, 1, 3) == "HOS" ~ "hospital",
                                            .default = "unknown"
      ), nonprofits_geoselect = NA)
    
    
    if (!is.null(types$nonprofits)) {
      
      nonprofits_1 <- nonprofits_1 |>
        dplyr::filter(type %in% types$nonprofits)
    }
    
    if (!is.null(geog)) {
      if (geog == "county") {
        nonprofits_1 <- nonprofits_1 |>
          dplyr::mutate(nonprofits_geoselect = paste(CENSUS_COUNTY_NAME, CENSUS_STATE_ABBR))
      }
      
      if (geog == "cbsa") {
        nonprofits_1 <- nonprofits_1 |>
          dplyr::mutate(nonprofits_geoselect = as.character(CENSUS_CBSA_FIPS))
      }
    }
    
    nonprofits_1 <- nonprofits_1 |>
      dplyr::select(c(ORG_ADDR_MATCH, type, ORG_NAME_CURRENT, LATITUDE, LONGITUDE, nonprofits_geoselect, ORG_ADDR_FULL, NCCS_LEVEL_3, ORG_YEAR_FIRST, ORG_YEAR_LAST))
    
    nonprofits_all <- rbind(nonprofits_all, nonprofits_1)
  }
  
  nonprofits <- data.frame(ORG_ADDR_MATCH = character(), type = character(), ORG_NAME_CURRENT = character(), LATITUDE = numeric(), LONGITUDE = numeric(), nonprofits_geoselect = character(), year = numeric())
  
  for (i in nonprofits_yrs) {
    nonprofits_2 <- nonprofits_all |>
      dplyr::filter(ORG_YEAR_FIRST >= i & ORG_YEAR_LAST <= i) |>
      dplyr::mutate(year = i) |>
      # using same method for determining distinct nonprofits as is used in calculating COI
      dplyr::distinct(ORG_ADDR_FULL, ORG_NAME_CURRENT, NCCS_LEVEL_3, .keep_all = TRUE) |>
      dplyr::select(c(ORG_ADDR_MATCH, type, ORG_NAME_CURRENT, LATITUDE, LONGITUDE, nonprofits_geoselect, year))
    
    nonprofits <- rbind(nonprofits, nonprofits_2)
  }
  
  nonprofits <- sf::st_as_sf(nonprofits, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  
  if (!is.null(geog)) {
    if (geog == "county") {
      nonprofits_counties <- county_geoids(state = states) |>
        dplyr::filter(GEOID %in% geoselect) |>
        dplyr::mutate(nonprofits_geoselect = paste(sub(",[^,]*$", "", name), state_abbr))
      
      nonprofits <- dplyr::inner_join(nonprofits, nonprofits_counties, by = dplyr::join_by("nonprofits_geoselect"))
    }
    
    if (geog == "cbsa") {
      nonprofits <- nonprofits |>
        dplyr::filter(nonprofits_geoselect %in% geoselect)
    }
    
    if (geog == "place") {
      geog_place <- place_geoids(state = states, sf = TRUE)
      
      geog_place <- geog_place |>
        dplyr::filter(GEOID %in% geoselect)
      
      nonprofits_intersect <- sf::st_intersection(nonprofits, geog_place)
      
      nonprofits <- nonprofits |>
        dplyr::filter(row.names(nonprofits) %in% rownames(nonprofits_intersect))
    }
  }
  
  nonprofits <- nonprofits |>
    # I am using NTEEV2 for type as it seems to have the smallest number of type categories out of the different variables for type
    dplyr::mutate(name = ORG_NAME_CURRENT, features = "nonprofits", address = ORG_ADDR_MATCH
    ) |>
    dplyr::select(c(name, type, features, year, address, geometry))

  
  return(nonprofits)
}

retrieve_crashes <- function(crashes_yrs, geog, geoselect, types) {
  
  empty_sf <- sf::st_sf(features = character(), year = numeric(), name = character(), type = character(), address = character(), geometry = list(), crs = 4326)
  
  crashes <- data.frame(STATE = numeric(), COUNTY = numeric(), YEAR = numeric(), LATITUDE = numeric(), LONGITUD = numeric(), MAN_COLL = numeric())
  
  for (i in crashes_yrs) {
    temp <- tempfile()
    
    utils::download.file(paste0("https://static.nhtsa.gov/nhtsa/downloads/FARS/", as.character(i), "/National/FARS", as.character(i), "NationalCSV.zip"), temp)
    
    temp2 <- tempfile()
    
    crashes1 <- utils::read.csv(utils::unzip(temp, files = dplyr::case_when(i < 2006 ~ "ACCIDENT.CSV", 
                                                                            i %in% 2006:2011 ~ paste0("FARS", as.character(i), "NationalCSV/ACCIDENT.CSV"),
                                                                            i %in% 2012:2014 ~ paste0("FARS", as.character(i), "NationalCSV/ACCIDENT.csv"),
                                                                            i %in% c(2015, 2020:2024) ~ paste0("FARS", as.character(i), "NationalCSV/accident.csv"),
                                                                            i %in% c(2016:2017, 2019) ~ "accident.CSV",
                                                                            .default = "accident.csv"), exdir = temp2))
    
    if (i %in% 2001:2007) {
      
      crashes1 <- crashes1 |>
        dplyr::rename(LATITUDE = latitude, LONGITUD = longitud)
    }
    
    crashes1 <- crashes1 |>
      dplyr::select(c(STATE, COUNTY, YEAR, LATITUDE, LONGITUD, MAN_COLL))
    
    unlink(temp)
    
    unlink(temp2)
    
    crashes <- rbind(crashes, crashes1)
  }
  
  crashes <- crashes |>
    dplyr::mutate(type = dplyr::case_when(MAN_COLL == 1 ~ "front-to-rear collision",
                                          MAN_COLL == 2 ~ "front-to-front collision",
                                          ((YEAR %in% 1999:2001 & MAN_COLL == 3) | (YEAR >= 2002 & MAN_COLL == 10)) ~ "rear-to-rear collision",
                                          ((YEAR %in% 1999:2001 & MAN_COLL == 4) | (YEAR %in% 2002:2009 & MAN_COLL %in% 3:6) | (YEAR >= 2010 & MAN_COLL == 6)) ~ "collision at angle",
                                          ((YEAR %in% 1999:2001 & MAN_COLL %in% 5:6) | (YEAR >= 2002 & MAN_COLL %in% 7:8)) ~ "sideswipe",
                                          .default = "other/unknown"
    )) |>
    dplyr::filter((!is.na(LATITUDE)) & (!is.na(LONGITUD)))
  
  if (!is.null(types$`fatal crashes`)) {
    
    crashes <- crashes |>
      dplyr::filter(type %in% types$`fatal crashes`)
  }
  
  
  # creating and vectorizing function to make each longitude and latitude value with just one number in it be equal to just that number
  unique_crashes <- function(x) {unique(strsplit(x, "")[[1]])}
  unique_crashes <- Vectorize(unique_crashes)
  
  # removing not reported/not available/unknown latitudes and longitudes
  
  crashes_test <- crashes |>
    dplyr::mutate(LATITUDE = unique_crashes(gsub("\\.", "", gsub("0", "", as.character(LATITUDE)))), LONGITUD = unique_crashes(gsub("\\.", "", gsub("0", "", as.character(LONGITUD)))), row = row.names(crashes)) |>
    dplyr::filter((!(LATITUDE %in% c("7", "8", "9"))) & (!(LONGITUD %in% c("7", "8", "9"))))
  
  crashes <- crashes |>
    dplyr::filter(row.names(crashes) %in% crashes_test$row)
  
  
  if (!is.null(geog)) {
    if (geog == "county") {
      crashes <- crashes |>
        dplyr::mutate(GEOID = paste0(leading_zeroes(STATE, 2), leading_zeroes(COUNTY, 3))) |>
        dplyr::filter(GEOID %in% geoselect)
    }
    
    if (geog == "state") {
      crashes <- crashes |>
        dplyr::mutate(GEOID = leading_zeroes(STATE, 2))
      
      crashes_states <- tigris::states(year = 2023) |>
        dplyr::filter(STUSPS %in% geoselect) |>
        dplyr::select(GEOID)
      
      crashes <- dplyr::inner_join(crashes, crashes_states, by = dplyr::join_by("GEOID"))
    }
    
    # I really tried to figure out what CRS this data is in and couldn't figure it out for certain, but another crash data document from NTHSA said it uses WGS84 (4326), and it sounds like the two main coordinate systems have a difference of only like 1-3 meters anyway. also, the documentation for this specific dataset discusses the coordinates being GPS, which it seems like is more aligned with WGS84
    crashes <- sf::st_as_sf(crashes, coords = c("LONGITUD", "LATITUDE"), crs = 4326)
    
    if (geog == "cbsa") {
      geog_cbsa <- cbsa_geoids(sf = TRUE)
      
      geog_cbsa <- geog_cbsa |>
        dplyr::filter(GEOID %in% geoselect)
      
      crashes_intersect <- sf::st_intersection(crashes, geog_cbsa)
      
      crashes <- crashes |>
        dplyr::filter(row.names(crashes) %in% rownames(crashes_intersect))
    }
    
    if (geog == "place") {
      # retrieving states corresponding to user-provided places so that place_geoids will only have to retrieve tigris outputs for the necessary states and thus the function can run faster
      crashes_states <- tigris::states(year = 2023) |>
        dplyr::filter(STUSPS %in% geoselect)
      
      geog_place <- place_geoids(state = states, sf = TRUE)
      
      geog_place <- geog_place |>
        dplyr::filter(GEOID %in% geoselect)
      
      crashes_intersect <- sf::st_intersection(crashes, geog_place)
      
      crashes <- crashes |>
        dplyr::filter(row.names(crashes) %in% rownames(crashes_intersect))
    }
  }
  
  crashes <- crashes |>
    dplyr::mutate(name = NA, features = "fatal crashes", year = YEAR, address = NA
    ) |>
    dplyr::select(c(name, type, features, year, address, geometry))
  
  return(crashes)
}

#' Retrieves shapefile of point or polygon features from chosen source
#' @description Retrieves shapefile of point or polygon features of user-specified data category,
#'  from data sources such as ArcGIS Online and the Institute of Museum and Library Services.
#' @param features Before supplying this argument,
#' ensure you have read the [retrieve_point_polygon_features()] function description, and have run that function to
#' return the data frame of retrievable features and their types. The `features` 
#' argument for this [retrieve_point_polygon()] function can be supplied in one of two ways:
#' * Specify an atomic vector of feature sets to retrieve.
#' * Specify a list of named vectors, where vector names are feature sets and 
#' vector values are the types to include within each feature set. Set a vector
#'  equal to `NULL` to include all possible types within the given feature set.
#' @param yrs If yrs is a numeric vector, this function will retrieve all
#' specified features for all years in the specified vector. If yrs is `NULL`,
#' this function will retrieve all most recent years of selected features. If yrs is not provided,
#' retrieve all available years for all features associated with specific years.
#'  The years for which data is available for each feature set are included in the
#'   [retrieve_point_polygon_features()] dataset. Please note that if `yrs` is 
#'   equal to `NA` in that dataset, the feature set can only be retrieved in 
#'   this [retrieve_point_polygon()] function when `yrs` is set to `NULL`.
#' @param geog Optionally, specify a geographic area to filter to. Can be
#' `"state"`, `"county"`, `"cbsa"` (core based statistical
#' area), `"place"`, or `"zip code"`. If no value is provided, a nationwide
#' dataset will be returned. Please see [retrieve_point_polygon_features()] to
#'  determine whether a particular feature set is able to be filtered to a particular
#'  geographic area, and only specify a geographic area that all selected feature
#'   sets can be filtered to.
#' @param geoselect If a value is supplied for `geog`, supply a vector (in character format) of two-letter state abbreviations; county,
#'  CBSA, or place GEOIDS; or zip codes (depending on whether `geog` is set to `"state"`,
#'  `"county"`, `"cbsa"` or `"place"`, or `"zip code"`) to filter to. By default, all possible
#'   values will be returned by the
#' function. To determine the corresponding GEOID for a particular county, CBSA,
#'  or place, use [county_geoids()], [cbsa_geoids()], or [place_geoids()],
#'  respectively. Note that for polygon features, all features intersecting the
#'  specified geographic area will be returned, not just those fully inside of it.
#' @param dataset_info Specify whether to append columns containing
#'  source information for the dataset in this function's
#'   output. Default value is `TRUE`. The appended information will
#'   be the same as that contained in the [retrieve_point_polygon_features()] data frame.
#' @export

retrieve_point_polygon <- function(features, yrs = 1999:2024, geog = NULL, geoselect = NULL, dataset_info = TRUE) {
  UseMethod("retrieve_point_polygon", features)
}

#' @rdname retrieve_point_polygon
#' @export
retrieve_point_polygon.character <- function(features, yrs = 1999:2024, geog = NULL, geoselect = NULL, dataset_info = TRUE) {
  
  features_names <- features
  
  features <- vector(mode = "list", length = length(features_names))
  
  names(features) <- features_names
  
  retrieve_point_polygon(features, yrs, geog, geoselect, dataset_info)
}

#' @rdname retrieve_point_polygon
#' @export
retrieve_point_polygon.list <- function(features, yrs = 1999:2024, geog = NULL, geoselect = NULL, dataset_info = TRUE) {
  
  types <- features
  
  features <- names(features)
  

  empty_sf <- sf::st_sf(features = character(), year = numeric(), name = character(), type = character(), address = character(), geometry = list(), crs = 4326)

  
  if (("parks" %in% features | "transit stops" %in% features) & is.null(yrs)) {
    
    sf_1 <- retrieve_sf_1(features, yrs, geog, geoselect, state = NULL, types) |>
      # filtering out the GEOID and land area columns that were added for use with retrieve_aggregated
      dplyr::select(!c(GEOID, land_sq_mi))
    
    } else {sf_1 <- empty_sf}
  
  if (is.null(yrs)) {
    schools_yrs <- 2024
  } else {
  schools_yrs <- yrs[yrs %in% 2015:2024]
  }

  if ("schools" %in% features & length(schools_yrs) != 0) {
  
    schools <- retrieve_schools(schools_yrs, geog, geoselect, state = NULL, types) |>
      # filtering out the GEOID column that was added for use with retrieve_aggregated
      dplyr::select(!GEOID)
    
    } else {schools <- empty_sf}
  
  if (is.null(yrs)) {
    libraries_yrs <- 2023
  } else {
    libraries_yrs <- yrs[yrs %in% 2008:2023]
  }
  
  if ("libraries" %in% features & length(libraries_yrs) != 0) {
    
    libraries <- retrieve_libraries(libraries_yrs, geog, geoselect, types)} else {libraries <- empty_sf}
  
  
  if (is.null(yrs)) {
    nonprofits_yrs <- 2023
  } else {
    nonprofits_yrs <- yrs[yrs %in% 1989:2023]
  }
  
  if ("nonprofits" %in% features & length(nonprofits_yrs) != 0) {
    
    nonprofits <- retrieve_nonprofits(nonprofits_yrs, geog, geoselect, types)} else {nonprofits <- empty_sf}
  
  
  if (is.null(yrs)) {
    crashes_yrs <- 2023
  } else {
    crashes_yrs <- yrs[yrs %in% 1999:2024]
  }

  if ("fatal crashes" %in% features & length(crashes_yrs) != 0) {
    
    crashes <- retrieve_crashes(crashes_yrs, geog, geoselect, types)} else {crashes <- empty_sf}

  
  sf_all <- rbind(sf_1, schools, libraries, nonprofits, crashes)

  if (dataset_info == TRUE) {
    dataset_info_df <- retrieve_point_polygon_features() |>
      dplyr::select(c(features, source))

    sf_all <- dplyr::inner_join(sf_all, dataset_info_df, by = dplyr::join_by(features))
  }

  sf_all <- sf_all |>
    dplyr::arrange(features, type, name)

  return(sf_all)
}
