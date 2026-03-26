globalVariables(c("CBSA", "CBSA.Code", "County", "County.Code", "County.Name", "DataValue_2001", "DataValue_2023", "GEOID", "GeoFips", "GeoName", "Median.AQI", "NAME", "State", "State.Code", "State.Name", "Year", "bea_key_value", "below_100_percent_poverty_level.estimate", "below_100_percent_poverty_level.moe", "below_150_percent_poverty_level.estimate", "below_150_percent_poverty_level.moe", "census_key_value", "county", "county_code", "county_state", "estimate", "estimate_at_above_150", "estimate_below_100", "estimate_moe", "estimate_total", "estimate_vacant", "gdp", "housing_vacancy.estimate", "housing_vacancy.moe", "moe", "moe_at_above_150", "moe_below_100", "moe_vacant", "percent", "population", "state_abbr", "state_code", "state_name", "variable", "year", "Days.with.AQI", "Unhealthy.Days", "estimate_carpooled", "estimate_drove_alone", "estimate_female_1", "estimate_female_2", "estimate_male_1", "estimate_male_2", "estimate_married_1", "estimate_married_2", "estimate_owner_occupied", "estimate_transit", "homeowners.estimate", "homeowners.moe", "median_aqi", "moe_female_1", "moe_female_2", "moe_male_1", "moe_male_2", "moe_owner_occupied", "moe_transit", "public_transport_commutes.estimate", "public_transport_commutes.moe", "single_parent.estimate", "single_parent.moe", "unhealthy_air_days", "estimate_poverty_total", "moe_poverty_total", "estimate_vacancy_total", "moe_vacancy_total", "estimate_homeownership_total", "moe_homeownership_total", "moe_married_1", "moe_married_2", "moe_drove_alone", "moe_carpooled", "metropolitan_statistical_area", "violent_crime", "motor_vehicle_theft", "counties_principal_cities", "msa", "cbsa", "single_parent_families.estimate", "single_parent_families.moe", "category", "type"))

# possible vars values include unemployment, high-skill employment, median family income, poverty, housing vacancy, median home value, median gross rent, households without vehicle, old-age dependency ratio, households with broadband, homeownership, single-parent families, public transport commutes, mean commute time, and Gini index
# seems like it always retrieves the ACS 5-year estimates for county and for CBSA?

census_retrieval_vars <- function(vars, yrs, geog, state, geoselect) {
  tidycensus::census_api_key(Sys.getenv("census_key_value"))

  variables <- c()

  if ("unemployment" %in% vars) {
    # retrieves percent of civilian labor force aged 16+ that is unemployed
    variables <- c(variables, unemployment = "DP03_0005P")
  }

  if ("high-skill employment" %in% vars) {
    # retrieves percent of civilian employed population aged 16+ in management, professional, and related occupations
    variables <- c(variables, high_skill_employment = "DP03_0027P")
  }

  if ("median family income" %in% vars) {
    # retrieves median family income
    variables <- c(variables, median_family_income = "DP03_0087")
  }

  if ("median home value" %in% vars) {
    # retrieves median home value for owner-occupied units
    variables <- c(variables, median_home_value = "B25077_001")
  }

  if ("mean commute time" %in% vars) {
    # retrieves mean travel time to work in minutes for workers age 16+
    variables <- c(variables, mean_commute_time = "DP03_0025")
  }

  if ("median gross rent" %in% vars) {
    # retrieves median gross rent
    variables <- c(variables, median_gross_rent = "B25064_001")
  }

  if ("poverty" %in% vars) {
    variables <- c(variables, below_100 = "B06012_002", at_above_150 = "B06012_004", poverty_total = "B06012_001")
  }

  if ("old-age dependency ratio" %in% vars) {
    variables <- c(variables, old_age_dependency_ratio = "S0101_C01_035")
  }

  if ("housing vacancy" %in% vars) {
    variables <- c(variables, vacant = "B25002_003", vacancy_total = "B25002_001")
  }

  if ("Gini index" %in% vars) {
    variables <- c(variables, Gini_index = "B19083_001")
  }

  if ("households without vehicle" %in% vars) {
    variables <- c(variables, households_without_vehicle = "DP04_0058P")
  }

  if ("households with broadband" %in% vars) {
    variables <- c(variables, households_with_broadband = "DP02_0154P")
  }

  if ("homeownership" %in% vars) {
    variables <- c(variables, owner_occupied = "B25008_002", homeownership_total = "B25008_001")
  }

  if ("single-parent families" %in% vars) {
    variables <- c(variables, male_1 = "B17010_011", female_1 = "B17010_017", male_2 = "B17010_031", female_2 = "B17010_037", married_1 = "B17010_004", married_2 = "B17010_024")
  }

  if ("public transport commutes" %in% vars) {
    variables <- c(variables, transit = "DP03_0021", drove_alone = "DP03_0019", carpooled = "DP03_0020")
  }


  census_data <- data.frame(GEOID = character(), NAME = character(), variable = character(), estimate = double(), moe = double(), year = double())


  if (length(yrs[yrs >= 2020]) > 0) {
    for (x in yrs[yrs >= 2020]) {
      census_data_1 <- tidycensus::get_acs(geography = geog, variables = variables, year = x, state = state, county = if (geog == "county") {geoselect} else {NULL}) |>
        dplyr::mutate(year = x)

      census_data <- rbind(census_data, census_data_1)
    }
  }


  if (length(yrs[yrs == 2019]) > 0) {
    if ("households with broadband" %in% vars) {
      variables["households_with_broadband"] <- "DP02_0153P"
    }

    census_data_1 <- tidycensus::get_acs(geography = geog, variables = variables, year = 2019, state = state, county = if (geog == "county") {geoselect} else {NULL}) |>
      dplyr::mutate(year = 2019)

    census_data <- rbind(census_data, census_data_1)
  }


  if (length(yrs[yrs >= 2017 & yrs <= 2018]) > 0) {
    if ("households with broadband" %in% vars) {
      variables["households_with_broadband"] <- "DP02_0152P"
    }


    for (x in yrs[yrs >= 2017 & yrs <= 2018]) {
      census_data_1 <- tidycensus::get_acs(geography = geog, variables = variables, year = x, state = state, county = if (geog == "county") {geoselect} else {NULL}) |>
        dplyr::mutate(year = x)

      census_data <- rbind(census_data, census_data_1)
    }
  }


  if (length(yrs[yrs >= 2015 & yrs <= 2016]) > 0) {
    if ("households with broadband" %in% vars) {
      variables["households_with_broadband"] <- "DP02_0152P"
    }


    if ("old-age dependency ratio" %in% vars) {
      variables["old_age_dependency_ratio"] <- "S0101_C01_033"
    }


    for (x in yrs[yrs >= 2015 & yrs <= 2016]) {
      census_data_1 <- tidycensus::get_acs(geography = geog, variables = variables, year = x, state = state, county = if (geog == "county") {geoselect} else {NULL}) |>
        dplyr::mutate(year = x)

      census_data <- rbind(census_data, census_data_1)
    }
  }


  if (length(yrs[yrs >= 2013 & yrs <= 2014]) > 0) {
    if ("households with broadband" %in% vars) {
      variables["households_with_broadband"] <- "DP02_0152P"
    }


    if ("old-age dependency ratio" %in% vars) {
      variables["old_age_dependency_ratio"] <- "S0101_C01_033"
    }


    if ("households without vehicle" %in% vars) {
      variables["households_without_vehicle"] <- "DP04_0057P"
    }


    for (x in yrs[yrs >= 2013 & yrs <= 2014]) {
      census_data_1 <- tidycensus::get_acs(geography = geog, variables = variables, year = x, state = state, county = if (geog == "county") {geoselect} else {NULL}) |>
        dplyr::mutate(year = x)

      census_data <- rbind(census_data, census_data_1)
    }
  }


  if (length(yrs[yrs >= 2010 & yrs <= 2012]) > 0) {
    if ("households with broadband" %in% vars) {
      variables <- variables[!variables == variables["households_with_broadband"]]
    }


    if ("old-age dependency ratio" %in% vars) {
      variables["old_age_dependency_ratio"] <- "S0101_C01_033"
    }


    if ("households without vehicle" %in% vars) {
      variables["households_without_vehicle"] <- "DP04_0057P"
    }


    for (x in yrs[yrs >= 2010 & yrs <= 2012]) {
      census_data_1 <- tidycensus::get_acs(geography = geog, variables = variables, year = x, state = state, county = if (geog == "county") {geoselect} else {NULL}) |>
        dplyr::mutate(year = x)

      census_data <- rbind(census_data, census_data_1)
    }

    # if households_with_broadband was taken out but not added back in, it will make the 2009 code section below act weird
    variables["households_with_broadband"] <- "placeholder"
  }

  if (length(yrs[yrs == 2009]) > 0) {
    if ("households with broadband" %in% vars) {
      variables <- variables[!variables == variables["households_with_broadband"]]
    }


    if ("old-age dependency ratio" %in% vars) {
      variables <- variables[!variables == variables["old_age_dependency_ratio"]]
    }


    if ("households without vehicle" %in% vars) {
      variables["households_without_vehicle"] <- "DP04_0057P"
    }


    if ("Gini index" %in% vars) {
      variables <- variables[!variables == variables["Gini_index"]]
    }


    for (x in yrs[yrs == 2009]) {
      census_data_1 <- tidycensus::get_acs(geography = geog, variables = variables, year = x, state = state, county = if (geog == "county") {geoselect} else {NULL}) |>
        dplyr::mutate(year = x)

      census_data <- rbind(census_data, census_data_1)
    }
  }

  return(census_data)
}

census_retrieval_cleaning <- function(vars, yrs, geog, state, geoselect) {
  
  census_data <- census_retrieval_vars(vars, yrs, geog, state, geoselect)

  
  if ("poverty" %in% vars) {
    poverty_split <- split(census_data, census_data$variable == "below_100" | census_data$variable == "at_above_150" | census_data$variable == "poverty_total")

    census_data_poverty <- poverty_split$`TRUE`

    # computes percent of population whose income in the past 12 months was below 100% and below 150% of the poverty level
    census_data_poverty <- census_data_poverty |>
      tidyr::pivot_wider(names_from = variable, values_from = c(estimate, moe), names_sep = "_") |>
      dplyr::mutate(
        below_150_percent_poverty_level.estimate = round(((estimate_poverty_total - estimate_at_above_150) / estimate_poverty_total) * 100, 2),
        below_100_percent_poverty_level.estimate = round((estimate_below_100 / estimate_poverty_total) * 100, 2),
        below_150_percent_poverty_level.moe = round((tidycensus::moe_prop(num = (estimate_poverty_total - estimate_at_above_150), denom = estimate_poverty_total, moe_num = ifelse(sum(c(estimate_poverty_total, estimate_at_above_150) == 0, na.rm = TRUE) > 1, tidycensus::moe_sum(moe = c(moe_poverty_total, moe_at_above_150), estimate = c(estimate_poverty_total, estimate_at_above_150)), tidycensus::moe_sum(moe = c(moe_poverty_total, moe_at_above_150), estimate = NULL)), moe_denom = moe_poverty_total)) * 100, 2),
        below_100_percent_poverty_level.moe = round((tidycensus::moe_prop(num = estimate_below_100, denom = estimate_poverty_total, moe_num = moe_below_100, moe_denom = moe_poverty_total)) * 100, 2)
      ) |>
      dplyr::select(c(GEOID, NAME, year, below_150_percent_poverty_level.estimate, below_100_percent_poverty_level.estimate, below_150_percent_poverty_level.moe, below_100_percent_poverty_level.moe)) |>
      tidyr::pivot_longer(cols = below_150_percent_poverty_level.estimate:below_100_percent_poverty_level.moe, names_to = c("variable", "estimate_moe"), values_to = "percent", names_sep = "[.]") |>
      tidyr::pivot_wider(names_from = estimate_moe, values_from = percent)

    census_data <- rbind(poverty_split$`FALSE`, census_data_poverty)
  }

  if ("housing vacancy" %in% vars) {
    vacancy_split <- split(census_data, census_data$variable == "vacant" | census_data$variable == "vacancy_total")

    census_data_vacancy <- vacancy_split$`TRUE`

    # computes percent of housing units vacant
    # prob make sure there isn't an easier way to retrieve this
    census_data_vacancy <- census_data_vacancy |>
      tidyr::pivot_wider(names_from = variable, values_from = c(estimate, moe), names_sep = "_") |>
      dplyr::mutate(
        housing_vacancy.estimate = round((estimate_vacant / estimate_vacancy_total) * 100, 2),
        housing_vacancy.moe = round((tidycensus::moe_prop(num = estimate_vacant, denom = estimate_vacancy_total, moe_num = moe_vacant, moe_denom = moe_vacancy_total)) * 100, 2)
      ) |>
      dplyr::select(c(GEOID, NAME, year, housing_vacancy.estimate, housing_vacancy.moe)) |>
      tidyr::pivot_longer(cols = housing_vacancy.estimate:housing_vacancy.moe, names_to = c("variable", "estimate_moe"), values_to = "percent", names_sep = "[.]") |>
      tidyr::pivot_wider(names_from = estimate_moe, values_from = percent)

    census_data <- rbind(vacancy_split$`FALSE`, census_data_vacancy)
  }

  if ("homeownership" %in% vars) {
    homeownership_split <- split(census_data, census_data$variable == "owner_occupied" | census_data$variable == "homeownership_total")

    census_data_homeownership <- homeownership_split$`TRUE`

    # computes percent of population in owner-occupied housing units
    census_data_homeownership <- census_data_homeownership |>
      tidyr::pivot_wider(names_from = variable, values_from = c(estimate, moe), names_sep = "_") |>
      dplyr::mutate(
        homeowners.estimate = round((estimate_owner_occupied / estimate_homeownership_total) * 100, 2),
        homeowners.moe = round((tidycensus::moe_prop(num = estimate_owner_occupied, denom = estimate_homeownership_total, moe_num = moe_owner_occupied, moe_denom = moe_homeownership_total)) * 100, 2)
      ) |>
      dplyr::select(c(GEOID, NAME, year, homeowners.estimate, homeowners.moe)) |>
      tidyr::pivot_longer(cols = homeowners.estimate:homeowners.moe, names_to = c("variable", "estimate_moe"), values_to = "percent", names_sep = "[.]") |>
      tidyr::pivot_wider(names_from = estimate_moe, values_from = percent)

    census_data <- rbind(homeownership_split$`FALSE`, census_data_homeownership)
  }

  if ("single-parent families" %in% vars) {
    single_parent_split <- split(census_data, census_data$variable == "male_1" | census_data$variable == "female_1" | census_data$variable == "male_2" | census_data$variable == "female_2" | census_data$variable == "married_1" | census_data$variable == "married_2")

    census_data_single_parent <- single_parent_split$`TRUE`

    # computes percent of families with related children under 18 that are single-parent
    census_data_single_parent <- census_data_single_parent |>
      tidyr::pivot_wider(names_from = variable, values_from = c(estimate, moe), names_sep = "_") |>
      dplyr::mutate(
        single_parent_families.estimate = round(((estimate_male_1 + estimate_female_1 + estimate_male_2 + estimate_female_2) / (estimate_male_1 + estimate_female_1 + estimate_male_2 + estimate_female_2 + estimate_married_1 + estimate_married_2)) * 100, 2),
        single_parent_families.moe = round((tidycensus::moe_prop(num = (estimate_male_1 + estimate_female_1 + estimate_male_2 + estimate_female_2), denom = (estimate_male_1 + estimate_female_1 + estimate_male_2 + estimate_female_2 + estimate_married_1 + estimate_married_2), moe_num = ifelse(sum(c(estimate_male_1, estimate_female_1, estimate_male_2, estimate_female_2) == 0, na.rm = TRUE) > 1, tidycensus::moe_sum(moe = c(moe_male_1, moe_female_1, moe_male_2, moe_female_2), estimate = c(estimate_male_1, estimate_female_1, estimate_male_2, estimate_female_2)), tidycensus::moe_sum(moe = c(moe_male_1, moe_female_1, moe_male_2, moe_female_2), estimate = NULL)), moe_denom = ifelse(sum(c(estimate_male_1, estimate_female_1, estimate_male_2, estimate_female_2, estimate_married_1, estimate_married_2) == 0, na.rm = TRUE) > 1, tidycensus::moe_sum(moe = c(moe_male_1, moe_female_1, moe_male_2, moe_female_2, moe_married_1, moe_married_2), estimate = c(estimate_male_1, estimate_female_1, estimate_male_2, estimate_female_2, estimate_married_1, estimate_married_2)), tidycensus::moe_sum(moe = c(moe_male_1, moe_female_1, moe_male_2, moe_female_2, moe_married_1, moe_married_2), estimate = NULL)))) * 100, 2)
      ) |>
      dplyr::select(c(GEOID, NAME, year, single_parent_families.estimate, single_parent_families.moe)) |>
      tidyr::pivot_longer(cols = single_parent_families.estimate:single_parent_families.moe, names_to = c("variable", "estimate_moe"), values_to = "percent", names_sep = "[.]") |>
      tidyr::pivot_wider(names_from = estimate_moe, values_from = percent)

    census_data <- rbind(single_parent_split$`FALSE`, census_data_single_parent)
  }

  if ("public transport commutes" %in% vars) {
    public_transport_commutes_split <- split(census_data, census_data$variable == "transit" | census_data$variable == "drove_alone" | census_data$variable == "carpooled")

    census_data_public_transport_commutes <- public_transport_commutes_split$`TRUE`

    # computes percent of motorized commutes to work made by public transportation
    census_data_public_transport_commutes <- census_data_public_transport_commutes |>
      tidyr::pivot_wider(names_from = variable, values_from = c(estimate, moe), names_sep = "_") |>
      dplyr::mutate(
        public_transport_commutes.estimate = round((estimate_transit / (estimate_drove_alone + estimate_carpooled + estimate_transit)) * 100, 2),
        public_transport_commutes.moe = round((tidycensus::moe_prop(num = estimate_transit, denom = (estimate_drove_alone + estimate_carpooled + estimate_transit), moe_num = moe_transit, moe_denom = ifelse(sum(c(estimate_drove_alone, estimate_carpooled, estimate_transit) == 0, na.rm = TRUE) > 1, tidycensus::moe_sum(moe = c(moe_drove_alone, moe_carpooled, moe_transit), estimate = c(estimate_drove_alone, estimate_carpooled, estimate_transit)), tidycensus::moe_sum(moe = c(moe_drove_alone, moe_carpooled, moe_transit), estimate = NULL)))) * 100, 2)
      ) |>
      dplyr::select(c(GEOID, NAME, year, public_transport_commutes.estimate, public_transport_commutes.moe)) |>
      tidyr::pivot_longer(cols = public_transport_commutes.estimate:public_transport_commutes.moe, names_to = c("variable", "estimate_moe"), values_to = "percent", names_sep = "[.]") |>
      tidyr::pivot_wider(names_from = estimate_moe, values_from = percent)

    census_data <- rbind(public_transport_commutes_split$`FALSE`, census_data_public_transport_commutes)
  }

  census_data <- census_data |>
    dplyr::select(!NAME) |>
    dplyr::mutate(variable = gsub("single parent", "single-parent", gsub("old age", "old-age", gsub("high skill", "high-skill", gsub("_", " ", variable)))))

  return(census_data)
}


bea_retrieval <- function(yrs, geog, geoselect) {
  if (geog == "county") {
    # U.S. county GDP over time
    
    if (is.null(geoselect)) {bea_fips <- "COUNTY"} else {bea_fips <- paste(geoselect, collapse = ",")}
    
    bea_year <- paste(as.character(yrs), collapse = ",")
    
    userSpecList <- list(
      "UserID" = Sys.getenv("bea_key_value"),
      "Method" = "GetData",
      "datasetname" = "Regional",
      "TableName" = "CAGDP1",
      "LineCode" = 1,
      "GeoFips" = bea_fips,
      "Year" = bea_year
    )
    
    county_GDP <- bea.R::beaGet(userSpecList, asTable = TRUE) |>
      tidyr::pivot_longer(cols = paste0("DataValue_", as.character(min(yrs))):paste0("DataValue_", as.character(max(yrs))), names_to = "year", values_to = "gdp") |>
      dplyr::mutate(
        year = as.numeric(substr(year, 11, 14)),
        gdp = gdp * 1000
      ) |>
      dplyr::select(GeoFips, gdp, year)
    # resulting gdp variable is in chained 2017 dollars


    # U.S. county population over time

    userSpecList <- list(
      "UserID" = Sys.getenv("bea_key_value"),
      "Method" = "GetData",
      "datasetname" = "Regional",
      "TableName" = "CAINC1",
      "LineCode" = 2,
      "GeoFips" = bea_fips,
      "Year" = bea_year
    )

    county_population <- bea.R::beaGet(userSpecList, asTable = TRUE) |>
      tidyr::pivot_longer(cols = paste0("DataValue_", as.character(min(yrs))):paste0("DataValue_", as.character(max(yrs))), names_to = "year", values_to = "population") |>
      dplyr::mutate(year = as.numeric(substr(year, 11, 14))) |>
      dplyr::select(GeoFips, population, year, GeoName)


    # U.S. county GDP per capita over time

    bea_data <- dplyr::inner_join(county_GDP, county_population, by = c("GeoFips", "year")) |>
      dplyr::mutate(estimate = round(gdp / population, 2)) |>
      dplyr::mutate(
        moe = NA,
        variable = "GDP per capita"
      ) |>
      dplyr::rename(GEOID = GeoFips) |>
      dplyr::select(c(GEOID, variable, estimate, moe, year))
  }

  if (geog == "cbsa") {
    # U.S. MSA GDP over time

    if (is.null(geoselect)) {bea_fips <- "MSA"} else {bea_fips <- paste(geoselect, collapse = ",")}
    
    bea_year <- paste(as.character(yrs), collapse = ",")
    
    userSpecList <- list(
      "UserID" = Sys.getenv("bea_key_value"),
      "Method" = "GetData",
      "datasetname" = "Regional",
      "TableName" = "CAGDP1",
      "LineCode" = 1,
      "GeoFips" = bea_fips,
      "Year" = bea_year
    )

    msa_GDP <- bea.R::beaGet(userSpecList, asTable = TRUE) |>
      tidyr::pivot_longer(cols = paste0("DataValue_", as.character(min(yrs))):paste0("DataValue_", as.character(max(yrs))), names_to = "year", values_to = "gdp") |>
      dplyr::mutate(
        year = as.numeric(substr(year, 11, 14)),
        gdp = gdp * 1000
      ) |>
      dplyr::select(GeoFips, gdp, year)
    # resulting gdp variable is in chained 2017 dollars


    # U.S. MSA population over time

    userSpecList <- list(
      "UserID" = Sys.getenv("bea_key_value"),
      "Method" = "GetData",
      "datasetname" = "Regional",
      "TableName" = "CAINC1",
      "LineCode" = 2,
      "GeoFips" = bea_fips,
      "Year" = bea_year
    )

    msa_population <- bea.R::beaGet(userSpecList, asTable = TRUE) |>
      tidyr::pivot_longer(cols = paste0("DataValue_", as.character(min(yrs))):paste0("DataValue_", as.character(max(yrs))), names_to = "year", values_to = "population") |>
      dplyr::mutate(year = as.numeric(substr(year, 11, 14))) |>
      dplyr::select(GeoFips, population, year, GeoName)


    # U.S. MSA GDP per capita over time

    bea_data <- dplyr::inner_join(msa_GDP, msa_population, by = c("GeoFips", "year")) |>
      dplyr::filter(GeoName != "United States (Metropolitan Portion)") |>
      dplyr::mutate(
        estimate = round(gdp / population, 2),
        moe = NA,
        variable = "GDP per capita"
      ) |>
      dplyr::rename(GEOID = GeoFips) |>
      dplyr::select(c(GEOID, variable, estimate, moe, year))
  }

  return(bea_data)
}


# should I be concerned that some counties don't seem to have many days with AQI? (see column in original datasets)
# the value of geog can be either "county" or "cbsa"

aqi_retrieval <- function(vars, yrs, geog) {
  if (geog == "county") {
    temp <- tempfile()

    utils::download.file(paste0("https://aqs.epa.gov/aqsweb/airdata/annual_aqi_by_county_", as.character(yrs[1]), ".zip"), temp)

    temp2 <- tempfile()

    aqi_data <- utils::read.csv(utils::unzip(temp, exdir = temp2)) |>
      dplyr::select(c(State, County, Year, Median.AQI, Days.with.AQI, Unhealthy.Days))

    unlink(temp)

    unlink(temp2)

    if (length(yrs) > 1) {
      for (x in yrs[-1]) {
        temp <- tempfile()

        utils::download.file(paste0("https://aqs.epa.gov/aqsweb/airdata/annual_aqi_by_county_", as.character(x), ".zip"), temp)

        temp2 <- tempfile()

        aqi_data_1 <- utils::read.csv(utils::unzip(temp, exdir = temp2)) |>
          dplyr::select(c(State, County, Year, Median.AQI, Days.with.AQI, Unhealthy.Days))

        unlink(temp)

        unlink(temp2)

        aqi_data <- rbind(aqi_data, aqi_data_1)
      }
    }

    if ("median AQI" %in% vars & "unhealthy air days" %in% vars) {
      aqi_data <- aqi_data |>
        dplyr::mutate(
          unhealthy_air_days = round((Unhealthy.Days / Days.with.AQI) * 100, 2),
          county_state = paste0(County, ", ", State),
          moe = NA
        ) |>
        dplyr::select(!c(County, State, Unhealthy.Days, Days.with.AQI)) |>
        dplyr::rename(median_aqi = Median.AQI, year = Year) |>
        tidyr::pivot_longer(cols = c(median_aqi, unhealthy_air_days), names_to = "variable", values_to = "estimate") |>
        dplyr::mutate(variable = dplyr::case_when(
          variable == "median_aqi" ~ "median AQI",
          variable == "unhealthy_air_days" ~ "unhealthy air days"
        ))
    } else if (vars == "median AQI") {
      aqi_data <- aqi_data |>
        dplyr::rename(year = Year, estimate = Median.AQI) |>
        dplyr::mutate(
          moe = NA,
          variable = "median AQI",
          county_state = paste0(County, ", ", State)
        ) |>
        dplyr::select(!c(County, State, Unhealthy.Days, Days.with.AQI))
    } else {
      aqi_data <- aqi_data |>
        dplyr::mutate(unhealthy_air_days = round((Unhealthy.Days / Days.with.AQI) * 100, 2)) |>
        dplyr::rename(year = Year, estimate = unhealthy_air_days) |>
        dplyr::mutate(moe = NA, variable = "unhealthy air days", county_state = paste0(County, ", ", State)) |>
        dplyr::select(!c(County, State, Unhealthy.Days, Days.with.AQI, Median.AQI))
    }

    temp <- tempfile()

    utils::download.file("https://aqs.epa.gov/aqsweb/airdata/aqs_monitors.zip", temp)

    temp2 <- tempfile()

    # matches the GEOIDs in this spreadsheet, also from the EPA, to counties in the counties AQI dataset, matching on county and state name
    aqs_monitors <- utils::read.csv(utils::unzip(temp, exdir = temp2)) |>
      dplyr::select(State.Code, County.Code, State.Name, County.Name) |>
      dplyr::distinct() |>
      dplyr::mutate(
        County.Code = sprintf("%03d", County.Code),
        GEOID = paste0(State.Code, County.Code),
        county_state = paste0(County.Name, ", ", State.Name)
      ) |>
      dplyr::select(c(GEOID, county_state))

    unlink(temp)

    unlink(temp2)

    aqi_data <- dplyr::inner_join(aqi_data, aqs_monitors, by = dplyr::join_by(county_state)) |>
      dplyr::select(!county_state)
  }

  if (geog == "cbsa") {
    temp <- tempfile()

    utils::download.file(paste0("https://aqs.epa.gov/aqsweb/airdata/annual_aqi_by_cbsa_", as.character(yrs[1]), ".zip"), temp)

    temp2 <- tempfile()

    aqi_data <- utils::read.csv(utils::unzip(temp, exdir = temp2)) |>
      dplyr::select(c(CBSA.Code, Year, Median.AQI, Days.with.AQI, Unhealthy.Days))

    unlink(temp)

    unlink(temp2)

    if (length(yrs) > 1) {
      for (x in yrs[-1]) {
        temp <- tempfile()

        utils::download.file(paste0("https://aqs.epa.gov/aqsweb/airdata/annual_aqi_by_cbsa_", as.character(x), ".zip"), temp)

        temp2 <- tempfile()

        aqi_data_1 <- utils::read.csv(utils::unzip(temp, exdir = temp2)) |>
          dplyr::select(c(CBSA.Code, Year, Median.AQI, Days.with.AQI, Unhealthy.Days))

        unlink(temp)

        unlink(temp2)

        aqi_data <- rbind(aqi_data, aqi_data_1)
      }
    }

    if ("median AQI" %in% vars & "unhealthy air days" %in% vars) {
      aqi_data <- aqi_data |>
        dplyr::mutate(unhealthy_air_days = round((Unhealthy.Days / Days.with.AQI) * 100, 2), moe = NA) |>
        dplyr::rename(median_aqi = Median.AQI, year = Year, GEOID = CBSA.Code) |>
        tidyr::pivot_longer(cols = c(median_aqi, unhealthy_air_days), names_to = "variable", values_to = "estimate") |>
        dplyr::select(!c(Unhealthy.Days, Days.with.AQI)) |>
        dplyr::mutate(variable = dplyr::case_when(
          variable == "median_aqi" ~ "median AQI",
          variable == "unhealthy_air_days" ~ "unhealthy air days"
        ))
    } else if (vars == "median AQI") {
      aqi_data <- aqi_data |>
        dplyr::rename(year = Year, estimate = Median.AQI, GEOID = CBSA.Code) |>
        dplyr::mutate(moe = NA, variable = "median AQI") |>
        dplyr::select(!c(Unhealthy.Days, Days.with.AQI))
    } else {
      aqi_data <- aqi_data |>
        dplyr::mutate(unhealthy_air_days = round((Unhealthy.Days / Days.with.AQI) * 100, 2)) |>
        dplyr::rename(year = Year, estimate = unhealthy_air_days, GEOID = CBSA.Code) |>
        dplyr::mutate(moe = NA, variable = "unhealthy air days") |>
        dplyr::select(!c(Unhealthy.Days, Days.with.AQI, Median.AQI))
    }
  }

  return(aqi_data)
}


crime_retrieval <- function(vars, yrs) {
  crime_data <- data.frame(msa = character(), variable = character(), estimate = double(), year = double())

  if (length(yrs[yrs >= 1999 & yrs <= 2004]) > 0) {
    for (i in yrs[yrs >= 1999 & yrs <= 2004]) {
      temp <- tempfile(fileext = ".xls")

      utils::download.file(ifelse(i <= 2003, paste0("https://ucr.fbi.gov/crime-in-the-u.s/", as.character(i), "/table6_metro", substr(as.character(i), 3, 4), ".xls"), "https://www2.fbi.gov/ucr/cius_04/documents/04tbl06a.xls"), temp, mode = "wb")

      crime_data_1 <- readxl::read_xls(temp, skip = ifelse(i == 2001 | i == 2003, 4, 3), .name_repair = ~ tolower(gsub("Larceny_theft", "larceny", gsub("_+", "_", gsub("Forcible_rape", "rape", gsub("\\s|-|/", "_", gsub("\\d", "", gsub("man.*slaughter", "manslaughter", gsub("non-negligent", "nonnegligent", .x))))))))) |>
        dplyr::select(c("metropolitan_statistical_area", "violent_crime", "property_crime", "murder_and_nonnegligent_manslaughter", "rape", "robbery", "aggravated_assault", "burglary", "larceny", "motor_vehicle_theft")) |>
        dplyr::filter(grepl("M.S.A.", metropolitan_statistical_area) | metropolitan_statistical_area == "Rate per 100,000 inhabitants") |>
        # preventing duplicate rate per 100,000 inhabitants values, since those are sometimes given for M.D.s as well as for M.S.A.s
        dplyr::filter(dplyr::lag(metropolitan_statistical_area) != metropolitan_statistical_area | is.na(dplyr::lag(metropolitan_statistical_area)))

      unlink(temp)

      msas <- crime_data_1$metropolitan_statistical_area[rep(c(TRUE, FALSE), nrow(crime_data_1) / 2)]
      msas <- gsub("\\d", "", msas)

      crime_data_1 <- crime_data_1 |>
        dplyr::filter(metropolitan_statistical_area == "Rate per 100,000 inhabitants") |>
        dplyr::mutate(
          msa = msas,
          dplyr::across(
            violent_crime:motor_vehicle_theft,
            ~ dplyr::case_when(. == "-" ~ NA, .default = .)
          )
        ) |>
        dplyr::select(!metropolitan_statistical_area) |>
        dplyr::mutate(dplyr::across(violent_crime:motor_vehicle_theft, as.double)) |>
        tidyr::pivot_longer(cols = violent_crime:motor_vehicle_theft, names_to = "variable", values_to = "estimate") |>
        dplyr::mutate(estimate = round(estimate, 2), year = i, variable = gsub("_", " ", variable))

      crime_data <- rbind(crime_data, crime_data_1)
    }
  }


  if (length(yrs[yrs >= 2005 & yrs <= 2019]) > 0) {
    for (i in yrs[yrs >= 2005 & yrs <= 2019]) {
      temp <- tempfile(fileext = ".xls")

      utils::download.file(ifelse(i == 2005, "https://www2.fbi.gov/ucr/05cius/data/documents/05tbl06.xls",
        ifelse(i >= 2006 & i <= 2009, paste0("https://www2.fbi.gov/ucr/cius", as.character(i), "/data/documents/", substr(as.character(i), 3, 4), "tbl06.xls"),
          ifelse(i == 2010 | i == 2011, paste0("https://ucr.fbi.gov/crime-in-the-u.s/", as.character(i), "/crime-in-the-u.s.-", as.character(i), "/tables/table-6/output.xls"),
            ifelse(i == 2012 | i == 2013, paste0("https://ucr.fbi.gov/crime-in-the-u.s/", as.character(i), "/crime-in-the-u.s.-", as.character(i), "/tables/6tabledatadecpdf/table-6/output.xls"),
              ifelse(i == 2014, "https://ucr.fbi.gov/crime-in-the-u.s/2014/crime-in-the-u.s.-2014/tables/table-6/Table_6_Crime_in_the_United_States_by_Metropolitan_Statistical_Area_2014/output.xls",
                ifelse(i == 2015, "https://ucr.fbi.gov/crime-in-the-u.s/2015/crime-in-the-u.s.-2015/tables/table-6/table_6_crime_in_the_united_states_by_metropolitan_statistical_area_2015.xls/output.xls",
                  ifelse(i == 2016, "https://ucr.fbi.gov/crime-in-the-u.s/2016/crime-in-the-u.s.-2016/tables/table-4/table-4/output.xls",
                    ifelse(i == 2017 | i == 2019, paste0("https://ucr.fbi.gov/crime-in-the-u.s/", as.character(i), "/crime-in-the-u.s.-", as.character(i), "/tables/table-6/table-6/output.xls"),
                      "https://ucr.fbi.gov/crime-in-the-u.s/2018/crime-in-the-u.s.-2018/tables/table-6/table-6.xls/output.xls"
                    )
                  )
                )
              )
            )
          )
        )
      ), temp, mode = "wb")

      crime_data_2 <- readxl::read_xls(temp, skip = 3, .name_repair = ~ tolower(gsub("Larceny_theft", "larceny", gsub("_+", "_", gsub("Forcible_rape", "rape", gsub("\\s|-|/", "_", gsub("\\d", "", gsub("man.*slaughter", "manslaughter", gsub("non-negligent", "nonnegligent", .x))))))))) |>
        dplyr::select(c("metropolitan_statistical_area", "counties_principal_cities", "violent_crime", "property_crime", "murder_and_nonnegligent_manslaughter", "rape", "robbery", "aggravated_assault", "burglary", "larceny", "motor_vehicle_theft")) |>
        dplyr::mutate(metropolitan_statistical_area = dplyr::case_when(counties_principal_cities == "Rate per 100,000 inhabitants" ~ "Rate per 100,000 inhabitants",
          .default = metropolitan_statistical_area
        )) |>
        dplyr::filter(grepl("M.S.A.", metropolitan_statistical_area) | metropolitan_statistical_area == "Rate per 100,000 inhabitants") |>
        # preventing duplicate rate per 100,000 inhabitants values, since those are sometimes given for M.D.s as well as for M.S.A.s
        dplyr::filter(dplyr::lag(metropolitan_statistical_area) != metropolitan_statistical_area | is.na(dplyr::lag(metropolitan_statistical_area)))

      unlink(temp)

      msas <- crime_data_2$metropolitan_statistical_area[rep(c(TRUE, FALSE), nrow(crime_data_2) / 2)]
      msas <- gsub("\\d", "", msas)

      crime_data_2 <- crime_data_2 |>
        dplyr::filter(metropolitan_statistical_area == "Rate per 100,000 inhabitants") |>
        dplyr::mutate(
          msa = msas,
          dplyr::across(
            violent_crime:motor_vehicle_theft,
            ~ dplyr::case_when(. == "-" ~ NA, .default = .)
          )
        ) |>
        dplyr::select(!c(metropolitan_statistical_area, counties_principal_cities)) |>
        dplyr::mutate(dplyr::across(violent_crime:motor_vehicle_theft, as.double)) |>
        tidyr::pivot_longer(cols = violent_crime:motor_vehicle_theft, names_to = "variable", values_to = "estimate") |>
        dplyr::mutate(estimate = round(estimate, 2), year = i, variable = gsub("_", " ", variable))

      crime_data <- rbind(crime_data, crime_data_2)
    }
  }

  if (length(yrs[yrs == 2020 | (yrs >= 2022 & yrs <= 2024)]) > 0) {
    for (i in yrs[yrs == 2020 | (yrs >= 2022 & yrs <= 2024)]) {
      crime_data_3 <- eval(parse(text = paste0("crime_by_msa_", as.character(i)))) |>
        dplyr::mutate(year = i)

      crime_data <- rbind(crime_data, crime_data_3)
    }
  }

  crime_data <- crime_data[crime_data$variable %in% vars, ]

  crime_data <- crime_data |>
    dplyr::mutate(msa = substr(msa, 1, nchar(msa) - 7))

  geog_cbsa <- cbsa_geoids() |>
    dplyr::filter(substr(name, nchar(name) - 9, nchar(name)) == "Metro Area") |>
    dplyr::mutate(msa = substr(name, 1, nchar(name) - 11), moe = NA)

  crime_data <- dplyr::inner_join(crime_data, geog_cbsa, by = dplyr::join_by(msa)) |>
    dplyr::select(!c(msa, name))

  return(crime_data)
}


#' Retrieves and combines data, aggregated by geographic area, from various sources
#' @description Retrieves data from sources such as the Census Bureau, Bureau of
#' Economic Analysis, and Environmental Protection Agency. User specifies
#' variables, timeframe, and geographic region of data to retrieve. Function
#' will return a dataset combining all variables requested.
#' @param vars Before supplying this argument, ensure you have read the
#' [retrieve_aggregated_vars()] function description, and have run that function to
#' return the data frame of retrievable variables. The `vars` argument of this
#' [retrieve_aggregated()] function can be supplied in one of two ways:
#' * As a vector of all variables you wish to retrieve. To retrieve all 
#' variables within one or more categories as given in the 
#' [retrieve_aggregated_vars()] data frame, set [retrieve_aggregated()]'s `vars`
#'  argument equal to a call to the [with_categories()] function, which will 
#'  return a vector of the desired variables.
#' * As a data frame, specifically a version of the [retrieve_aggregated_vars()]
#'  data frame filtered to only the rows corresponding to variables you wish to 
#'  retrieve.
#' @param yrs Supply a vector (in numeric format) of one or more years to filter
#'  to. If no value is supplied, will
#' default to the largest possible range of years across which data is available
#'  on at least one of the selected variables. The years for which data is
#'  available for each variable are included in the [retrieve_aggregated_vars()]
#'  dataset.
#' @param geog Specify `"county"`, `"cbsa"` (core based statistical
#' area), or `"place"` (cities/towns/villages/census-designated places). The
#' geographic area types for which data is available for each variable are
#' included in the [retrieve_aggregated_vars()] data frame.
#' @param state Optionally (ONLY if `geog` is set to `"county"` or `"place"`),
#' supply a vector of state abbreviations to filter to. By default, all possible
#'  values will be returned by the function.
#' @param geoselect Optionally, supply a vector (in character format) of county, CBSA, or place
#' (depending on whether `geog` is set to `"county"`, `"cbsa"`, or `"place"`)
#' GEOIDs to filter to. By default, all possible values will be returned by the
#' function. To determine the corresponding GEOID for a particular county, CBSA,
#'  or place, use [county_geoids()], [cbsa_geoids()], or [place_geoids()],
#'  respectively.
#' @param vars_info Specify whether to append columns containing
#'  category, source, and type information for each variable in this function's
#'   output data frame. Default value is `TRUE`. The appended information will
#'   be the same as that contained in the [retrieve_aggregated_vars()] data frame.
#' @param sf If `FALSE` (default value), function returns a data frame without
#'  geospatial polygon geometry. If `TRUE`, function returns a shapefile with
#'  geospatial polygon geometry.
#' @export

retrieve_aggregated <- function(vars, yrs = 1980:2024, geog, state = NULL, geoselect = NULL, vars_info = TRUE, sf = FALSE) {
  UseMethod("retrieve_aggregated", vars)
}

#' @rdname retrieve_aggregated
#' @export
retrieve_aggregated.data.frame <- function(vars, yrs = 1980:2024, geog, state = NULL, geoselect = NULL, vars_info = TRUE, sf = FALSE) {
  vars <- vars$variable

  retrieve_aggregated(vars, yrs, geog, state, geoselect, vars_info, sf)
}

#' @rdname retrieve_aggregated
#' @export
retrieve_aggregated.character <- function(vars, yrs = 1980:2024, geog, state = NULL, geoselect = NULL, vars_info = TRUE, sf = FALSE) {

  # creating an empty dataframe with column names for use when rbind-ing datasets together
  empty_df <- data.frame(matrix(nrow = 0, ncol = 5))
  colnames(empty_df) <- c("GEOID", "variable", "estimate", "moe", "year")


  # filtering based on the availability of ACS data for counties and CBSAs (which seems to always be 5-year)
  census_yrs <- yrs[yrs >= 2009 & yrs <= 2023]

  if (all(("unemployment" %in% vars | "high-skill employment" %in% vars | "median family income" %in% vars | "poverty" %in% vars | "old-age dependency ratio" %in% vars | "housing vacancy" %in% vars | "Gini index" %in% vars | "median home value" %in% vars | "median gross rent" %in% vars | "households without vehicle" %in% vars | "households with broadband" %in% vars | "homeownership" %in% vars | "single-parent families" %in% vars | "public transport commutes" %in% vars | "mean commute time" %in% vars), length(census_yrs) != 0, (!identical(vars, "old-age dependency ratio") | !identical(yrs, 2009)), (!identical(vars, "Gini index") | !identical(yrs, 2009)), (!identical(vars, c("old-age dependency ratio", "Gini index")) | !identical(yrs, 2009)), (!identical(vars, "households with broadband") | sum(match(yrs, 2013:2024), na.rm = TRUE) > 0))) {
    census_vars <- vars[vars %in% c("unemployment", "high-skill employment", "median family income", "poverty", "old-age dependency ratio", "housing vacancy", "Gini index", "median home value", "median gross rent", "households without vehicle", "households with broadband", "homeownership", "single-parent families", "public transport commutes", "mean commute time")]

    census_data <- census_retrieval_cleaning(vars = census_vars, yrs = census_yrs, geog = geog, state = state, geoselect = geoselect)
  } else {
    census_data <- empty_df
  }


  bea_yrs <- yrs[yrs >= 2001 & yrs <= 2023]

  if ("GDP per capita" %in% vars & length(bea_yrs) != 0 & (geog == "county" | geog == "cbsa")) {
    bea_data <- bea_retrieval(yrs = bea_yrs, geog = geog, geoselect = geoselect)
  } else {
    bea_data <- empty_df
  }


  aqi_yrs <- yrs[yrs >= 1980 & yrs <= 2024]

  if (("median AQI" %in% vars | "unhealthy air days" %in% vars) & length(aqi_yrs) != 0 & (geog == "county" | geog == "cbsa")) {
    aqi_vars <- vars[vars %in% c("median AQI", "unhealthy air days")]

    aqi_data <- aqi_retrieval(vars = aqi_vars, yrs = aqi_yrs, geog = geog)
  } else {
    aqi_data <- empty_df
  }


  crime_yrs <- yrs[yrs >= 1999 & yrs <= 2024]

  if (("violent crime" %in% vars | "murder and nonnegligent manslaughter" %in% vars | "rape" %in% vars | "robbery" %in% vars | "aggravated assault" %in% vars | "property crime" %in% vars | "burglary" %in% vars | "larceny" %in% vars | "motor vehicle theft" %in% vars) & length(crime_yrs) != 0 & geog == "cbsa") {
    crime_vars <- vars[vars %in% c("violent crime", "murder and nonnegligent manslaughter", "rape", "robbery", "aggravated assault", "property crime", "burglary", "larceny", "motor vehicle theft")]

    crime_data <- crime_retrieval(vars = crime_vars, yrs = crime_yrs)
  } else {
    crime_data <- empty_df
  }

  data_all <- rbind(census_data, bea_data, aqi_data, crime_data)


  # retrieving county/CBSA/place names to join with data
  if (geog == "county") {
    geog_county <- county_geoids(state = state, sf = sf)

    data_all <- dplyr::inner_join(geog_county, data_all, by = dplyr::join_by(GEOID))

    data_all <- data_all[, c("GEOID", "name", "state_abbr", "year", "variable", "estimate", "moe")]
  }

  if (geog == "cbsa") {
    geog_cbsa <- cbsa_geoids(sf = sf)

    data_all <- dplyr::inner_join(geog_cbsa, data_all, by = dplyr::join_by(GEOID))

    data_all <- data_all[, c("GEOID", "name", "year", "variable", "estimate", "moe")]
  }

  if (geog == "place") {
    geog_place <- place_geoids(state = state, sf = sf)

    data_all <- dplyr::inner_join(geog_place, data_all, by = dplyr::join_by(GEOID))

    data_all <- data_all[, c("GEOID", "name", "year", "variable", "estimate", "moe")]
  }

  if (!is.null(geoselect)) {
    data_all <- data_all |>
      dplyr::filter(GEOID %in% geoselect)
  }

  if (vars_info == TRUE) {
    vars_info_df <- retrieve_aggregated_vars() |>
      dplyr::select(c(variable, category, source, type))

    poverty_subset <- subset(vars_info_df, variable == "poverty")

    poverty_subset_2 <- poverty_subset

    poverty_subset$variable <- "below 150 percent poverty level"

    poverty_subset_2$variable <- "below 100 percent poverty level"

    vars_info_df <- vars_info_df |>
      dplyr::filter(variable != "poverty")

    vars_info_df <- rbind(vars_info_df, poverty_subset, poverty_subset_2)

    data_all <- dplyr::inner_join(data_all, vars_info_df)
  }

  data_all <- data_all |>
    dplyr::filter(!is.na(estimate)) |>
    dplyr::arrange(GEOID, year)

  return(data_all)
}
