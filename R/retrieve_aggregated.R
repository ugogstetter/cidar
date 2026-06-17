globalVariables(c("CBSA", "CBSA.Code", "County", "County.Code", "County.Name", "DataValue_2001", "DataValue_2023", "GEOID", "GeoFips", "GeoName", "Median.AQI", "NAME", "State", "State.Code", "State.Name", "Year", "bea_key_value", "below_100_percent_poverty_level.estimate", "below_100_percent_poverty_level.moe", "below_150_percent_poverty_level.estimate", "below_150_percent_poverty_level.moe", "census_key_value", "county", "county_code", "county_state", "estimate", "estimate_at_above_150", "estimate_below_100", "estimate_moe", "estimate_total", "estimate_vacant", "gdp", "housing_vacancy.estimate", "housing_vacancy.moe", "moe", "moe_at_above_150", "moe_below_100", "moe_vacant", "percent", "population", "state_abbr", "state_code", "state_name", "variable", "year", "Days.with.AQI", "Unhealthy.Days", "estimate_carpooled", "estimate_drove_alone", "estimate_female_1", "estimate_female_2", "estimate_male_1", "estimate_male_2", "estimate_married_1", "estimate_married_2", "estimate_owner_occupied", "estimate_transit", "homeowners.estimate", "homeowners.moe", "median_aqi", "moe_female_1", "moe_female_2", "moe_male_1", "moe_male_2", "moe_owner_occupied", "moe_transit", "public_transport_commutes.estimate", "public_transport_commutes.moe", "single_parent.estimate", "single_parent.moe", "unhealthy_air_days", "estimate_poverty_total", "moe_poverty_total", "estimate_vacancy_total", "moe_vacancy_total", "estimate_homeownership_total", "moe_homeownership_total", "moe_married_1", "moe_married_2", "moe_drove_alone", "moe_carpooled", "metropolitan_statistical_area", "violent_crime", "motor_vehicle_theft", "counties_principal_cities", "msa", "cbsa", "single_parent_families.estimate", "single_parent_families.moe", "category", "type", "name"))

# possible vars values include unemployment, high-skill employment, median family income, poverty, housing vacancy, median home value, median gross rent, households without vehicle, old-age dependency ratio, households with broadband, homeownership, single-parent families, public transport commutes, mean commute time, Gini index, sex, age, and race
# seems like it always retrieves the ACS 5-year estimates for county and for CBSA?

census_retrieval_vars <- function(vars, yrs, geog, state, geoselect) {
  tidycensus::census_api_key(Sys.getenv("census_key_value"))
  
  # if a state value is not provided but a geoselect value is, and if geog is place, supply state values corresponding to geoselect to input to the tidycensus calls so the function will only have to retrieve the states it needs
  if (is.null(state) & !is.null(geoselect)) {
    
  if (geog == "place") {

    state <- unique(as.vector(substr(geoselect, 1, 2)))
  }
  }

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
  
  if ("sex" %in% vars) {
    variables <- c(variables, male = "DP05_0002P", female = "DP05_0003P")
  }
  
  if ("age" %in% vars) {
    variables <- c(variables, median_age = "DP05_0018", at_above_18 = "DP05_0021P", at_above_65 = "DP05_0024P")
  }
  
  if ("race" %in% vars) {
    variables <- c(variables, hispanic_latino = "DP05_0076P", white = "DP05_0082P", black = "DP05_0083P", american_indian_alaska_native = "DP05_0084P", asian = "DP05_0085P", native_hawaiian_pacific_islander = "DP05_0086P", other_race = "DP05_0087P", two_or_more_races = "DP05_0088P")
  }

  census_data <- data.frame(GEOID = character(), NAME = character(), variable = character(), estimate = double(), moe = double(), year = double())


  if (length(yrs[yrs == 2023]) > 0) {
      census_data_1 <- tidycensus::get_acs(geography = geog, variables = variables, year = 2023, state = state, county = if (geog == "county") {
        geoselect
      } else {
        NULL
      }) |>
        dplyr::mutate(year = 2023)

      census_data <- rbind(census_data, census_data_1)
  }
  
  if (length(yrs[yrs == 2022]) > 0) {
    
    if ("race" %in% vars) {
      variables["hispanic_latino"] <- "DP05_0073P"
      variables["white"] <- "DP05_0079P"
      variables["black"] <- "DP05_0080P"
      variables["american_indian_alaska_native"] <- "DP05_0081P"
      variables["asian"] <- "DP05_0082P"
      variables["native_hawaiian_pacific_islander"] <- "DP05_0083P"
      variables["other_race"] <- "DP05_0084P"
      variables["two_or_more_races"] <- "DP05_0085P"
    }
    
    census_data_1 <- tidycensus::get_acs(geography = geog, variables = variables, year = 2022, state = state, county = if (geog == "county") {
      geoselect
    } else {
      NULL
    }) |>
      dplyr::mutate(year = 2022)
    
    census_data <- rbind(census_data, census_data_1)
  }
  
  if (length(yrs[yrs >= 2020 & yrs <= 2021]) > 0) {
    
    if ("race" %in% vars) {
      variables["hispanic_latino"] <- "DP05_0071P"
      variables["white"] <- "DP05_0077P"
      variables["black"] <- "DP05_0078P"
      variables["american_indian_alaska_native"] <- "DP05_0079P"
      variables["asian"] <- "DP05_0080P"
      variables["native_hawaiian_pacific_islander"] <- "DP05_0081P"
      variables["other_race"] <- "DP05_0082P"
      variables["two_or_more_races"] <- "DP05_0083P"
    }
    
    for (x in yrs[yrs >= 2020 & yrs <= 2021]) {
      census_data_1 <- tidycensus::get_acs(geography = geog, variables = variables, year = x, state = state, county = if (geog == "county") {
        geoselect
      } else {
        NULL
      }) |>
        dplyr::mutate(year = x)
      
      census_data <- rbind(census_data, census_data_1)
    }
  }


  if (length(yrs[yrs == 2019]) > 0) {
    
    if ("race" %in% vars) {
      variables["hispanic_latino"] <- "DP05_0071P"
      variables["white"] <- "DP05_0077P"
      variables["black"] <- "DP05_0078P"
      variables["american_indian_alaska_native"] <- "DP05_0079P"
      variables["asian"] <- "DP05_0080P"
      variables["native_hawaiian_pacific_islander"] <- "DP05_0081P"
      variables["other_race"] <- "DP05_0082P"
      variables["two_or_more_races"] <- "DP05_0083P"
    }
    
    if ("households with broadband" %in% vars) {
      variables["households_with_broadband"] <- "DP02_0153P"
    }

    census_data_1 <- tidycensus::get_acs(geography = geog, variables = variables, year = 2019, state = state, county = if (geog == "county") {
      geoselect
    } else {
      NULL
    }) |>
      dplyr::mutate(year = 2019)

    census_data <- rbind(census_data, census_data_1)
  }


  if (length(yrs[yrs >= 2017 & yrs <= 2018]) > 0) {
    
    if ("race" %in% vars) {
      variables["hispanic_latino"] <- "DP05_0071P"
      variables["white"] <- "DP05_0077P"
      variables["black"] <- "DP05_0078P"
      variables["american_indian_alaska_native"] <- "DP05_0079P"
      variables["asian"] <- "DP05_0080P"
      variables["native_hawaiian_pacific_islander"] <- "DP05_0081P"
      variables["other_race"] <- "DP05_0082P"
      variables["two_or_more_races"] <- "DP05_0083P"
    }
    
    if ("households with broadband" %in% vars) {
      variables["households_with_broadband"] <- "DP02_0152P"
    }


    for (x in yrs[yrs >= 2017 & yrs <= 2018]) {
      census_data_1 <- tidycensus::get_acs(geography = geog, variables = variables, year = x, state = state, county = if (geog == "county") {
        geoselect
      } else {
        NULL
      }) |>
        dplyr::mutate(year = x)

      census_data <- rbind(census_data, census_data_1)
    }
  }


  if (length(yrs[yrs >= 2015 & yrs <= 2016]) > 0) {
    
    if ("age" %in% vars) {
      variables["median_age"] <- "DP05_0017"
      variables["at_above_18"] <- "DP05_0022P"
      variables["at_above_65"] <- "DP05_0025P"
    }
    
    if ("race" %in% vars) {
      variables["hispanic_latino"] <- "DP05_0066P"
      variables["white"] <- "DP05_0072P"
      variables["black"] <- "DP05_0073P"
      variables["american_indian_alaska_native"] <- "DP05_0074P"
      variables["asian"] <- "DP05_0075P"
      variables["native_hawaiian_pacific_islander"] <- "DP05_0076P"
      variables["other_race"] <- "DP05_0077P"
      variables["two_or_more_races"] <- "DP05_0078P"
    }
    
    if ("households with broadband" %in% vars) {
      variables["households_with_broadband"] <- "DP02_0152P"
    }


    if ("old-age dependency ratio" %in% vars) {
      variables["old_age_dependency_ratio"] <- "S0101_C01_033"
    }


    for (x in yrs[yrs >= 2015 & yrs <= 2016]) {
      census_data_1 <- tidycensus::get_acs(geography = geog, variables = variables, year = x, state = state, county = if (geog == "county") {
        geoselect
      } else {
        NULL
      }) |>
        dplyr::mutate(year = x)

      census_data <- rbind(census_data, census_data_1)
    }
  }


  if (length(yrs[yrs >= 2013 & yrs <= 2014]) > 0) {
    
    if ("age" %in% vars) {
      variables["median_age"] <- "DP05_0017"
      variables["at_above_18"] <- "DP05_0022P"
      variables["at_above_65"] <- "DP05_0025P"
    }
    
    if ("race" %in% vars) {
      variables["hispanic_latino"] <- "DP05_0066P"
      variables["white"] <- "DP05_0072P"
      variables["black"] <- "DP05_0073P"
      variables["american_indian_alaska_native"] <- "DP05_0074P"
      variables["asian"] <- "DP05_0075P"
      variables["native_hawaiian_pacific_islander"] <- "DP05_0076P"
      variables["other_race"] <- "DP05_0077P"
      variables["two_or_more_races"] <- "DP05_0078P"
    }
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
      census_data_1 <- tidycensus::get_acs(geography = geog, variables = variables, year = x, state = state, county = if (geog == "county") {
        geoselect
      } else {
        NULL
      }) |>
        dplyr::mutate(year = x)

      census_data <- rbind(census_data, census_data_1)
    }
  }


  if (length(yrs[yrs >= 2010 & yrs <= 2012]) > 0) {
    
    if ("age" %in% vars) {
      variables["median_age"] <- "DP05_0017"
      variables["at_above_18"] <- "DP05_0022P"
      variables["at_above_65"] <- "DP05_0025P"
    }
    
    if ("race" %in% vars) {
      variables["hispanic_latino"] <- "DP05_0066P"
      variables["white"] <- "DP05_0072P"
      variables["black"] <- "DP05_0073P"
      variables["american_indian_alaska_native"] <- "DP05_0074P"
      variables["asian"] <- "DP05_0075P"
      variables["native_hawaiian_pacific_islander"] <- "DP05_0076P"
      variables["other_race"] <- "DP05_0077P"
      variables["two_or_more_races"] <- "DP05_0078P"
    }
    
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
      census_data_1 <- tidycensus::get_acs(geography = geog, variables = variables, year = x, state = state, county = if (geog == "county") {
        geoselect
      } else {
        NULL
      }) |>
        dplyr::mutate(year = x)

      census_data <- rbind(census_data, census_data_1)
    }

    # if households_with_broadband was taken out but not added back in, it will make the 2009 code section below act weird
    variables["households_with_broadband"] <- "placeholder"
  }

  if (length(yrs[yrs == 2009]) > 0) {
    
    if ("age" %in% vars) {
      variables["median_age"] <- "DP05_0017"
      variables["at_above_18"] <- "DP05_0022P"
      variables["at_above_65"] <- "DP05_0025P"
    }
    
    if ("race" %in% vars) {
      variables["hispanic_latino"] <- "DP05_0066P"
      variables["white"] <- "DP05_0072P"
      variables["black"] <- "DP05_0073P"
      variables["american_indian_alaska_native"] <- "DP05_0074P"
      variables["asian"] <- "DP05_0075P"
      variables["native_hawaiian_pacific_islander"] <- "DP05_0076P"
      variables["other_race"] <- "DP05_0077P"
      variables["two_or_more_races"] <- "DP05_0078P"
    }
    
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
      census_data_1 <- tidycensus::get_acs(geography = geog, variables = variables, year = x, state = state, county = if (geog == "county") {
        geoselect
      } else {
        NULL
      }) |>
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
  
  if ("age" %in% vars) {
    age_split <- split(census_data, census_data$variable == "median_age" | census_data$variable == "at_above_18" | census_data$variable == "at_above_65")
    
    census_data_age <- age_split$`TRUE`
    
    # computes percentages of people below age 18, between 18 and 64, and 65 or older
    census_data_age <- census_data_age |>
      tidyr::pivot_wider(names_from = variable, values_from = c(estimate, moe), names_sep = "_") |>
      dplyr::mutate(
        median_age.estimate = estimate_median_age,
        under_age_18.estimate = 100 - estimate_at_above_18,
        age_18_to_64.estimate = estimate_at_above_18 - estimate_at_above_65,
        age_65_plus.estimate = estimate_at_above_65,
        median_age.moe = moe_median_age,
        under_age_18.moe = moe_at_above_18,
        age_18_to_64.moe = round((tidycensus::moe_sum(moe = c(moe_at_above_18, moe_at_above_65), estimate = c(estimate_at_above_18, estimate_at_above_65))), 2),
        age_65_plus.moe = moe_at_above_65
      ) |>
      dplyr::select(c(GEOID, NAME, year, median_age.estimate, under_age_18.estimate, age_18_to_64.estimate, age_65_plus.estimate, median_age.moe, under_age_18.moe, age_18_to_64.moe, age_65_plus.moe)) |>
      tidyr::pivot_longer(cols = median_age.estimate:age_65_plus.moe, names_to = c("variable", "estimate_moe"), values_to = "percent", names_sep = "[.]") |>
      tidyr::pivot_wider(names_from = estimate_moe, values_from = percent)
    
    census_data <- rbind(age_split$`FALSE`, census_data_age)
  }

  census_data <- census_data |>
    dplyr::select(!NAME) |>
    dplyr::mutate(variable = gsub("single parent", "single-parent", gsub("old age", "old-age", gsub("high skill", "high-skill", gsub("_", " ", variable)))))

  return(census_data)
}


bea_retrieval <- function(yrs, geog, geoselect) {
  if (geog == "county") {
    # U.S. county GDP over time

    if (is.null(geoselect)) {
      bea_fips <- "COUNTY"
    } else {
      bea_fips <- paste(geoselect, collapse = ",")
    }

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

    if (is.null(geoselect)) {
      bea_fips <- "MSA"
    } else {
      bea_fips <- paste(geoselect, collapse = ",")
    }

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

aqi_retrieval <- function(vars, yrs, geog, state) {
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

    # matches GEOIDS to counties in the counties AQI dataset, matching on county and state name
    aqi_counties <- county_geoids(state = state, namelsad = FALSE) |>
      dplyr::select(GEOID, name) |>
      dplyr::rename(county_state = name)

    aqi_data <- dplyr::inner_join(aqi_data, aqi_counties, by = dplyr::join_by(county_state)) |>
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

      dplyr::case_when(i == 2005 ~ "https://www2.fbi.gov/ucr/05cius/data/documents/05tbl06.xls",
                       i >= 2006 & i <= 2009 ~ paste0("https://www2.fbi.gov/ucr/cius", as.character(i), "/data/documents/", substr(as.character(i), 3, 4), "tbl06.xls"),
                       i == 2010 | i == 2011 ~ paste0("https://ucr.fbi.gov/crime-in-the-u.s/", as.character(i), "/crime-in-the-u.s.-", as.character(i), "/tables/table-6/output.xls"),
                       i == 2012 | i == 2013 ~ paste0("https://ucr.fbi.gov/crime-in-the-u.s/", as.character(i), "/crime-in-the-u.s.-", as.character(i), "/tables/6tabledatadecpdf/table-6/output.xls"),
                       i == 2014 ~ "https://ucr.fbi.gov/crime-in-the-u.s/2014/crime-in-the-u.s.-2014/tables/table-6/Table_6_Crime_in_the_United_States_by_Metropolitan_Statistical_Area_2014/output.xls",
                       i == 2015 ~ "https://ucr.fbi.gov/crime-in-the-u.s/2015/crime-in-the-u.s.-2015/tables/table-6/table_6_crime_in_the_united_states_by_metropolitan_statistical_area_2015.xls/output.xls",
                       i == 2016 ~ "https://ucr.fbi.gov/crime-in-the-u.s/2016/crime-in-the-u.s.-2016/tables/table-4/table-4/output.xls",
                       i == 2017 | i == 2019 ~ paste0("https://ucr.fbi.gov/crime-in-the-u.s/", as.character(i), "/crime-in-the-u.s.-", as.character(i), "/tables/table-6/table-6/output.xls"),
                       .default = "https://ucr.fbi.gov/crime-in-the-u.s/2018/crime-in-the-u.s.-2018/tables/table-6/table-6.xls/output.xls") |>
      utils::download.file(temp, mode = "wb")

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

schools_retrieval <- function(yrs, geog, state, geoselect) {
  
  retrieve_schools(schools_yrs = yrs, geog = geog, geoselect = geoselect, state = state, types = list("schools" = NULL)) |>
    dplyr::select(c(year, GEOID)) |>
    dplyr::group_by(year, GEOID) |>
    dplyr::mutate(estimate = dplyr::n(), moe = NA, variable = "schools") |>
    dplyr::ungroup() |>
    sf::st_drop_geometry() |>
    as.data.frame() |>
    dplyr::distinct()
}

transit_retrieval <- function(yrs, geog, state, geoselect) {
  
  retrieve_sf_1(features = "transit stops", yrs = yrs, geog = geog, geoselect = geoselect, state = state, types = list("transit stops" = NULL)) |>
    dplyr::select(c(year, GEOID, land_sq_mi)) |>
    dplyr::group_by(year, GEOID) |>
    dplyr::mutate(estimate = round((dplyr::n())/land_sq_mi, 2), moe = NA, variable = "transit stops") |>
    dplyr::ungroup() |>
    dplyr::select(!land_sq_mi) |>
    sf::st_drop_geometry() |>
    as.data.frame() |>
    dplyr::distinct()
}

libraries_retrieval <- function(yrs, geog, state, geoselect) {
  
  retrieve_libraries(libraries_yrs = yrs, geog = geog, geoselect = geoselect, state = state, types = list("libraries" = NULL)) |>
    dplyr::select(c(year, GEOID)) |>
    dplyr::group_by(year, GEOID) |>
    dplyr::mutate(estimate = dplyr::n(), moe = NA, variable = "libraries") |>
    dplyr::ungroup() |>
    sf::st_drop_geometry() |>
    as.data.frame() |>
    dplyr::distinct()
}

nonprofits_retrieval <- function(yrs, geog, state, geoselect) {
  
  retrieve_nonprofits(nonprofits_yrs = yrs, geog = geog, geoselect = geoselect, state = state, types = list("nonprofits" = NULL)) |>
    dplyr::select(c(year, GEOID, land_sq_mi)) |>
    dplyr::group_by(year, GEOID) |>
    dplyr::mutate(estimate = round((dplyr::n())/land_sq_mi, 2), moe = NA, variable = "nonprofits") |>
    dplyr::ungroup() |>
    dplyr::select(!land_sq_mi) |>
    sf::st_drop_geometry() |>
    as.data.frame() |>
    dplyr::distinct()
}

health_nonprofits_retrieval <- function(yrs, geog, state, geoselect) {
  
  retrieve_nonprofits(nonprofits_yrs = yrs, geog = geog, geoselect = geoselect, state = state, types = list("nonprofits" = c("health", "hospital"))) |>
    dplyr::select(c(year, GEOID, land_sq_mi)) |>
    dplyr::group_by(year, GEOID) |>
    dplyr::mutate(estimate = round((dplyr::n())/land_sq_mi, 2), moe = NA, variable = "health-related nonprofits") |>
    dplyr::ungroup() |>
    dplyr::select(!land_sq_mi) |>
    sf::st_drop_geometry() |>
    as.data.frame() |>
    dplyr::distinct()
}

crashes_retrieval <- function(yrs, geog, state, geoselect) {
  
  tidycensus::census_api_key(Sys.getenv("census_key_value"))
  
  crashes_population <- data.frame(GEOID = character(), population = numeric(), year = numeric())
  
  for (x in yrs) {
    crashes_population_1 <- tidycensus::get_acs(geography = geog, variables = "DP05_0001", year = x, state = state, county = if (geog == "county") {
      geoselect
    } else {
      NULL
    }) |>
      dplyr::mutate(year = x, population = estimate) |>
      dplyr::select(c(GEOID, population, year))
    
    crashes_population <- rbind(crashes_population, crashes_population_1)
  }
  
  crashes <- retrieve_crashes(crashes_yrs = yrs, geog = geog, geoselect = geoselect, state = state, types = list("fatal crashes" = NULL)) |>
    dplyr::select(c(year, GEOID))
  
  crashes <- dplyr::inner_join(crashes, crashes_population, by = dplyr::join_by(GEOID, year)) |>
    dplyr::group_by(year, GEOID) |>
    dplyr::mutate(estimate = round(100000*(dplyr::n()/population), 2), moe = NA, variable = "fatal crashes") |>
    dplyr::ungroup() |>
    dplyr::select(!population) |>
    sf::st_drop_geometry() |>
    as.data.frame() |>
    dplyr::distinct()
  
  return(crashes)
}

voting_retrieval <- function(yrs, state, geoselect) {
  
  tidycensus::census_api_key(Sys.getenv("census_key_value"))
  
  ct_included <- FALSE
  
  wi_included <- FALSE
  
  if (is.null(state) & is.null(geoselect)) {
    
    ct_included <- TRUE
    wi_included <- TRUE
   
     }
  
  if ("CT" %in% state) {ct_included <- TRUE}
  
  if (!is.null(geoselect)) {
    
    if (substr(geoselect, 1, 2) == "09") {ct_included <- TRUE}
    
  }
  
  if (("WI" %in% state) & (max(yrs) >= 2018)) {wi_included <- TRUE}
  
  if (!is.null(geoselect)) {
    
    if ((substr(geoselect, 1, 2) == "55") & (max(yrs) >= 2018)) {wi_included <- TRUE}
    
  }
  
  voting_df <- data.frame(year = numeric(), GEOID = character(), ballots_cast = numeric(), state_abbr = character(), jurisdiction_name = character(), full_FIPS = character())
  
  voting_pop <- data.frame(year = numeric(), GEOID = character(), name = character(), adult_citizens = numeric())
  
  for (i in yrs) {
    
    if (i %in% 2022) {
      
      temp <- tempfile()
      
      utils::download.file("https://www.eac.gov/sites/default/files/2023-06/2022_EAVS_for_Public_Release_nolabel_V1_CSV.zip", temp)
      
      voting_1 <- utils::read.csv(utils::unzip(temp)) |>
        dplyr::mutate(year = i, full_FIPS = FIPSCode, state_abbr = State_Abbr, jurisdiction_name = Jurisdiction_Name, ballots_cast = F1a)
      
      unlink(temp)
    }
    
    if (i %in% 2018:2020) {
      
      voting_1 <- dplyr::case_when(i == 2018 ~ "https://www.eac.gov/sites/default/files/Research/EAVS_2018_for_Public_Release_Updates3.csv",
                       i == 2020 ~ "https://www.eac.gov/sites/default/files/EAVS%202020/2020_EAVS_for_Public_Release_nolabel_V2.csv") |>
        read.csv() |>
        dplyr::mutate(year = i, full_FIPS = FIPSCode, state_abbr = State_Abbr, jurisdiction_name = Jurisdiction_Name, ballots_cast = F1a)
      
    }
    
    if (i == 2016) {
      
      temp <- tempfile()
  
      utils::download.file("https://www.eac.gov/sites/default/files/Research/EAVS_2016_Final_Data_for_Public_Release_v2.csv.zip", temp)
    
      voting_1 <- utils::read.csv(utils::unzip(temp)) |>
        dplyr::mutate(year = i, full_FIPS = FIPSCode, state_abbr = State, jurisdiction_name = JurisdictionName, ballots_cast = F1a)
    
      unlink(temp)
      
    }
      
    if (i == 2014 | i == 2010) {
      
      temp <- tempfile()
      
      dplyr::case_when(i == 2014 ~ "https://www.eac.gov/sites/default/files/eac_assets/1/1/2014_EAVS_Excel_Files1.zip",
                       i == 2010 ~ "https://www.eac.gov/sites/default/files/eac_assets/1/28/Final%20EAVS%20Data%20-%20Sections%20C%20to%20F_EXCEL.zip") |>
        utils::download.file(temp)
      
      temp2 <- tempfile()
      
      voting_1 <- readxl::read_xlsx(utils::unzip(temp,
                                                 files = dplyr::case_when(i == 2014 ~ "EAVS_Section_F.xlsx", 
                                                                          i == 2010 ~ "EAVS Section F.xlsx"),
                                                 exdir = temp2)) |>
        dplyr::mutate(year = i, full_FIPS = FIPSCode, state_abbr = State, jurisdiction_name = Jurisdiction, ballots_cast = QF1a)
      
      unlink(temp)
      
      unlink(temp2)
      
    }
 
    if (i == 2012) {
      
      temp <- tempfile()
      
      utils::download.file("https://www.eac.gov/sites/default/files/eac_assets/1/6/Excel%20Files-Part%202.zip", temp)
      
      temp2 <- tempfile()
      
      voting_1 <- readxl::read_xls(utils::unzip(temp, exdir = temp2))|>
        dplyr::mutate(year = i, full_FIPS = `FIPSCode...3`, state_abbr = State, jurisdiction_name = Jurisdiction, ballots_cast = QF1a)
      
      unlink(temp)
      
      unlink(temp2)
    }
    
    voting_1 <- voting_1 |>
      dplyr::mutate(full_FIPS = leading_zeroes(full_FIPS, 10)) |>
      dplyr::mutate(GEOID = substr(full_FIPS, 1, 5), ballots_cast = as.numeric(ballots_cast)) |>
      dplyr::filter(ballots_cast >= 0) |>
      dplyr::select(c(year, GEOID, ballots_cast, state_abbr, jurisdiction_name, full_FIPS))
    
    voting_df <- rbind(voting_df, voting_1)
    
    if (i == 2010) {
      
      voting_pop_1 <- tidycensus::get_acs(geography = "county",
                                          variables = c("B16008_009", "B16008_023"),
                                          year = i, state = state, county = geoselect) |>
        dplyr::select(!moe) |>
        tidyr::pivot_wider(names_from = "variable", values_from = "estimate") |>
        dplyr::mutate(adult_citizens = B16008_009 + B16008_023, year = i, name = NAME) |>
        dplyr::select(c(GEOID, name, year, adult_citizens))
      
    } else {
    
      voting_pop_1 <- tidycensus::get_acs(geography = "county",
                                          variables = dplyr::case_when(i %in% 2012:2014 ~ "S1601_C01_024",
                                                                       i %in% 2016:2022 ~ "S1601_C01_020"),
                                          year = i, state = state, county = geoselect) |>
        dplyr::mutate(year = i, adult_citizens = estimate, name = NAME) |>
        dplyr::select(c(GEOID, name, year, adult_citizens))
     
    }
    
    voting_pop <- rbind(voting_pop, voting_pop_1)
  }
  
  if (wi_included == TRUE) {
    
     # preparing version of counties dataset to merge with Wisconsin data
     wi_counties <- voting_pop |>
       dplyr::mutate(merge_name = toupper(name))
     
     # isolating data on Wisconsin cities/towns with names and counties but without effective corresponding GEOIDs
     merge_wi <- split(voting_df, (voting_df$state_abbr == "WI") & (voting_df$year %in% 2018:2022))$`TRUE` |>
       dplyr::select(!GEOID) |>
       # removing Wisconsin cities/towns stretching across multiple counties
       dplyr::filter(gsub(".*- ", "", jurisdiction_name) != "MULTIPLE COUNTIES") |>
       dplyr::mutate(merge_name = paste0(gsub(".*- ", "", jurisdiction_name), ", WISCONSIN"))
     
     # assigning effective GEOIDs to Wisconsin cities/towns
     merge_wi <- dplyr::left_join(merge_wi, wi_counties, by = dplyr::join_by(merge_name, year)) |>
       dplyr::select(!merge_name)
     
     # isolating data without ineffective Wisconsin cities/towns
     voting_df <- split(voting_df, (voting_df$state_abbr == "WI") & (voting_df$year %in% 2018:2022))$`FALSE`
     
  }
  
  if (ct_included == TRUE) {
    
     # retrieving CSV to match CT cities/towns with planning regions instead of them now being designated with the old counties which are not in the counties dataset
     ct_match <- read.csv("https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-to-planning-region/refs/heads/main/ct-town-to-planning-region.csv") |>
       dplyr::mutate(ct_GEOID = leading_zeroes(ce_fips_2022, 5), state_abbr = "CT", full_FIPS = leading_zeroes(town_fips_2020, 10)) |>
       dplyr::select(c(ct_GEOID, full_FIPS, state_abbr))
     
     # assigning effective GEOIDs (corresponding to planning regions) to CT cities/towns
     voting_df <- dplyr::left_join(voting_df, ct_match, by = dplyr::join_by(full_FIPS, state_abbr)) |>
       dplyr::mutate(GEOID = dplyr::case_when(!is.na(ct_GEOID) ~ ct_GEOID,
                                              .default = GEOID)) |>
       dplyr::select(!ct_GEOID)
     
  }
     
  # assigning effective GEOIDs to all other cities/towns without GEOID info corresponding to their county, except for cities/towns spanning across multiple states
  voting_df <- voting_df |>
    dplyr::mutate(GEOID = dplyr::case_when(full_FIPS == "1706613000" ~ "17113",
                                           full_FIPS == "1714000000" ~ "17031",
                                           full_FIPS == "1722255000" ~ "17163",
                                           full_FIPS == "1728326000" ~ "17095",
                                           full_FIPS == "1759000000" ~ "17143",
                                           year <= 2012 & full_FIPS %in% c("2309999901", "2309999902", "2309999903", "2309999904", "2309999905") ~ "23031",
                                           year > 2012 & full_FIPS == "2309999901" ~ "23017",
                                           year > 2012 & full_FIPS == "2309999902" ~ "23005",
                                           year > 2012 & full_FIPS %in% c("2309999903", "2309999904") ~ "23029",
                                           year > 2012 & full_FIPS == "2309999905" ~ "23025",
                                           full_FIPS == "3612295082" ~ "36123",
                                           full_FIPS == "4611300000" ~ "46102",
                                           full_FIPS == "5151500000" ~ "51019",
                  .default = GEOID))
  
  # merging with valid GEOIDs (almost all GEOIDs at this point are valid)
  voting_df <- dplyr::inner_join(voting_df, voting_pop, by = dplyr::join_by(GEOID, year)) |>
    # filtering out US territories
    dplyr::filter(!(state_abbr %in% c("AS", "GU", "MP", "PR", "VI")))
  
  if (wi_included == TRUE) {
  
      # merging with Wisconsin data
      voting_df <- rbind(voting_df, merge_wi)
      
  }
  
  # computing total numbers of voting-age citizens and ballots cast in each county, and using this info to compute voter turnout
  voting_df <- voting_df |>
    dplyr::group_by(GEOID, year) |>
    dplyr::mutate(adult_citizens = sum(adult_citizens), ballots_cast = sum(ballots_cast)) |>
    dplyr::mutate(estimate = round((ballots_cast/adult_citizens)*100, 2), moe = NA, variable = "voter turnout") |>
    dplyr::select(c(GEOID, year, estimate, moe, variable)) |>
    dplyr::ungroup() |>
    dplyr::distinct()
  
  return(voting_df)
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
#'  argument equal to a call to the [categories_vars()] function, which will
#'  return a vector of the desired variables.
#' * As a data frame, specifically a version of the [retrieve_aggregated_vars()]
#'  data frame filtered to only the rows corresponding to variables you wish to
#'  retrieve.
#' @param yrs If yrs is a numeric vector, this function will retrieve all
#' specified varaibles for all years in the specified vector. If yrs is `NULL`,
#' this function will retrieve all most recent years of selected variables If yrs is not provided,
#' retrieve all available years for all variables associated with specific years.
#'  The years for which data is available for each variable are included in the
#'   [retrieve_aggregated_vars()] dataset. Please note that if `yrs` is 
#'   equal to `NA` in that dataset, the variable can only be retrieved in 
#'   this [retrieve_aggregated()] function when `yrs` is set to `NULL`.
#' @param geog Specify `"county"`, `"cbsa"` (core based statistical
#' area), or `"place"` (cities/towns/villages/census-designated places). The
#' geographic area types for which data is available for each variable are
#' included in the [retrieve_aggregated_vars()] data frame.
#' @param state Optionally (ONLY if `geog` is set to `"county"` or `"place"`, 
#' and `geoselect` is `NULL`),
#' supply a vector of state abbreviations to filter to. By default, all possible
#'  values will be returned by the function.
#' @param geoselect Optionally (ONLY if `state` is `NULL`), supply a vector (in character format) of county, CBSA, or place
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
  if (is.null(yrs)) {
    census_yrs <- 2023
  } else {
  census_yrs <- yrs[yrs >= 2009 & yrs <= 2023]
  }

  if (all(("unemployment" %in% vars | "high-skill employment" %in% vars | "median family income" %in% vars | "poverty" %in% vars | "old-age dependency ratio" %in% vars | "housing vacancy" %in% vars | "Gini index" %in% vars | "median home value" %in% vars | "median gross rent" %in% vars | "households without vehicle" %in% vars | "households with broadband" %in% vars | "homeownership" %in% vars | "single-parent families" %in% vars | "public transport commutes" %in% vars | "mean commute time" %in% vars | "sex" %in% vars | "age" %in% vars | "race" %in% vars), length(census_yrs) != 0, (!identical(vars, "old-age dependency ratio") | !identical(yrs, 2009)), (!identical(vars, "Gini index") | !identical(yrs, 2009)), (!identical(vars, c("old-age dependency ratio", "Gini index")) | !identical(yrs, 2009)), (!identical(vars, "households with broadband") | sum(match(yrs, 2013:2024), na.rm = TRUE) > 0))) {
    census_vars <- vars[vars %in% c("unemployment", "high-skill employment", "median family income", "poverty", "old-age dependency ratio", "housing vacancy", "Gini index", "median home value", "median gross rent", "households without vehicle", "households with broadband", "homeownership", "single-parent families", "public transport commutes", "mean commute time", "sex", "age", "race")]

    census_data <- census_retrieval_cleaning(vars = census_vars, yrs = census_yrs, geog = geog, state = state, geoselect = geoselect)
  } else {
    census_data <- empty_df
  }

if (is.null(yrs)) {
  bea_yrs <- 2023
} else {
  bea_yrs <- yrs[yrs >= 2001 & yrs <= 2023]
}

  if ("GDP per capita" %in% vars & length(bea_yrs) != 0 & (geog == "county" | geog == "cbsa")) {
    bea_data <- bea_retrieval(yrs = bea_yrs, geog = geog, geoselect = geoselect)
  } else {
    bea_data <- empty_df
  }


  if (is.null(yrs)) {
    aqi_yrs <- 2024
  } else {
  aqi_yrs <- yrs[yrs >= 1980 & yrs <= 2024]
  }

  if (("median AQI" %in% vars | "unhealthy air days" %in% vars) & length(aqi_yrs) != 0 & (geog == "county" | geog == "cbsa")) {
    aqi_vars <- vars[vars %in% c("median AQI", "unhealthy air days")]

    aqi_data <- aqi_retrieval(vars = aqi_vars, yrs = aqi_yrs, geog = geog, state = state)
  } else {
    aqi_data <- empty_df
  }


  if (is.null(yrs)) {
    crime_yrs <- 2024
  } else {
  crime_yrs <- yrs[yrs >= 1999 & yrs <= 2024]
  }

  if (("violent crime" %in% vars | "murder and nonnegligent manslaughter" %in% vars | "rape" %in% vars | "robbery" %in% vars | "aggravated assault" %in% vars | "property crime" %in% vars | "burglary" %in% vars | "larceny" %in% vars | "motor vehicle theft" %in% vars) & length(crime_yrs) != 0 & geog == "cbsa") {
    crime_vars <- vars[vars %in% c("violent crime", "murder and nonnegligent manslaughter", "rape", "robbery", "aggravated assault", "property crime", "burglary", "larceny", "motor vehicle theft")]

    crime_data <- crime_retrieval(vars = crime_vars, yrs = crime_yrs)
  } else {
    crime_data <- empty_df
  }
  
  
  if (is.null(yrs)) {
    schools_yrs <- 2024
  } else {
  schools_yrs <- yrs[yrs >= 2015 & yrs <= 2024]
  }
  
  if ("schools" %in% vars & length(schools_yrs) != 0)  {
    schools_data <- schools_retrieval(yrs = schools_yrs, geog = geog, state = state, geoselect = geoselect)
  } else {
    schools_data <- empty_df
  }
  
  if ("transit stops" %in% vars & is.null(yrs)) {
    transit_data <- transit_retrieval(yrs = yrs, geog = geog, state = state, geoselect = geoselect)
  } else {
    transit_data <- empty_df
  }
  
  if (is.null(yrs)) {
    libraries_yrs <- 2023
  } else {
    libraries_yrs <- yrs[yrs >= 2008 & yrs <= 2023]
  }
  
  if ("libraries" %in% vars & length(libraries_yrs) != 0)  {
    libraries_data <- libraries_retrieval(yrs = libraries_yrs, geog = geog, state = state, geoselect = geoselect)
  } else {
    libraries_data <- empty_df
  }
  
  if (is.null(yrs)) {
    nonprofits_yrs <- 2023
  } else {
    nonprofits_yrs <- yrs[yrs >= 1989 & yrs <= 2023]
  }
  
  if ("nonprofits" %in% vars & length(nonprofits_yrs) != 0)  {
    nonprofits_data <- nonprofits_retrieval(yrs = nonprofits_yrs, geog = geog, state = state, geoselect = geoselect)
  } else {
    nonprofits_data <- empty_df
  }
  
  if ("health-related nonprofits" %in% vars & length(nonprofits_yrs) != 0)  {
    health_nonprofits_data <- health_nonprofits_retrieval(yrs = nonprofits_yrs, geog = geog, state = state, geoselect = geoselect)
  } else {
    health_nonprofits_data <- empty_df
  }
  
  if ("fatal crashes" %in% vars & length(census_yrs) != 0)  {
    crashes_data <- crashes_retrieval(yrs = census_yrs, geog = geog, state = state, geoselect = geoselect)
  } else {
    crashes_data <- empty_df
  }
  
  if (is.null(yrs)) {
    voting_yrs <- 2022
  } else {
    voting_yrs <- yrs[yrs %in% c(2010, 2012, 2014, 2016, 2018, 2020, 2022)]
  }
  
  if ("voter turnout" %in% vars & length(voting_yrs) != 0 & geog == "county")  {
    voting_data <- voting_retrieval(yrs = voting_yrs, state = state, geoselect = geoselect)
  } else {
    voting_data <- empty_df
  }

  data_all <- rbind(census_data, bea_data, aqi_data, crime_data, schools_data, transit_data, libraries_data, nonprofits_data, health_nonprofits_data, crashes_data, voting_data)


  # retrieving county/CBSA/place names to join with data
  if (geog == "county") {
    geog_county <- county_geoids(state = state, sf = sf) |>
      dplyr::select(!state_abbr)

    data_all <- dplyr::inner_join(geog_county, data_all, by = dplyr::join_by(GEOID))

    data_all <- data_all[, c("GEOID", "name", "year", "variable", "estimate", "moe")]
  }

  if (geog == "cbsa") {
    geog_cbsa <- cbsa_geoids(sf = sf)

    data_all <- dplyr::inner_join(geog_cbsa, data_all, by = dplyr::join_by(GEOID))

    data_all <- data_all[, c("GEOID", "name", "year", "variable", "estimate", "moe")]
  }

  if (geog == "place") {
    
    if (is.null(state) & !is.null(geoselect)) {
      
      # creating vector of state abbreviations just for requested places so tigris call in place_geoids will run faster
      place_states <- tigris::states(year = 2023) |>
        dplyr::filter(GEOID %in% as.vector(substr(geoselect, 1, 2)))
      
      place_states <- as.vector(place_states$STUSPS)
      
    } else {
      
      place_states <- state
    }
    
    geog_place <- place_geoids(state = place_states, sf = sf) |>
      dplyr::select(!state_abbr)

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
    
    age_subset <- subset(vars_info_df, variable == "age")
    
    age_subset_2 <- age_subset_3 <- age_subset_4 <- age_subset
    
    age_subset$variable <- "median age"
    
    age_subset_2$variable <- "under age 18"
    
    age_subset_3$variable <- "age 18 to 64"
    
    age_subset_4$variable <- "age 65 plus"
    
    vars_info_df <- vars_info_df |>
      dplyr::filter(variable != "age")
    
    vars_info_df <- rbind(vars_info_df, age_subset, age_subset_2, age_subset_3, age_subset_4)

    data_all <- dplyr::inner_join(data_all, vars_info_df, by = dplyr::join_by(variable))
  }

  data_all <- data_all |>
    dplyr::filter(!is.na(estimate)) |>
    dplyr::arrange(GEOID, year, variable, name)

  return(data_all)
}
