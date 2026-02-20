globalVariables(c("CBSA", "CBSA.Code", "County", "County.Code", "County.Name", "DataValue_2001", "DataValue_2023", "GEOID", "GeoFips", "GeoName", "Median.AQI", "NAME", "State", "State.Code", "State.Name", "Year", "bea_key_value", "below_100_percent_poverty_level.estimate", "below_100_percent_poverty_level.moe", "below_150_percent_poverty_level.estimate", "below_150_percent_poverty_level.moe", "census_key_value", "county", "county_code", "county_state", "estimate", "estimate_at_above_150", "estimate_below_100", "estimate_moe", "estimate_total", "estimate_vacant", "gdp", "housing_vacancy.estimate", "housing_vacancy.moe", "moe", "moe_at_above_150", "moe_below_100", "moe_vacant", "percent", "population", "state_abbr", "state_code", "state_name", "variable", "year", "Days.with.AQI", "Unhealthy.Days", "estimate_carpooled", "estimate_drove_alone", "estimate_female_1", "estimate_female_2", "estimate_male_1", "estimate_male_2", "estimate_married_1", "estimate_married_2", "estimate_owner_occupied", "estimate_transit", "homeowners.estimate", "homeowners.moe", "median_aqi", "moe_female_1", "moe_female_2", "moe_male_1", "moe_male_2", "moe_owner_occupied", "moe_transit", "public_transport_commutes.estimate", "public_transport_commutes.moe", "single_parent.estimate", "single_parent.moe", "unhealthy_air_days"))

# possible vars values include unemployment, high-skill employment, median family income, poverty, housing vacancy, median home value, median gross rent, households without vehicle, old-age dependency ratio, households with broadband, homeownership, single-parent families, public transport commutes, mean commute time, and Gini index
# seems like it always retrieves the ACS 5-year estimates for county and for CBSA?

census_retrieval_vars <- function(vars, yrs, geog) {

  tidycensus::census_api_key(Sys.getenv("census_key_value"))
    
    variables <- c()
    
    if ("unemployment" %in% vars) {
      
      # retrieves percent of civilian labor force aged 16+ that is unemployed
      variables <- c(variables, unemployment = "DP03_0005P") }
    
    if ("high-skill employment" %in% vars) {
      
      # retrieves percent of civilian employed population aged 16+ in management, professional, and related occupations
      variables <- c(variables, high_skill_employment = "DP03_0027P")}
    
    if ("median family income" %in% vars) {
      
      # retrieves median family income
      variables <- c(variables, median_family_income = "DP03_0087")}
    
    if ("median home value" %in% vars) {
      
      # retrieves median home value for owner-occupied units
      variables <- c(variables, median_home_value = "B25077_001")}
    
    if ("mean commute time" %in% vars) {
      
      # retrieves mean travel time to work in minutes for workers age 16+
      variables <- c(variables, mean_commute_time = "DP03_0025")}
    
    if ("median gross rent" %in% vars) {
      
      # retrieves median gross rent
      variables <- c(variables, median_gross_rent = "B25064_001")}
    
    if ("poverty" %in% vars) {
      
      variables <- c(variables, below_100 = "B06012_002", at_above_150 = "B06012_004", poverty_total = "B06012_001")}
    
    if ("old-age dependency ratio" %in% vars) {
      
      variables <- c(variables, age_ratio = "S0101_C01_035")}
    
    if ("housing vacancy" %in% vars) {
      
      variables <- c(variables, vacant = "B25002_003", vacancy_total = "B25002_001")}
    
    if ("Gini index" %in% vars) {
      
      variables <- c(variables, gini = "B19083_001")}
    
    if ("households without vehicle" %in% vars) {
      
      variables <- c(variables, without_vehicle = "DP04_0058P")}
    
    if ("households with broadband" %in% vars) {
      
      variables <- c(variables, with_broadband = "DP02_0154P")}
    
    if ("homeownership" %in% vars) {
      
      variables <- c(variables, owner_occupied = "B25008_002", homeownership_total = "B25008_001")}
    
    if ("single-parent families" %in% vars) {
      
      variables <- c(variables, male_1 = "B17010_011", female_1 = "B17010_017", male_2 = "B17010_031", female_2 = "B17010_037", married_1 = "B17010_004", married_2 = "B17010_024")}
    
    if ("public transport commutes" %in% vars) {
      
      variables <- c(variables, transit = "DP03_0021", drove_alone = "DP03_0019", carpooled = "DP03_0020")}
    
    
    census_data <- data.frame(GEOID = character(), NAME = character(), variable = character(), estimate = double(), moe = double(), year = double())
    
    
    if (length(yrs[yrs >= 2020]) > 0) {
      
      for (x in yrs[yrs >= 2020]) {
        
        census_data_1 <- tidycensus::get_acs(geography = geog, variables = variables, year = x) |>
          dplyr::mutate(year = x)
        
        census_data <- rbind(census_data, census_data_1)
        
      }}
    
    
    if (length(yrs[yrs == 2019]) > 0) {
      
      if ("households with broadband" %in% vars) {
        
        variables["with_broadband"] <- "DP02_0153P"}
      
      census_data_1 <- tidycensus::get_acs(geography = geog, variables = variables, year = 2019) |>
        dplyr::mutate(year = 2019)
      
      census_data <- rbind(census_data, census_data_1)
    }
    
    
    if (length(yrs[yrs >= 2017 & yrs <= 2018]) > 0) {
      
      if ("households with broadband" %in% vars) {
        
        variables["with_broadband"] <- "DP02_0152P"}
      
      
      for (x in yrs[yrs >= 2017 & yrs <= 2018]) {
        
        census_data_1 <- tidycensus::get_acs(geography = geog, variables = variables, year = x) |>
          dplyr::mutate(year = x)
        
        census_data <- rbind(census_data, census_data_1)
        
      }}
    
    
    if (length(yrs[yrs >= 2015 & yrs <= 2016]) > 0) {
      
      if ("households with broadband" %in% vars) {
        
        variables["with_broadband"] <- "DP02_0152P"}
      
      
      if ("old-age dependency ratio" %in% vars) {
        
        variables["age_ratio"] <- "S0101_C01_033"}
      
      
      for (x in yrs[yrs >= 2015 & yrs <= 2016]) {
        
        census_data_1 <- tidycensus::get_acs(geography = geog, variables = variables, year = x) |>
          dplyr::mutate(year = x)
        
        census_data <- rbind(census_data, census_data_1)
        
      }}
    
    
    if (length(yrs[yrs >= 2013 & yrs <= 2014]) > 0) {
      
      if ("households with broadband" %in% vars) {
        
        variables["with_broadband"] <- "DP02_0152P"}
      
      
      if ("old-age dependency ratio" %in% vars) {
        
        variables["age_ratio"] <- "S0101_C01_033"}
      
      
      if ("households without vehicle" %in% vars) {
        
        variables["without_vehicle"] <- "DP04_0057P"}
      
      
      for (x in yrs[yrs >= 2013 & yrs <= 2014]) {
        
        census_data_1 <- tidycensus::get_acs(geography = geog, variables = variables, year = x) |>
          dplyr::mutate(year = x)
        
        census_data <- rbind(census_data, census_data_1)
        
      }}
    
    
    if (length(yrs[yrs >= 2010 & yrs <= 2012]) > 0) {
      
      if ("households with broadband" %in% vars) {
        
        variables <- variables[!variables == variables["with_broadband"]]}
      
      
      if ("old-age dependency ratio" %in% vars) {
        
        variables["age_ratio"] <- "S0101_C01_033"}
      
      
      if ("households without vehicle" %in% vars) {
        
        variables["without_vehicle"] <- "DP04_0057P"}
      
      
      for (x in yrs[yrs >= 2010 & yrs <= 2012]) {
        
        census_data_1 <- tidycensus::get_acs(geography = geog, variables = variables, year = x) |>
          dplyr::mutate(year = x)
        
        census_data <- rbind(census_data, census_data_1)
        
      }
      
      # if with_broadband was taken out but not added back in, it will make the 2009 code section below act weird
      variables["with_broadband"] <- "placeholder"
      
      }
    
    if (length(yrs[yrs == 2009]) > 0) {
      
      if ("households with broadband" %in% vars) {
        
        variables <- variables[!variables == variables["with_broadband"]]}
      
      
      if ("old-age dependency ratio" %in% vars) {
        
        variables <- variables[!variables == variables["age_ratio"]]}
      
      
      if ("households without vehicle" %in% vars) {
        
        variables["without_vehicle"] <- "DP04_0057P"}
      
      
      if ("Gini index" %in% vars) {
        
        variables <- variables[!variables == variables["gini"]]}
      
      
      for (x in yrs[yrs == 2009]) {
        
        census_data_1 <- tidycensus::get_acs(geography = geog, variables = variables, year = x) |>
          dplyr::mutate(year = x)
        
        census_data <- rbind(census_data, census_data_1)
        
      }}
    
    return(census_data)
}

census_retrieval_cleaning <- function(vars, yrs, geog) {
  
  census_data <- census_retrieval_vars(vars, yrs, geog)
  
  
  if ("poverty" %in% vars) {
    
    poverty_split <- split(census_data, census_data$variable == "below_100" | census_data$variable == "at_above_150" | census_data$variable == "poverty_total")
    
    census_data_poverty <- poverty_split$`TRUE`
    
    # computes percent of population whose income in the past 12 months was below 100% and below 150% of the poverty level
    census_data_poverty <- census_data_poverty |>
      tidyr::pivot_wider(names_from = variable, values_from = c(estimate, moe), names_sep = "_") |>
      dplyr::mutate(
        below_150_percent_poverty_level.estimate = round(((estimate_poverty_total - estimate_at_above_150)/estimate_poverty_total)*100, 2),
        below_100_percent_poverty_level.estimate = round((estimate_below_100/estimate_poverty_total)*100, 2),
        below_150_percent_poverty_level.moe = round((tidycensus::moe_prop(num = (estimate_poverty_total - estimate_at_above_150), denom = estimate_poverty_total, moe_num = tidycensus::moe_sum(moe = c(moe_poverty_total, moe_at_above_150), estimate = c(estimate_poverty_total, estimate_at_above_150)), moe_denom = moe_poverty_total))*100, 2),
        below_100_percent_poverty_level.moe = round((tidycensus::moe_prop(num = estimate_below_100, denom = estimate_poverty_total, moe_num = moe_below_100, moe_denom = moe_poverty_total))*100, 2)
        ) |>
      dplyr::select(c(GEOID, NAME, year,  below_150_percent_poverty_level.estimate, below_100_percent_poverty_level.estimate, below_150_percent_poverty_level.moe, below_100_percent_poverty_level.moe)) |>
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
      housing_vacancy.estimate = round((estimate_vacant/estimate_vacancy_total)*100, 2),
      housing_vacancy.moe = round((tidycensus::moe_prop(num = estimate_vacant, denom = estimate_vacancy_total, moe_num = moe_vacant, moe_denom = moe_vacancy_total))*100, 2)
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
      homeowners.estimate = round((estimate_owner_occupied/estimate_homeownership_total)*100, 2),
      homeowners.moe = round((tidycensus::moe_prop(num = estimate_owner_occupied, denom = estimate_homeownership_total, moe_num = moe_owner_occupied, moe_denom = moe_homeownership_total))*100, 2)
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
      single_parent.estimate = round(((estimate_male_1 + estimate_female_1 + estimate_male_2 + estimate_female_2)/(estimate_male_1 + estimate_female_1 + estimate_male_2 + estimate_female_2 + estimate_married_1 + estimate_married_2))*100, 2),
      single_parent.moe = round((tidycensus::moe_prop(num = (estimate_male_1 + estimate_female_1 + estimate_male_2 + estimate_female_2), denom = (estimate_male_1 + estimate_female_1 + estimate_male_2 + estimate_female_2 + estimate_married_1 + estimate_married_2), moe_num = tidycensus::moe_sum(moe = c(moe_male_1, moe_female_1, moe_male_2, moe_female_2), estimate = c(estimate_male_1, estimate_female_1, estimate_male_2, estimate_female_2)), moe_denom = tidycensus::moe_sum(moe = c(moe_male_1, moe_female_1, moe_male_2, moe_female_2, moe_married_1, moe_married_2), estimate = c(estimate_male_1, estimate_female_1, estimate_male_2, estimate_female_2, estimate_married_1, estimate_married_2))))*100, 2)
    ) |>
    dplyr::select(c(GEOID, NAME, year,  single_parent.estimate, single_parent.moe)) |>
    tidyr::pivot_longer(cols = single_parent.estimate:single_parent.moe, names_to = c("variable", "estimate_moe"), values_to = "percent", names_sep = "[.]") |>
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
        public_transport_commutes.estimate = round((estimate_transit/(estimate_drove_alone + estimate_carpooled + estimate_transit))*100, 2),
        public_transport_commutes.moe = round((tidycensus::moe_prop(num = estimate_transit, denom = (estimate_drove_alone + estimate_carpooled + estimate_transit), moe_num = moe_transit, moe_denom = tidycensus::moe_sum(moe = c(moe_drove_alone, moe_carpooled, moe_transit), estimate = c(estimate_drove_alone, estimate_carpooled, estimate_transit))))*100, 2)
        ) |>
      dplyr::select(c(GEOID, NAME, year, public_transport_commutes.estimate, public_transport_commutes.moe)) |>
      tidyr::pivot_longer(cols = public_transport_commutes.estimate:public_transport_commutes.moe, names_to = c("variable", "estimate_moe"), values_to = "percent", names_sep = "[.]") |>
      tidyr::pivot_wider(names_from = estimate_moe, values_from = percent)
    
    census_data <- rbind(public_transport_commutes_split$`FALSE`, census_data_public_transport_commutes)
  }
    
  
    census_data <- census_data |>
      dplyr::mutate(type = dplyr::case_when(variable == "unemployment" ~ "percent (of civilian labor force age 16+)",
                                            variable == "high_skill_employment" ~ "percent (of civilian employed population 16+)",
                                            variable == "median_family_income" ~ "value (dollars)",
                                            variable == "median_home_value" ~ "value (dollars)",
                                            variable == "mean_commute_time" ~ "value (minutes)",
                                            variable == "median_gross_rent" ~ "value (dollars)",
                                            variable == "below_150_percent_poverty_level" ~ "percent (of population)",
                                            variable == "below_100_percent_poverty_level" ~ "percent (of population)",
                                            variable == "age_ratio" ~ "ratio (of people age 65+ per 100 people age 18-64)",
                                            variable == "housing_vacancy" ~ "percent (of housing units)",
                                            variable == "gini" ~ "summary measure (from 0, perfect income equality, to 1, perfect income inequality)",
                                            variable == "without_vehicle" ~ "percent (of households)",
                                            variable == "with_broadband" ~ "percent (of households)",
                                            variable == "homeowners" ~ "percent (of population)",
                                            variable == "single_parent" ~ "percent (of families with children under 18)",
                                            variable == "public_transport_commutes" ~ "percent (of motorized commutes to work)"
                                            ))
  
  census_data <- census_data |>
    dplyr::select(!NAME)
  
  return(census_data)
}


bea_retrieval <- function(yrs, geog) {
  
  if (geog == "county") {
    
    # U.S. county GDP over time
    
    userSpecList <- list('UserID' = Sys.getenv("bea_key_value"),
                         'Method' = 'GetData',
                         'datasetname' = 'Regional',
                         'TableName' = 'CAGDP1',
                         'LineCode' = 1,
                         'GeoFips' = 'COUNTY',
                         'Year' = 'ALL')
    
    county_GDP <- bea.R::beaGet(userSpecList, asTable = TRUE)|>
      tidyr::pivot_longer(cols = DataValue_2001:DataValue_2023, names_to = "year", values_to = "gdp") |>
      dplyr::mutate(
        year = as.numeric(substr(year, 11, 14)),
        gdp = gdp*1000
        ) |>
      dplyr::select(GeoFips, gdp, year)
    # resulting gdp variable is in chained 2017 dollars
    
    
    # U.S. county population over time
    
    userSpecList <- list('UserID' = Sys.getenv("bea_key_value"),
                         'Method' = 'GetData',
                         'datasetname' = 'Regional',
                         'TableName' = 'CAINC1',
                         'LineCode' = 2,
                         'GeoFips' = 'COUNTY',
                         'Year' = '2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023')
    
    county_population <- bea.R::beaGet(userSpecList, asTable = TRUE) |>
      tidyr::pivot_longer(cols = DataValue_2001:DataValue_2023, names_to = "year", values_to = "population") |>
      dplyr::mutate(year = as.numeric(substr(year, 11, 14))) |>
      dplyr::select(GeoFips, population, year, GeoName)
    
    
    # U.S. county GDP per capita over time
    
    bea_data <- dplyr::inner_join(county_GDP, county_population, by = c("GeoFips", "year")) |>
      dplyr::mutate(estimate = gdp/population) |>
      dplyr::mutate(
        moe = NA,
        variable = "gdp_per_capita"
        ) |>
      dplyr::rename(GEOID = GeoFips) |>
      dplyr::select(c(GEOID, variable, estimate, moe, year)) |>
      dplyr::filter(year %in% yrs) |>
      dplyr::mutate(type = "ratio (per capita)")}
  
  if (geog == "cbsa") {
    
    # U.S. MSA GDP over time
    
    userSpecList <- list('UserID' = Sys.getenv("bea_key_value"),
                         'Method' = 'GetData',
                         'datasetname' = 'Regional',
                         'TableName' = 'CAGDP1',
                         'LineCode' = 1,
                         'GeoFips' = 'MSA',
                         'Year' = 'ALL')
    
    msa_GDP <- bea.R::beaGet(userSpecList, asTable = TRUE) |>
      tidyr::pivot_longer(cols = DataValue_2001:DataValue_2023, names_to = "year", values_to = "gdp") |>
      dplyr::mutate(
        year = as.numeric(substr(year, 11, 14)),
        gdp = gdp*1000
        ) |>
      dplyr::select(GeoFips, gdp, year)
    # resulting gdp variable is in chained 2017 dollars
    
    
    # U.S. MSA population over time
    
    userSpecList <- list('UserID' = Sys.getenv("bea_key_value"),
                         'Method' = 'GetData',
                         'datasetname' = 'Regional',
                         'TableName' = 'CAINC1',
                         'LineCode' = 2,
                         'GeoFips' = 'MSA',
                         'Year' = '2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023')
    
    msa_population <- bea.R::beaGet(userSpecList, asTable = TRUE) |>
      tidyr::pivot_longer(cols = DataValue_2001:DataValue_2023, names_to = "year", values_to = "population") |>
      dplyr::mutate(year = as.numeric(substr(year, 11, 14))) |>
      dplyr::select(GeoFips, population, year, GeoName)
    
    
    # U.S. MSA GDP per capita over time
    
    bea_data <- dplyr::inner_join(msa_GDP, msa_population, by = c("GeoFips", "year")) |>
      dplyr::filter(GeoName != "United States (Metropolitan Portion)") |>
      dplyr::mutate(
        estimate = gdp/population,
        moe = NA,
        variable = "gdp_per_capita"
        ) |>
      dplyr::rename(GEOID = GeoFips) |>
      dplyr::select(c(GEOID, variable, estimate, moe, year)) |>
      dplyr::filter(year %in% yrs) |>
      dplyr::mutate(type = "ratio (per capita)")}
  
  return(bea_data)}


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
        
      }}
    
    if ("median AQI" %in% vars & "unhealthy air days" %in% vars) {
      
      aqi_data <- aqi_data |>
        dplyr::mutate(
          unhealthy_air_days = round((Unhealthy.Days/Days.with.AQI)*100, 2),
          county_state = paste0(County, ", ", State),
          moe = NA
          ) |>
        dplyr::select(!c(County, State, Unhealthy.Days, Days.with.AQI)) |>
        dplyr::rename(median_aqi = Median.AQI, year = Year) |>
        tidyr::pivot_longer(cols = c(median_aqi, unhealthy_air_days), names_to = "variable", values_to = "estimate")
        
      
    } else if (vars == "median AQI") {
      
    aqi_data <- aqi_data |>
      dplyr::rename(year = Year, estimate = Median.AQI) |>
      dplyr::mutate(
        moe = NA, 
        variable = "median_aqi", 
        county_state = paste0(County, ", ", State)
        ) |>
      dplyr::select(!c(County, State, Unhealthy.Days, Days.with.AQI))
    
    } else {
      
      aqi_data <- aqi_data |>
        dplyr::mutate(unhealthy_air_days = round((Unhealthy.Days/Days.with.AQI)*100, 2)) |>
        dplyr::rename(year = Year, estimate = unhealthy_air_days) |>
        dplyr::mutate(moe = NA, variable = "unhealthy_air_days", county_state = paste0(County, ", ", State)) |>
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
      dplyr::select(c(CBSA, CBSA.Code, Year, Median.AQI, Days.with.AQI, Unhealthy.Days))
    
    unlink(temp)
    
    unlink(temp2)
    
    if (length(yrs) > 1) {
      
      for (x in yrs[-1]) {
        
        temp <- tempfile()
        
        utils::download.file(paste0("https://aqs.epa.gov/aqsweb/airdata/annual_aqi_by_cbsa_", as.character(x), ".zip"), temp)
        
        temp2 <- tempfile()
        
        aqi_data_1 <- utils::read.csv(utils::unzip(temp, exdir = temp2)) |>
          dplyr::select(c(CBSA, CBSA.Code, Year, Median.AQI, Days.with.AQI, Unhealthy.Days))
        
        unlink(temp)
        
        unlink(temp2)
        
        aqi_data <- rbind(aqi_data, aqi_data_1)
        
      }}
    
    if ("median AQI" %in% vars & "unhealthy air days" %in% vars) {
      
      aqi_data <- aqi_data |>
        dplyr::mutate(unhealthy_air_days = round((Unhealthy.Days/Days.with.AQI)*100, 2), moe = NA) |>
        dplyr::rename(median_aqi = Median.AQI, year = Year, GEOID = CBSA.Code) |>
        tidyr::pivot_longer(cols = c(median_aqi, unhealthy_air_days), names_to = "variable", values_to = "estimate")
      
      
    } else if (vars == "median AQI") {
      
      aqi_data <- aqi_data |>
        dplyr::rename(year = Year, estimate = Median.AQI, GEOID = CBSA.Code) |>
        dplyr::mutate(moe = NA, variable = "median_aqi") |>
        dplyr::select(!c(Unhealthy.Days, Days.with.AQI))
      
    } else {
      
      aqi_data <- aqi_data |>
        dplyr::mutate(unhealthy_air_days = round((Unhealthy.Days/Days.with.AQI)*100, 2)) |>
        dplyr::rename(year = Year, estimate = unhealthy_air_days, GEOID = CBSA.Code) |>
        dplyr::mutate(moe = NA, variable = "unhealthy_air_days") |>
        dplyr::select(!c(Unhealthy.Days, Days.with.AQI, Median.AQI))
      
    }
  }
  
  aqi_data <- aqi_data |>
    dplyr::mutate(type = dplyr::case_when(variable == "median_aqi" ~ "value (index ranging from 0, excellent, to 500, extremely hazardous)",
                                   variable == "unhealthy_air_days" ~ "percent (of days with data available)"))
  
  return(aqi_data)}


#' Retrieves and combines data from various sources
#' @description Retrieves data from sources such as the Census Bureau, Bureau of
#' Economic Analysis, and Environmental Protection Agency. User specifies 
#' variables, timeframe, and geographic region of data to retrieve. Function 
#' will return a dataframe combining all variables requested.
#' @param vars Supply a vector of variables to request. Available variables 
#' include: `"unemployment"`, `"high-skill employment"`, 
#' `"median family income"`, `"poverty"`, `"old-age dependency ratio"`, 
#' `"housing vacancy"`, `"Gini index"`, `"median home value"`, 
#' `"median gross rent"`, `"households without vehicle"`, 
#' `"households with broadband"`, `"homeownership"`, `"single-parent families"`,
#'  `"public transport commutes"`, `"mean commute time"`, 
#'  `"GDP per capita"`, `"median AQI"`, `"unhealthy air days"`. Some variables 
#' cannot be retrieved without first registering for an API key for the 
#' corresponding data source and supplying the API key in R (see notes below).
#' * Variables requiring a U.S. Census Bureau API key: `"unemployment"`, 
#' `"high-skill employment"`, `"median family income"`, `"poverty"`, 
#' `"old-age dependency ratio"`, `"housing vacancy"`, `"Gini index"`, 
#' `"median home value"`, `"median gross rent"`, `"households without vehicle"`,
#'  `"households with broadband"`, `"homeownership"`, 
#'  `"single-parent families"`, `"public transport commutes"`, 
#'  `"mean commute time"`. To retrieve these variables, first establish a Census
#'   Bureau API key by following the instructions for [census_key()]. Retrieval
#'    will be performed using the tidycensus package.
#' * Variables requiring a U.S. Bureau of Economic Analysis API key: 
#' `"GDP per capita"`. To retrieve these variables, first establish a Bureau of
#' Economic Analysis API key by following the instructions for [bea_key()]. 
#' Retrieval will be performed using the bea.R package.
#' @param yrs Supply a year or range of years. If no value is supplied, will 
#' default to the largest possible range of years across which data is available
#'  on at least one of the selected variables.
#' @param geog Specify either `"county"` or `"cbsa"`.
#' @param state Optionally (ONLY if `geog` is set to `"county"`), supply a vector
#'  of state abbreviations to filter to. By default, all possible values will
#'   be returned by the function.
#' @param geoselect Optionally, supply a vector of county or CBSA (depending on
#'  whether `geog` is set to `"county"` or `"cbsa"`) GEOIDs to filter to. By
#'  default, all possible values will be returned by the function. To determine
#'  the corresponding GEOID for a particular county or CBSA, use 
#'  [county_geoids()] or [cbsa_geoids()], respectively.
#' @export

retrieve_data <- function(vars, yrs = 1980:2024, geog, state = NULL, geoselect = NULL) {
  
  # creating an empty dataframe with column names for use when rbind-ing datasets together
  empty_df <- data.frame(matrix(nrow = 0, ncol = 6))
  colnames(empty_df) = c("GEOID", "variable", "estimate", "moe", "year", "type")
  
  
  # filtering based on the availability of ACS data for counties and CBSAs (which seems to always be 5-year)
  census_yrs <- yrs[yrs >= 2009 & yrs <= 2023]
  
  if (all(("unemployment" %in% vars | "high-skill employment" %in% vars | "median family income" %in% vars | "poverty" %in% vars | "old-age dependency ratio" %in% vars | "housing vacancy" %in% vars | "Gini index" %in% vars | "median home value" %in% vars | "median gross rent" %in% vars | "households without vehicle" %in% vars | "households with broadband" %in% vars | "homeownership" %in% vars | "single-parent families" %in% vars | "public transport commutes" %in% vars | "mean commute time" %in% vars), length(census_yrs) != 0, (!identical(vars, "old-age dependency ratio") | !identical(yrs, 2009)), (!identical(vars, "Gini index") | !identical(yrs, 2009)), (!identical(vars, c("old-age dependency ratio", "Gini index")) | !identical(yrs, 2009)), (!identical(vars, "households with broadband") | sum(match(yrs, 2013:2024), na.rm = TRUE) > 0))) {
    
    census_vars <- vars[vars %in% c("unemployment", "high-skill employment", "median family income", "poverty", "old-age dependency ratio", "housing vacancy", "Gini index", "median home value", "median gross rent", "households without vehicle", "households with broadband", "homeownership", "single-parent families", "public transport commutes", "mean commute time")]
    
    census_data <- census_retrieval_cleaning(vars = census_vars, yrs = census_yrs, geog = geog)
  } else {census_data <- empty_df}
  
  
  bea_yrs <- yrs[yrs >= 2001 & yrs <= 2023]
  
  if ("GDP per capita" %in% vars & length(bea_yrs) != 0) {
    
    bea_data <- bea_retrieval(yrs = bea_yrs, geog = geog)
  } else {bea_data <- empty_df}
  
  
  aqi_yrs <- yrs[yrs >= 1980 & yrs <= 2024]
  
  if (("median AQI" %in% vars | "unhealthy air days" %in% vars) & length(aqi_yrs) != 0) {
    
    aqi_vars <- vars[vars %in% c("median AQI", "unhealthy air days")]
    
    aqi_data <- aqi_retrieval(vars = aqi_vars, yrs = aqi_yrs, geog = geog)
  } else {aqi_data <- empty_df}
  
  data_all <- rbind(census_data, bea_data, aqi_data)
  
  
  # retrieving county/CBSA names to join with data
  tidycensus::census_api_key(Sys.getenv("census_key_value"))
  if (geog == "county") {
    geog_county <- tidycensus::fips_codes |>
      dplyr::mutate(
        GEOID = paste0(state_code, county_code),
        county = paste0(county, ", ", state_name)
        ) |>
      dplyr::rename(state_abbr = state) |>
      dplyr::select(c(GEOID, county, state_abbr))
    
    data_all <- dplyr::inner_join(data_all, geog_county, by = dplyr::join_by(GEOID))
    
    data_all <- data_all[, c("GEOID", "county", "state_abbr", "year", "variable", "type", "estimate", "moe")]
    
    if (!is.null(state)) {
      
      data_all <- data_all |>
        dplyr::filter(state_abbr %in% state)
    }}
  
  if (geog == "cbsa") {
    geog_cbsa <- tidycensus::get_acs(geography = "cbsa", variables = "DP03_0005P", year = 2023) |>
      dplyr::select(c(GEOID, NAME)) |>
      dplyr::rename(cbsa = NAME) |>
      dplyr::distinct()
    
    data_all <- dplyr::inner_join(data_all, geog_cbsa, by = dplyr::join_by(GEOID))
    
    data_all <- data_all[, c("GEOID", "cbsa", "variable", "type", "estimate", "moe")]
    }
  
  if (!is.null(geoselect)) {
    
    data_all <- data_all |>
      dplyr::filter(GEOID %in% geoselect)
  }
  
  data_all <- data_all |>
    dplyr::arrange(GEOID, year)
  
  return(data_all)
}