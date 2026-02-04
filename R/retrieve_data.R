# possible vars values include unemployment, high-skill employment, median family income, poverty, and housing vacancy
# seems like it always retrieves the ACS 5-year estimates for county and for CBSA?

census_retrieval <- function(vars, yrs, geog) {

  tidycensus::census_api_key(census_key_value)
  
  if ("unemployment" %in% vars | "high-skill employment" %in% vars | "median family income" %in% vars) {
    
    variables <- c()
    
    if ("unemployment" %in% vars) {
      
      # retrieves percent of civilian labor force aged 16+ that is unemployed
      variables <- c(variables, "DP03_0005P") }
    
    if ("high-skill employment" %in% vars) {
      
      # retrieves percent of civilian employed population aged 16+ in management, professional, and related occupations
      variables <- c(variables, "DP03_0027P")}
    
    if ("median family income" %in% vars) {
      
      # retrieves median family income
      variables <- c(variables, "DP03_0087E")}
    
    census_data <- tidycensus::get_acs(geography = geog, 
                           variables = variables, 
                           year = yrs[1]) |>
      dplyr::mutate(year = yrs[1])
    
    if (length(yrs) > 1) {
      
      for (x in yrs[-1]) {
        
        census_data_1 <- tidycensus::get_acs(geography = geog, variables = variables, year = x) |>
          dplyr::mutate(year = x)
        
        census_data <- rbind(census_data, census_data_1)
        
      }}
    
    census_data <- census_data |>
      dplyr::mutate(variable = dplyr::case_when(variable == "DP03_0005P" ~ "unemployment",
                                  variable == "DP03_0027P" ~ "high-skill employment",
                                  variable == "DP03_0087" ~ "median family income"))
    
  }
  
  if ("poverty" %in% vars) {
    
    census_data_poverty <- tidycensus::get_acs(geography = geog, 
                                   variables = c(below_100 = "B06012_002", at_above_150 = "B06012_004", total = "B06012_001"), year = yrs[1]) |>
      dplyr::mutate(year = yrs[1])
    
    if (length(yrs) > 1) {
      
      for (x in yrs[-1]) {
        
        census_data_poverty_1 <- tidycensus::get_acs(geography = geog, variables = c(below_100 = "B06012_002", at_above_150 = "B06012_004", total = "B06012_001"), year = x) |>
          dplyr::mutate(year = x)
        
        census_data_poverty <- rbind(census_data_poverty, census_data_poverty_1)
        
      }}
    
    # computes percent of population whose income in the past 12 months was below 100% and below 150% of the poverty level
    # check that I calculated the margin of error percentages correctly -> I might need to instead use the moe_ratio and similar functions from tidycensus
    census_data_poverty <- census_data_poverty |>
      tidyr::pivot_wider(names_from = variable, values_from = c(estimate, moe), names_sep = "_") |>
      dplyr::mutate(below_150_percent_poverty_level.estimate = round(((estimate_total - estimate_at_above_150)/estimate_total)*100, 2)) |>
      dplyr::mutate(below_100_percent_poverty_level.estimate = round((estimate_below_100/estimate_total)*100, 2)) |>
      dplyr::mutate(below_150_percent_poverty_level.moe = round((moe_at_above_150/estimate_total)*100, 2)) |>
      dplyr::mutate(below_100_percent_poverty_level.moe = round((moe_below_100/estimate_total)*100, 2)) |>
      dplyr::select(c(GEOID, NAME, year,  below_150_percent_poverty_level.estimate, below_100_percent_poverty_level.estimate, below_150_percent_poverty_level.moe, below_100_percent_poverty_level.moe)) |>
      tidyr::pivot_longer(cols = below_150_percent_poverty_level.estimate:below_100_percent_poverty_level.moe, names_to = c("variable", "estimate_moe"), values_to = "percent", names_sep = "[.]") |>
      tidyr::pivot_wider(names_from = estimate_moe, values_from = percent)
    
    if (identical(vars, c("poverty"))) {census_data <- census_data_poverty} else {
      census_data <- rbind(census_data, census_data_poverty)}
    
  }
  
  if ("old-age dependency ratio" %in% vars) {
    
    yrs_early <- yrs[yrs >= 2010 & yrs <= 2016]
    
    if (length(yrs_early) >= 1) {
    
    census_data_age_ratio_early <- tidycensus::get_acs(geography = geog, 
                                               variables = c(age_ratio = "S0101_C01_033"), year = yrs_early[1]) |>
      dplyr::mutate(year = yrs_early[1])}
    
    if (length(yrs_early) > 1) {
      
      for (x in yrs_early[-1]) {
        
        census_data_age_ratio_early_1 <- tidycensus::get_acs(geography = geog, variables = c(age_ratio = "S0101_C01_033"), year = x) |>
          dplyr::mutate(year = x)
        
        census_data_age_ratio_early <- rbind(census_data_age_ratio_early, census_data_age_ratio_early_1)
        
      }}
      
      yrs_late <- yrs[yrs >= 2017 & yrs <= 2024]
      
      if(length(yrs_late) >= 1) {
      
      census_data_age_ratio_late <- tidycensus::get_acs(geography = geog, 
                                                         variables = c(age_ratio = "S0101_C01_035"), year = yrs_late[1]) |>
        dplyr::mutate(year = yrs_late[1])}
      
      if (length(yrs_late) > 1) {
        
        for (x in yrs_late[-1]) {
          
          census_data_age_ratio_late_1 <- tidycensus::get_acs(geography = geog, variables = c(age_ratio = "S0101_C01_035"), year = x) |>
            dplyr::mutate(year = x)
          
          census_data_age_ratio_late <- rbind(census_data_age_ratio_late, census_data_age_ratio_late_1)
          
        }}
      
      if (length(yrs_early > 0) & length(yrs_late > 0)) {census_data_age_ratio <- rbind(census_data_age_ratio_early, census_data_age_ratio_late)}
      if (length(yrs_early > 0) & length(yrs_late == 0)) {census_data_age_ratio <- census_data_age_ratio_early}
      if (length(yrs_early == 0) & length(yrs_late > 0)) {census_data_age_ratio <- census_data_age_ratio_late}
    
      
    if (identical(vars, c("old-age dependency ratio"))) {census_data <- census_data_age_ratio} else {
      census_data <- rbind(census_data, census_data_age_ratio)}
  }
  
  
  if ("housing vacancy" %in% vars) {
    
    census_data_vacancy <- tidycensus::get_acs(geography = geog, variables = c(vacant = "B25002_003", total = "B25002_001"), year = yrs[1]) |>
      dplyr::mutate(year = yrs[1])
    
    if (length(yrs) > 1) {
      
      for (x in yrs[-1]) {
        
        census_data_vacancy_1 <- tidycensus::get_acs(geography = geog, variables = c(vacant = "B25002_003", total = "B25002_001"), year = x) |>
          dplyr::mutate(year = x)
        
        census_data_vacancy <- rbind(census_data_vacancy, census_data_vacancy_1)
        
      }}
    
    # computes percent of housing units vacant
    # prob make sure there isn't an easier way to retrieve this
    # check that I calculated the margin of error percentages correctly -> I might need to instead use the moe_ratio and similar functions from tidycensus
    census_data_vacancy <- census_data_vacancy |>
      tidyr::pivot_wider(names_from = variable, values_from = c(estimate, moe), names_sep = "_") |>
      dplyr::mutate(housing_vacancy.estimate = round((estimate_vacant/estimate_total)*100, 2)) |>
      dplyr::mutate(housing_vacancy.moe = round((moe_vacant/estimate_total)*100, 2)) |>
      dplyr::select(c(GEOID, NAME, year, housing_vacancy.estimate, housing_vacancy.moe)) |>
      tidyr::pivot_longer(cols = housing_vacancy.estimate:housing_vacancy.moe, names_to = c("variable", "estimate_moe"), values_to = "percent", names_sep = "[.]") |>
      tidyr::pivot_wider(names_from = estimate_moe, values_from = percent)
    
    if (identical(vars, c("housing vacancy"))) {census_data <- census_data_vacancy} else {
      census_data <- rbind(census_data, census_data_vacancy)}
    
  }
  
  census_data <- census_data |>
    dplyr::select(!NAME)
  
  return(census_data)
}


bea_retrieval <- function(yrs, geog) {
  
  if (geog == "county") {
    
    # U.S. county GDP over time
    
    userSpecList <- list('UserID' = bea_key_value,
                         'Method' = 'GetData',
                         'datasetname' = 'Regional',
                         'TableName' = 'CAGDP1',
                         'LineCode' = 1,
                         'GeoFips' = 'COUNTY',
                         'Year' = 'ALL')
    
    county_GDP <- bea.R::beaGet(userSpecList, asTable = TRUE)|>
      tidyr::pivot_longer(cols = DataValue_2001:DataValue_2023, names_to = "year", values_to = "gdp") |>
      dplyr::mutate(year = as.numeric(substr(year, 11, 14))) |>
      dplyr::mutate(gdp = gdp*1000) |>
      dplyr::select(GeoFips, gdp, year)
    # resulting gdp variable is in chained 2017 dollars
    
    
    # U.S. county population over time
    
    userSpecList <- list('UserID' = bea_key_value,
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
      dplyr::mutate(moe = NA) |>
      dplyr::mutate(variable = "GDP per capita") |>
      dplyr::rename(GEOID = GeoFips) |>
      dplyr::select(c(GEOID, variable, estimate, moe, year)) |>
      dplyr::filter(year %in% yrs)}
  
  if (geog == "cbsa") {
    
    # U.S. MSA GDP over time
    
    userSpecList <- list('UserID' = bea_key_value,
                         'Method' = 'GetData',
                         'datasetname' = 'Regional',
                         'TableName' = 'CAGDP1',
                         'LineCode' = 1,
                         'GeoFips' = 'MSA',
                         'Year' = 'ALL')
    
    msa_GDP <- bea.R::beaGet(userSpecList, asTable = TRUE) |>
      tidyr::pivot_longer(cols = DataValue_2001:DataValue_2023, names_to = "year", values_to = "gdp") |>
      dplyr::mutate(year = as.numeric(substr(year, 11, 14))) |>
      dplyr::mutate(gdp = gdp*1000) |>
      dplyr::select(GeoFips, gdp, year)
    # resulting gdp variable is in chained 2017 dollars
    
    
    # U.S. MSA population over time
    
    userSpecList <- list('UserID' = bea_key_value,
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
      dplyr::mutate(estimate = gdp/population) |>
      dplyr::mutate(moe = NA) |>
      dplyr::mutate(variable = "GDP per capita") |>
      dplyr::rename(GEOID = GeoFips) |>
      dplyr::select(c(GEOID, variable, estimate, moe, year)) |>
      dplyr::filter(year %in% yrs)}
  
  return(bea_data)}


# should I be concerned that some counties don't seem to have many days with AQI? (see column in original datasets)
# the value of geog can be either "county" or "cbsa"

aqi_retrieval <- function(yrs, geog) {
  
  if (geog == "county") {
    
    temp <- tempfile()
    
    download.file(paste0("https://aqs.epa.gov/aqsweb/airdata/annual_aqi_by_county_", as.character(yrs[1]), ".zip"), temp)
    
    temp2 <- tempfile()
    
    aqi_data <- read.csv(unzip(temp, exdir = temp2)) |>
      dplyr::select(c(State, County, Year, Median.AQI))
    
    unlink(temp)
    
    unlink(temp2)
    
    if (length(yrs) > 1) {
      
      for (x in yrs[-1]) {
        
        temp <- tempfile()
        
        download.file(paste0("https://aqs.epa.gov/aqsweb/airdata/annual_aqi_by_county_", as.character(x), ".zip"), temp)
        
        temp2 <- tempfile()
        
        aqi_data_1 <- read.csv(unzip(temp, exdir = temp2)) |>
          dplyr::select(c(State, County, Year, Median.AQI))
        
        unlink(temp)
        
        unlink(temp2)
        
        aqi_data <- rbind(aqi_data, aqi_data_1)
        
      }}
    
    aqi_data <- aqi_data |>
      dplyr::rename(year = Year, estimate = Median.AQI) |>
      dplyr::mutate(moe = NA) |>
      dplyr::mutate(variable = "median AQI") |>
      dplyr::mutate(county_state = paste0(County, ", ", State)) |>
      dplyr::select(!c(County, State))
    
    temp <- tempfile()
    
    download.file("https://aqs.epa.gov/aqsweb/airdata/aqs_monitors.zip", temp)
    
    temp2 <- tempfile()
    
    # matches the GEOIDs in this spreadsheet, also from the EPA, to counties in the counties AQI dataset, matching on county and state name
    aqs_monitors <- read.csv(unzip(temp, exdir = temp2)) |>
      dplyr::select(State.Code, County.Code, State.Name, County.Name) |>
      dplyr::distinct() |>
      dplyr::mutate(County.Code = sprintf("%03d", County.Code)) |>
      dplyr::mutate(GEOID = paste0(State.Code, County.Code)) |>
      dplyr::mutate(county_state = paste0(County.Name, ", ", State.Name)) |>
      dplyr::select(c(GEOID, county_state))
    
    unlink(temp)
    
    unlink(temp2)
    
    aqi_data <- dplyr::inner_join(aqi_data, aqs_monitors, by = dplyr::join_by(county_state)) |>
      dplyr::select(!county_state)
    
  } 
  
  if (geog == "cbsa") {
    
    temp <- tempfile()
    
    download.file(paste0("https://aqs.epa.gov/aqsweb/airdata/annual_aqi_by_cbsa_", as.character(yrs[1]), ".zip"), temp)
    
    temp2 <- tempfile()
    
    aqi_data <- read.csv(unzip(temp, exdir = temp2)) |>
      dplyr::select(c(CBSA, CBSA.Code, Year, Median.AQI))
    
    unlink(temp)
    
    unlink(temp2)
    
    if (length(yrs) > 1) {
      
      for (x in yrs[-1]) {
        
        temp <- tempfile()
        
        download.file(paste0("https://aqs.epa.gov/aqsweb/airdata/annual_aqi_by_cbsa_", as.character(x), ".zip"), temp)
        
        temp2 <- tempfile()
        
        aqi_data_1 <- read.csv(unzip(temp, exdir = temp2)) |>
          dplyr::select(c(CBSA, CBSA.Code, Year, Median.AQI))
        
        unlink(temp)
        
        unlink(temp2)
        
        aqi_data <- rbind(aqi_data, aqi_data_1)
        
      }}
    
    aqi_data <- aqi_data |>
      dplyr::rename(GEOID = CBSA.Code, year = Year, estimate = Median.AQI) |>
      dplyr::mutate(moe = NA) |>
      dplyr::mutate(variable = "median AQI")
  }
  
  return(aqi_data)}



#' Retrieves and combines data from various sources
#' @description Retrieves data from sources such as the Census Bureau, Bureau of Economic Analysis, and Environmental Protection Agency. User specifies variables, timeframe, and geographic region of data to retrieve. Function will return a dataframe combining all variables requested.
#' @param vars Supply a vector of variables to request. Available variables include: unemployment, high-skill employment, median family income, poverty, old-age dependency ratio, housing vacancy, GDP per capita, median AQI. Some variables cannot be retrieved without first registering for an API key for the corresponding data source and supplying the API key in R (see notes below).
#' * Variables requiring a U.S. Census Bureau API key: unemployment, high-skill employment, median family income, poverty, old-age dependency ratio, housing vacancy. To retrieve these variables, first establish a Census Bureau API key by following the instructions for [census_key()].
#' * Variables requiring a U.S. Bureau of Economic Analysis API key: GDP per capita. To retrieve these variables, first establish a Bureau of Economic Analysis API key by following the instructions for [bea_key()].
#' @param yrs Supply a year or range of years. If no value is supplied, will default to the largest possible range of years across which data is available on at least one of the selected variables.
#' @param geog Specify either "county" or "cbsa".
#' @param state Optionally (ONLY if geog is set to "county"), supply a vector of state abbreviations for filter to. By default, all possible values will be returned by the function.
#' @param geoselect Optionally, supply a vector of county or CBSA (depending on whether geog is set to "county" or "cbsa") GEOIDs to filter to. By default, all possible values will be returned by the function.
#' @export

retrieve_data <- function(vars, yrs = 1980:2024, geog, state = NULL, geoselect = NULL) {
  
  # creating an empty dataframe with column names for use when rbind-ing datasets together
  empty_df <- data.frame(matrix(nrow = 0, ncol = 4))
  colnames(empty_df) = c("GEOID", "variable", "estimate", "moe")
  
  
  # filtering based on the availability of ACS data for counties and CBSAs (which seems to always be 5-year)
  census_yrs <- yrs[yrs >= 2009 & yrs <= 2023]
  
  if (all(("unemployment" %in% vars | "high-skill employment" %in% vars | "median family income" %in% vars | "poverty" %in% vars | "old-age dependency ratio" %in% vars | "housing vacancy" %in% vars), length(census_yrs) != 0, (!identical(vars, "old-age dependency ratio") | !identical(yrs, 2009)))) {
    
    census_vars <- vars[vars %in% c("unemployment", "high-skill employment", "median family income", "poverty", "old-age dependency ratio", "housing vacancy")]
    
    census_data <- census_retrieval(vars = census_vars, yrs = census_yrs, geog = geog)
  } else {census_data <- empty_df}
  
  
  bea_yrs <- yrs[yrs >= 2001 & yrs <= 2023]
  
  if ("GDP per capita" %in% vars & length(bea_yrs) != 0) {
    
    bea_data <- bea_retrieval(yrs = bea_yrs, geog = geog)
  } else {bea_data <- empty_df}
  
  
  aqi_yrs <- yrs[yrs >= 1980 & yrs <= 2024]
  
  if ("median AQI" %in% vars & length(aqi_yrs) != 0) {
    
    aqi_data <- aqi_retrieval(yrs = aqi_yrs, geog = geog)
  } else {aqi_data <- empty_df}
  
  data_all <- rbind(census_data, bea_data, aqi_data)
  
  
  # retrieving county/CBSA names to join with data
  tidycensus::census_api_key(census_key_value)
  if (geog == "county") {
    geog_county <- tidycensus::fips_codes |>
      dplyr::mutate(GEOID = paste0(state_code, county_code)) |>
      dplyr::mutate(county = paste0(county, ", ", state_name)) |>
      dplyr::rename(state_abbr = state) |>
      dplyr::select(c(GEOID, county, state_abbr))
    
    data_all <- dplyr::inner_join(data_all, geog_county, by = dplyr::join_by(GEOID))
    
    if (!is.null(state)) {
      
      data_all <- data_all |>
        dplyr::filter(state_abbr %in% state)
    }}
  
  if (geog == "cbsa") {
    geog_cbsa <- tidycensus::get_acs(geography = "cbsa", variables = "DP03_0005P", year = 2023) |>
      dplyr::select(c(GEOID, NAME)) |>
      dplyr::rename(cbsa = NAME) |>
      dplyr::distinct()
    
    data_all <- dplyr::inner_join(data_all, geog_cbsa, by = dplyr::join_by(GEOID))}
  
  if (!is.null(geoselect)) {
    
    data_all <- data_all |>
      dplyr::filter(GEOID %in% geoselect)
  }
  
  return(data_all)
}