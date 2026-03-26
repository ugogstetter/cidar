globalVariables(c("category"))

#' Returns table of variables retrievable through [retrieve_aggregated()]
#' @description Returns a table with all variables retrievable through the
#' [retrieve_aggregated()] function. Variables are sorted
#' into ten categories: economy, housing & family, transportation &
#' walkability, health, education, crime, community spaces, sustainability,
#' governance, and demographics. For each variable, the table lists its
#' corresponding category as well as other information on the given variable
#' including whether an API key is required for retrieval, data source, data
#' type, and available years and geographic scales. If an API key is required
#' to retrieve a certain variable, run the indicated function for setting the
#' API key before attempting to retrieve the variable for the first time
#' using [retrieve_aggregated()]. Once set, the API key will be saved across R
#' sessions. Some variables are computed by cidar using multiple variables from
#'  the data source. More information on how the variables were selected and
#' categorized is available at (add link to data documentation w/ explanation?).
#' @details Details on included datasets:
#' \itemize{
#' \item Census Bureau API - American Community Survey 5-year estimates: These estimates should only be compared with each other across non-overlapping 5-year periods. See the Census Bureau webpage [here](https://www.census.gov/programs-surveys/acs/guidance/comparing-acs-data.html) for more guidance on data use.
#' \item U.S. Environmental Protection Agency: See the EPA Air Data pre-generated data files [here](https://aqs.epa.gov/aqsweb/airdata/download_files.html) for more information on data files.
#' \item U.S. Federal Bureau of Investigation - Crime in the United States estimations: Be careful in comparing estimates across years; see the FBI Universal Crime Reporting document [here](https://ucr.fbi.gov/ucr-statistics-their-proper-use) for additional guidance on data use. Note that the definition of rape was changed between 2012 and 2013, see the Crime in the United States 2013 rape addendum [here](https://ucr.fbi.gov/crime-in-the-u.s/2013/crime-in-the-u.s.-2013/rape-addendum/rape_addendum_final) for more details.
#' }
#' @export

retrieve_aggregated_vars <- function() {
  vars_df <- data.frame(
    variable = c("unemployment", "high-skill employment", "median family income", "poverty", "old-age dependency ratio", "housing vacancy", "Gini index", "median home value", "median gross rent", "households without vehicle", "households with broadband", "homeownership", "single-parent families", "public transport commutes", "mean commute time", "GDP per capita", "median AQI", "unhealthy air days", "violent crime", "murder and nonnegligent manslaughter", "rape", "robbery", "aggravated assault", "property crime", "burglary", "larceny", "motor vehicle theft"),
    category = c("economy", "economy", "economy", "economy", "economy", "housing & family", "economy", "housing & family", "housing & family", "housing & family", "housing & family", "housing & family", "housing & family", "transportation & walkability", "transportation & walkability", "economy", "sustainability", "sustainability", "crime", "crime", "crime", "crime", "crime", "crime", "crime", "crime", "crime"),
    api_key_required = c(rep("yes, set using census_key()", 15), "yes, set using bea_key()", rep("no", 11)),
    source = c(rep("Census Bureau API - American Community Survey 5-year estimates", 15), "U.S. Bureau of Economic Analysis API", rep("U.S. Environmental Protection Agency", 2), rep("U.S. Federal Bureau of Investigation - Crime in the United States estimations", 9)),
    type = c("percent (of civilian labor force age 16+)", "percent (of civilian employed population 16+)", "value (dollars)", "percent (of population)", "ratio (of people age 65+ per 100 people age 18-64)", "percent (of housing units)", "summary measure (from 0, perfect income equality, to 1, perfect income inequality)", "value (dollars)", "value (dollars)", "percent (of households)", "percent (of households)", "percent (of population)", "percent (of families with children under 18)", "percent (of motorized commutes to work)", "value (minutes)", "ratio (per capita, in chained 2017 dollars)", "value (index ranging from 0, excellent, to 500, extremely hazardous)", "percent (of days with data available)", rep("rate (per 100,000 inhabitants)", 9)),
    yrs_available = c(rep("2009-2023", 4), "2010-2023", "2009-2023", "2010-2023", rep("2009-2023", 3), "2013-2023", rep("2009-2023", 4), "2001-2023", rep("1980-2024", 2), rep("1999-2020, 2022-2024", 9)),
    geogs_available = c(rep("county, cbsa", 18), rep("cbsa", 9))
  ) |>
    dplyr::arrange(category, variable)

  return(vars_df)
}
