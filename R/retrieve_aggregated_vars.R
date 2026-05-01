globalVariables(c("category"))

#' Returns table of variables retrievable through [retrieve_aggregated()]
#' @description Returns a table with all variables retrievable through the
#' [retrieve_aggregated()] function. Variables are sorted
#' into ten categories: economy, housing & family, transportation &
#' walkability, health, education, crime, community spaces, sustainability,
#' governance, and demographics. For each variable, the table lists its
#' corresponding category as well as other information on the given variable
#' including whether an API key is required for retrieval, data source, data
#' type, and available years (`yrs_available` is `NA` when the source data is updated regularly and thus, in 
#'  [retrieve_aggregated()], the variable can only be retrieved when `yrs` is set to `NULL`) and geographic scales. If an API key is required
#' to retrieve a certain variable, run the indicated function for setting the
#' API key before attempting to retrieve the variable for the first time
#' using [retrieve_aggregated()]. Once set, the API key will be saved across R
#' sessions. Some variables are computed by cidar using multiple variables from
#'  the data source. More information on how the variables were selected and
#' categorized is available at (add link to data documentation w/ explanation?).
#' @details Data source links and further notes:
#' \itemize{
#' \item Census Bureau API - American Community Survey 5-year estimates: This API data is accessed through the [tidycensus package](https://cran.r-project.org/web/packages/tidycensus/index.html). The American Community Survey estimates should only be compared with each other across non-overlapping 5-year periods. See the Census Bureau webpage [here](https://www.census.gov/programs-surveys/acs/guidance/comparing-acs-data.html) for more guidance on data use.
#' \item National Transportation Atlas Database, Bureau of Transportation Statistics, U.S. Department of Transportation - ArcGIS Online: The `transit stops` dataset is pulled from [National Transit Map Stops](https://www.arcgis.com/home/item.html?id=6be77dd1081a43c0865282cad76718ab). Number of transit stops per square mile of land area is calculated using Census Tiger/Line Shapefile land area information retrieved via the [tigris package](https://cran.r-project.org/web/packages/tigris/index.html) and converted to square miles.
#' \item U.S. Bureau of Economic Analysis API: This API data is accessed through the [bea.R package](https://cran.r-project.org/web/packages/bea.R/index.html).
#' \item U.S. Environmental Protection Agency: See the EPA Air Data pre-generated data files [here](https://aqs.epa.gov/aqsweb/airdata/download_files.html) for more information on data files.
#' \item U.S. Federal Bureau of Investigation - Crime in the United States estimations: See [crime_by_msa_2020], [crime_by_msa_2022], [crime_by_msa_2023], and [crime_by_msa_2024] for documentation on the crime data used for 2020-2023. For previous years, crime data is pulled from [Crime in the U.S.](https://ucr.fbi.gov/crime-in-the-u.s) Metropolitan Statistical Area-level data files. Be careful in comparing estimates across years; see the FBI Universal Crime Reporting document [here](https://ucr.fbi.gov/ucr-statistics-their-proper-use) for additional guidance on data use. Note that the definition of rape was changed between 2012 and 2013, see the Crime in the United States 2013 rape addendum [here](https://ucr.fbi.gov/crime-in-the-u.s/2013/crime-in-the-u.s.-2013/rape-addendum/rape_addendum_final) for more details.
#' \item National Center for Education Statistics - ArcGIS Online: This data is aggregated from the National Center for Education Statistics (NCES)' Public School Locations and Private School Locations feature sets. For all years between 2015 and 2024, there exists a Public School Locations dataset spanning that year and the one following it (e.g. 2015-2016, 2016-2017, etc). For every odd-numbered year between 2015 and 2023, there exists a Private School Locations dataset spanning that year and the one following it (e.g. 2015-2016, 2017-2018, etc). Requesting `schools` data via [retrieve_aggregated()] will return data from the Public School Locations beginning with the given year, and from the Private School Locations dataset including that year in its timeframe. More information about the NCES school point data is available [here](https://nces.ed.gov/programs/edge/Geographic/SchoolLocations).
#' }
#' @export

retrieve_aggregated_vars <- function() {
  vars_df <- data.frame(
    variable = c("unemployment", "high-skill employment", "median family income", "poverty", "old-age dependency ratio", "housing vacancy", "Gini index", "median home value", "median gross rent", "households without vehicle", "households with broadband", "homeownership", "single-parent families", "public transport commutes", "mean commute time", "GDP per capita", "median AQI", "unhealthy air days", "violent crime", "murder and nonnegligent manslaughter", "rape", "robbery", "aggravated assault", "property crime", "burglary", "larceny", "motor vehicle theft", "schools", "transit stops"),
    category = c("economy", "economy", "economy", "economy", "economy", "housing & family", "economy", "housing & family", "housing & family", "housing & family", "housing & family", "housing & family", "housing & family", "transportation & walkability", "transportation & walkability", "economy", "sustainability", "sustainability", "crime", "crime", "crime", "crime", "crime", "crime", "crime", "crime", "crime", "education", "transportation & walkability"),
    api_key_required = c(rep("yes, set using census_key()", 15), "yes, set using bea_key()", rep("no", 13)),
    source = c(rep("Census Bureau API - American Community Survey 5-year estimates", 15), "U.S. Bureau of Economic Analysis API", rep("U.S. Environmental Protection Agency", 2), rep("U.S. Federal Bureau of Investigation - Crime in the United States estimations", 9), "National Center for Education Statistics - ArcGIS Online", "National Transportation Atlas Database, Bureau of Transportation Statistics, U.S. Department of Transportation - ArcGIS Online"),
    type = c("percent (of civilian labor force age 16+)", "percent (of civilian employed population 16+)", "value (dollars)", "percent (of population)", "ratio (of people age 65+ per 100 people age 18-64)", "percent (of housing units)", "summary measure (from 0, perfect income equality, to 1, perfect income inequality)", "value (dollars)", "value (dollars)", "percent (of households)", "percent (of households)", "percent (of population)", "percent (of families with children under 18)", "percent (of motorized commutes to work)", "value (minutes)", "ratio (per capita, in chained 2017 dollars)", "value (index ranging from 0, excellent, to 500, extremely hazardous)", "percent (of days with data available)", rep("rate (per 100,000 inhabitants)", 9), "count (in given area)", "count per sq mi of land"),
    yrs_available = c(rep("2009-2023", 4), "2010-2023", "2009-2023", "2010-2023", rep("2009-2023", 3), "2013-2023", rep("2009-2023", 4), "2001-2023", rep("1980-2024", 2), rep("1999-2020, 2022-2024", 9), "2015-2024", NA),
    geogs_available = c(rep("county, cbsa, place", 15), rep("county, cbsa", 3), rep("cbsa", 9), rep("county, cbsa, place", 2))
  ) |>
    dplyr::arrange(category, variable)

  return(vars_df)
}
