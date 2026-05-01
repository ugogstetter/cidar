globalVariables(c("geometry_type", "features"))

#' Returns table of geospatial feature sets retrievable through [retrieve_point_polygon()]
#' @description Returns a table with all point and polygon feature sets
#' retrievable through the [retrieve_point_polygon()] function. For each set of
#' features, the table lists the geometry type (point or polygon), whether
#' an API key is required for data retrieval, what the data source is, what
#' subtypes the feature set is categorized into ("types"), what years the feature
#'  set covers (`NA` means the feature set is updated regularly and thus, in 
#'  [retrieve_point_polygon()], can only be retrieved when `yrs` is set to `NULL`), and what geography types
#'  the feature set can be filtered by. If an API key is required
#' to retrieve a certain variable, run the indicated function for setting the
#' API key before attempting to retrieve the variable for the first time
#' using [retrieve_point_polygon()]. Once set, the API key will be saved across R
#' sessions.
#' @details Data source links and further notes:
#' \itemize{
#' \item TomTom, ArcGIS Data and Maps - ArcGIS Online: The `parks` features are pulled from [USA Parks](https://www.maps.arcgis.com/home/item.html?id=e49e181ac82c46edac3ae601ebb3ef2d).
#' \item National Transportation Atlas Database, Bureau of Transportation Statistics, U.S. Department of Transportation - ArcGIS Online: The `transit stops` features are pulled from [National Transit Map Stops](https://www.arcgis.com/home/item.html?id=6be77dd1081a43c0865282cad76718ab).
#' \item National Center for Education Statistics - ArcGIS Online: The `schools` features are pulled from the National Center for Education Statistics (NCES)' Public School Locations and Private School Locations datasets. For all years between 2015 and 2024, there exists a Public School Locations dataset spanning that year and the one following it (e.g. 2015-2016, 2016-2017, etc). For every odd-numbered year between 2015 and 2023, there exists a Private School Locations dataset spanning that year and the one following it (e.g. 2015-2016, 2017-2018, etc). Requesting `schools` data via [retrieve_point_polygon()] will return data from the Public School Locations beginning with the given year, and from the Private School Locations dataset including that year in its timeframe. More information about the NCES school point data is available [here](https://nces.ed.gov/programs/edge/Geographic/SchoolLocations).
#' \item Institute of Museum and Library Services: The `libraries` features are pulled from the [Public Libraries Survey data files](https://www.imls.gov/research-evaluation/surveys/public-libraries-survey-pls).
#' \item IRS Business Master File Data Catalog - National Center for Charitable Statistics, Urban Institute: The `nonprofits` features are pulled from the [United BMF data files](https://nccs.urban.org/nccs/catalogs/catalog-bmf.html). The inconsistent quality of addresses provided for geocoding in this dataset means that some geocodes are incorrect or imprecise. In `cidar`'s version of the dataset, geocodes likely to be imprecise or inaccurate are removed; however, this means that not all nonprofits located in a given area will appear in the shapefile data. The values from this feature set's `type` variable correspond to the National Taxonomy of Exempt Entities Level 1 Industry Group code categories, as outlined on [this](https://nccs.urban.org/nccs/widgets/ntee_tables/ntee_descriptions.html) Urban Institute webpage.
#' \item Fatality Analysis Reporting System, National Highway Traffic Safety Administration: The `crashes` features are pulled from the [National Highway Traffic Safety Administration Fatality Analysis Reporting System data files](https://www.nhtsa.gov/file-downloads?p=nhtsa/downloads/FARS/). In `cidar`'s version of the dataset, some crashes had to be removed due to having missing or unknown coordinates, and thus being impossible to convert into shapefile format.
#' }
#' @export

retrieve_point_polygon_features <- function() {
  features_df <- data.frame(
    features = c("parks", "transit stops", "schools", "libraries", "nonprofits", "fatal crashes"),
    geometry_type = c("polygon", rep("point", 5)),
    api_key_required = c(rep("no", 6)),
    source = c("TomTom, ArcGIS Data and Maps - ArcGIS Online", "National Transportation Atlas Database, Bureau of Transportation Statistics, U.S. Department of Transportation - ArcGIS Online", "National Center for Education Statistics - ArcGIS Online", "Institute of Museum and Library Services", "IRS Business Master File Data Catalog - National Center for Charitable Statistics, Urban Institute", "Fatality Analysis Reporting System, National Highway Traffic Safety Administration"),
    types = c("local park; national park or forest; regional park; state park or forest; county park", "bus; tram, streetcar, light rail; rail; ferry; subway, metro; trolleybus; funicular; aerial lift, suspended cable car; cable tram; monorail", "public; private", "central library; branch library; bookmobile(s); books-by-mail only", "arts, culture, and humanities; education; environment and animals; health; human services; international, foreign affairs; mutual/membership benefit; public, societal benefit; religion related; university; hospital; unknown", "front-to-rear collision; front-to-front collision; rear-to-rear collision; collision at angle; sideswipe; other/unknown"),
    yrs_available = c(rep(NA, 2), "2015-2024", "2008-2023", "1989-2023", "1999-2024"),
    geogs_available = c(rep("state, county, cbsa, place", 2), rep("state, county, cbsa, place, zip code", 2), rep("state, county, cbsa, place", 2))
  ) |>
    dplyr::arrange(geometry_type, features)

  return(features_df)
}
