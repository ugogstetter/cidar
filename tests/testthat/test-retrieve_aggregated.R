# testing that there will be no warning and no error when the input is valid and has the character type of variable input
testthat::test_that("valid input with character", {
  testthat::expect_no_warning(retrieve_aggregated(vars = c("unemployment", "GDP per capita"), yrs = 2020, geog = "county", geoselect = "01001"))
  testthat::expect_no_error(retrieve_aggregated(vars = c("unemployment", "GDP per capita"), yrs = 2020, geog = "county", geoselect = "01001"))
})

# testing that there will be no warning and no error when the input is valid and has the data frame type of variable input
testthat::test_that("valid input with data frame", {
  test_df <- retrieve_aggregated_vars() |>
    dplyr::filter(category == "housing & family")

  testthat::expect_no_warning(retrieve_aggregated(vars = test_df, yrs = 2022, geog = "place", geoselect = "1714000"))
  testthat::expect_no_error(retrieve_aggregated(vars = test_df, yrs = 2022, geog = "place", geoselect = "1714000"))
})

# testing that the appropriate error will be returned when `geog` is `cbsa` and `state` is not `NULL`
testthat::test_that("geog is cbsa and state is not NULL", {
  testthat::expect_error(retrieve_aggregated(vars = c("unemployment", "median AQI"), yrs = 2020, state = "NY", geog = "cbsa"), "A `state` value cannot be provided when `geog` is `cbsa`, because CBSAs can extend across state lines.")
})

# testing that there will be no warning and no error when the input is valid and `yrs` is set to `NULL`
testthat::test_that("valid input with NULL yrs", {
  testthat::expect_no_warning(retrieve_aggregated(vars = c("mean commute time", "robbery"), yrs = NULL, geog = "cbsa", geoselect = "10740"))
  testthat::expect_no_error(retrieve_aggregated(vars = c("mean commute time", "robbery"), yrs = NULL, geog = "cbsa", geoselect = "10740"))
})

# testing that the correct error is produced when features is not of class `character` or `data.frame`
testthat::test_that("features of invalid class", {
  test_df <- retrieve_aggregated_vars() |>
    dplyr::filter(category == "housing & family")

  testthat::expect_error(retrieve_aggregated(vars = as.list(test_df), yrs = 2022, geog = "place", geoselect = "1714000"), "`vars` must be of class `character` or `data.frame`.")
})

# testing that the correct error is produced when `yrs` value cannot be coerced to numeric
testthat::test_that("non-numeric yrs", {
  testthat::expect_error(retrieve_aggregated(vars = c("unemployment", "GDP per capita", "unhealthy air days"), yrs = "apple", geog = "county", geoselect = "01001"), "Must enter `yrs` value that can be coerced to numeric.")
})

# testing that the correct error is produced when `yrs` is not a whole number
testthat::test_that("non-whole number yrs", {
  testthat::expect_error(retrieve_aggregated(vars = c("unemployment", "GDP per capita", "unhealthy air days"), yrs = 2022.5, geog = "county", geoselect = "01001"), "All numeric values in `yrs` must be whole numbers.")
})

# testing that the correct error is produced when `geog` is `NULL`
testthat::test_that("NULL geog", {
  testthat::expect_error(retrieve_aggregated(vars = c("unemployment", "GDP per capita", "unhealthy air days"), yrs = 2020, geog = NULL, geoselect = "01001"), "`geog` must have length 1.")
})

# testing that the correct error is produced when `geog` does not have length 1
testthat::test_that("geog not length 1", {
  testthat::expect_error(retrieve_aggregated(vars = c("unemployment", "GDP per capita", "unhealthy air days"), yrs = 2020, geog = c("county", "CBSA")), "`geog` must have length 1.")
})

# testing that the correct error is produced when `geog` is not `county`, `cbsa`, or `place`
testthat::test_that("geog not acceptable value", {
  testthat::expect_error(retrieve_aggregated(vars = c("unemployment", "GDP per capita", "unhealthy air days"), yrs = 2020, geog = "country"), "`geog` must be `county`, `cbsa`, or `place`.")
})

# testing that the correct error is produced when `vars_info` is `NULL`
testthat::test_that("NULL vars_info", {
  testthat::expect_error(retrieve_aggregated(vars = c("unemployment", "GDP per capita", "unhealthy air days"), yrs = 2020, geog = "county", geoselect = "01001", vars_info = NULL), "`vars_info` must have length 1.")
})

# testing that the correct error is produced when `vars_info` does not have length 1
testthat::test_that("vars_info not length 1", {
  testthat::expect_error(retrieve_aggregated(vars = c("unemployment", "GDP per capita", "unhealthy air days"), yrs = 2020, geog = "county", geoselect = "01001", vars_info = c(TRUE, FALSE)), "`vars_info` must have length 1.")
})

# testing that the correct error is produced when `vars_info` is not coercible to logical
testthat::test_that("vars_info not coercible to logical", {
  testthat::expect_error(retrieve_aggregated(vars = c("unemployment", "GDP per capita", "unhealthy air days"), yrs = 2020, geog = "county", geoselect = "01001", vars_info = "tru"), "`vars_info` must be coercible to logical (`TRUE` or `FALSE`).", fixed = TRUE)
})

# testing that the correct error is produced when `sf` is `NULL`
testthat::test_that("NULL sf", {
  testthat::expect_error(retrieve_aggregated(vars = c("unemployment", "GDP per capita", "unhealthy air days"), yrs = 2020, geog = "county", geoselect = "01001", sf = NULL), "`sf` must have length 1.")
})

# testing that the correct error is produced when `sf` does not have length 1
testthat::test_that("sf not length 1", {
  testthat::expect_error(retrieve_aggregated(vars = c("unemployment", "GDP per capita", "unhealthy air days"), yrs = 2020, geog = "county", geoselect = "01001", sf = c(TRUE, FALSE)), "`sf` must have length 1.")
})

# testing that the correct error is produced when `sf` is not coercible to logical
testthat::test_that("sf not coercible to logical", {
  testthat::expect_error(retrieve_aggregated(vars = c("unemployment", "GDP per capita", "unhealthy air days"), yrs = 2020, geog = "county", geoselect = "01001", sf = "tru"), "`sf` must be coercible to logical (`TRUE` or `FALSE`).", fixed = TRUE)
})

# testing that the correct error is produced when `state` contains invalid values
testthat::test_that("state contains invalid values", {
  testthat::expect_error(retrieve_aggregated(vars = c("unemployment", "GDP per capita", "unhealthy air days"), yrs = 2020, state = c("MA", "AK", "AA", "AB"), geog = "county"), "`state` contains the following invalid state abbreviation(s): AA, AB. Make sure `state` is set equal to a character vector of two-letter abbreviations for any of the 50 states or DC.", fixed = TRUE)
})

# testing that the correct error is produced when neither `state` nor `geoselect` are `NULL`
testthat::test_that("state and geoselect both not NULL", {
  testthat::expect_error(retrieve_aggregated(vars = c("unemployment", "GDP per capita", "unhealthy air days"), yrs = 2020, state = "AL", geog = "county", geoselect = "01001"), "If a value is provided for `state`, then `geoselect` must be `NULL`.")
})

# testing that the correct error is produced when `geog` contains invalid GEOIDS for `county`
testthat::test_that("geog contains invalid county GEOIDS", {
  testthat::expect_error(retrieve_aggregated(vars = c("unemployment", "GDP per capita", "unhealthy air days"), yrs = 2020, geog = "county", geoselect = c("01001", "100000", "200000")), "`geoselect` contains the following invalid GEOID(s): 100000, 200000. Use GEOIDs that are present in the `GEOID` column of the dataset retrieved by `county_geoids()`.", fixed = TRUE)
})

# testing that the correct error is produced when `geog` contains invalid GEOIDS for `cbsa`
testthat::test_that("geog contains invalid cbsa GEOIDS", {
  testthat::expect_error(retrieve_aggregated(vars = c("unemployment", "robbery"), yrs = 2022, geog = "cbsa", geoselect = c("10740", "100000", "200000")), "`geoselect` contains the following invalid GEOID(s): 100000, 200000. Use GEOIDs that are present in the `GEOID` column of the dataset retrieved by `cbsa_geoids()`.", fixed = TRUE)
})

# testing that the correct error is produced when `geog` contains invalid GEOIDS for `place`
testthat::test_that("geog contains invalid place GEOIDS", {
  testthat::expect_error(retrieve_aggregated(vars = c("unemployment", "median family income"), yrs = 2022, geog = "place", geoselect = c("1714000", "100000", "200000")), "`geoselect` contains the following invalid GEOID(s): 100000, 200000. Use GEOIDs that are present in the `GEOID` column of the dataset retrieved by `place_geoids()`.", fixed = TRUE)
})

# testing that the correct error is produced when an incorrect data frame is provided for `vars`
testthat::test_that("incorrect data frame for vars", {
  testthat::expect_error(retrieve_aggregated(vars = as.data.frame(list("vars" = c("unemployment", "high-skill employment", "median family income"), "category" = c("economy", "economy", "economy"))), yrs = 2020, geog = "county", geoselect = "01001"), "The data frame you have provided for `vars` must be a filtered version of the `retrieve_aggregated_vars()` data frame.", fixed = TRUE)
})

# testing that the correct error is produced when `vars` contains invalid variable names
testthat::test_that("invalid variable names in vars", {
  testthat::expect_error(retrieve_aggregated(vars = c("homownership", "unemployment", "GDP per capita", "unhealthy air dayys"), yrs = 2020, geog = "county", geoselect = "01001"), "`vars` contains the following invalid variable names: homownership, unhealthy air dayys. All variables available for retrieval through this function are listed in the `variable` column of the dataset retrieved by `retrieve_aggregated_vars()`.", fixed = TRUE)
})

# testing that the correct warning is produced, but no error is produced, when a Census variable has no data available at any of the given geographies
testthat::test_that("Census variable has no data available for given geographies", {
  testthat::expect_warning(retrieve_aggregated(vars = c("median gross rent", "Gini index"), yrs = 2023, geog = "place", geoselect = c("0100484", "0100100")), "For the following variable(s), data is not available for the given year(s) and any of the given geographies: `median gross rent`. The inputs provided were valid, some datasets simply do not cover all geographic areas due to limitations in data collection and aggregation.", fixed = TRUE)
  testthat::expect_no_error(retrieve_aggregated(vars = c("median gross rent", "Gini index"), yrs = 2023, geog = "place", geoselect = c("0100484", "0100100")))
})

# testing that the correct warning is produced, but no error is produced, when the BEA variable has no data available at any of the given geographies
testthat::test_that("BEA variable has no data available for given geographies", {
  testthat::expect_warning(retrieve_aggregated(vars = c("Gini index", "GDP per capita"), yrs = 2023, geog = "county", geoselect = c("15009", "51059")), "For the following variable(s), data is not available for the given year(s) and any of the given geographies: `GDP per capita`. The inputs provided were valid, some datasets simply do not cover all geographic areas due to limitations in data collection and aggregation.", fixed = TRUE)
  testthat::expect_no_error(retrieve_aggregated(vars = c("Gini index", "GDP per capita"), yrs = 2023, geog = "county", geoselect = c("15009", "51059")))
})

# testing that the correct warning is produced, but no error is produced, when an EPA variable has no data available at any of the given geographies
testthat::test_that("EPA variable has no data available for given geographies", {
  testthat::expect_warning(retrieve_aggregated(vars = c("unemployment", "median AQI"), yrs = 2022, geog = "county", geoselect = "01001"), "For the following variable(s), data is not available for the given year(s) and any of the given geographies: `median AQI`. The inputs provided were valid, some datasets simply do not cover all geographic areas due to limitations in data collection and aggregation.", fixed = TRUE)
  testthat::expect_no_error(retrieve_aggregated(vars = c("unemployment", "median AQI"), yrs = 2022, geog = "county", geoselect = "01001"))
})

# testing that the correct warning is produced, but no error is produced, when an crime variable has no data available at any of the given geographies
testthat::test_that("crime variable has no data available for given geographies", {
  testthat::expect_warning(retrieve_aggregated(vars = c("unemployment", "robbery"), yrs = 2020, geog = "cbsa", geoselect = c("10100", "10760")), "For the following variable(s), data is not available for the given year(s) and any of the given geographies: `robbery`. The inputs provided were valid, some datasets simply do not cover all geographic areas due to limitations in data collection and aggregation.", fixed = TRUE)
  testthat::expect_no_error(retrieve_aggregated(vars = c("unemployment", "robbery"), yrs = 2020, geog = "cbsa", geoselect = c("10100", "10760")))
})

# testing that the correct warning is produced, but no error is produced, when the schools variable has no data available at any of the given geographies
testthat::test_that("schools variable has no data available for given geographies", {
  testthat::expect_warning(retrieve_aggregated(vars = c("schools", "unemployment"), yrs = 2023, geog = "place", geoselect = "3668088"), "For the following variable(s), data is not available for the given year(s) and any of the given geographies: `schools`. The inputs provided were valid, some datasets simply do not cover all geographic areas due to limitations in data collection and aggregation.", fixed = TRUE)
  testthat::expect_no_error(retrieve_aggregated(vars = c("schools", "unemployment"), yrs = 2023, geog = "place", geoselect = "3668088"))
})

# testing that the correct warning is produced, but no error is produced, when the transit stops variable has no data available at any of the given geographies
#testthat::test_that("transit stops variable has no data available for given geographies", {
#  testthat::expect_warning(retrieve_aggregated(vars = c("transit stops", "unemployment"), yrs = NULL, geog = "place", geoselect = c("3351460", "3362580")), "For the following variable(s), data is not available for the given year(s) and any of the given geographies: `transit stops`. The inputs provided were valid, some datasets simply do not cover all geographic areas due to limitations in data collection and aggregation.", fixed = TRUE)
#  testthat::expect_no_error(retrieve_aggregated(vars = c("transit stops", "unemployment"), yrs = NULL, geog = "place", geoselect = c("3351460", "3362580")))
#})

# testing that the correct warning is produced, but no error is produced, when the libraries variable has no data available at any of the given geographies
testthat::test_that("libraries variable has no data available for given geographies", {
  testthat::expect_warning(retrieve_aggregated(vars = "libraries", yrs = 2019, geog = "county", geoselect = "02063"), "For the following variable(s), data is not available for the given year(s) and any of the given geographies: `libraries`. The inputs provided were valid, some datasets simply do not cover all geographic areas due to limitations in data collection and aggregation.", fixed = TRUE)
  testthat::expect_no_error(retrieve_aggregated(vars = "libraries", yrs = 2019, geog = "county", geoselect = "02063"))
})

# testing that the correct warning is produced, but no error is produced, when nonprofits variables have no data available at any of the given geographies
testthat::test_that("nonprofits variable has no data available for given geographies", {
  testthat::expect_warning(retrieve_aggregated(vars = c("nonprofits", "unemployment", "health-related nonprofits"), yrs = 2020, geog = "place", geoselect = c("0220716", "0145472")), "For the following variable(s), data is not available for the given year(s) and any of the given geographies: `nonprofits`, `health-related nonprofits`. The inputs provided were valid, some datasets simply do not cover all geographic areas due to limitations in data collection and aggregation.", fixed = TRUE)
  testthat::expect_no_error(retrieve_aggregated(vars = c("nonprofits", "unemployment", "health-related nonprofits"), yrs = 2020, geog = "place", geoselect = c("0220716", "0145472")))
})

# testing that the correct warning is produced, but no error is produced, when the fatal crashes variable has no data available at any of the given geographies
testthat::test_that("fatal crashes variable has no data available for given geographies", {
  testthat::expect_warning(retrieve_aggregated(vars = c("fatal crashes", "unemployment"), yrs = 2021, geog = "county", geoselect = "56027"), "For the following variable(s), data is not available for the given year(s) and any of the given geographies: `fatal crashes`. The inputs provided were valid, some datasets simply do not cover all geographic areas due to limitations in data collection and aggregation.", fixed = TRUE)
  testthat::expect_no_error(retrieve_aggregated(vars = c("fatal crashes", "unemployment"), yrs = 2021, geog = "county", geoselect = "56027"))
})

# testing that the correct warning is produced, but no error is produced, when the voter turnout variable has no data available at any of the given geographies
testthat::test_that("voter turnout variable has no data available for given geographies", {
  testthat::expect_warning(retrieve_aggregated(vars = c("voter turnout", "unemployment"), yrs = 2018, geog = "county", geoselect = c("05039", "02013")), "For the following variable(s), data is not available for the given year(s) and any of the given geographies: `voter turnout`. The inputs provided were valid, some datasets simply do not cover all geographic areas due to limitations in data collection and aggregation.", fixed = TRUE)
  testthat::expect_no_error(retrieve_aggregated(vars = c("voter turnout", "unemployment"), yrs = 2021, geog = "county", geoselect = c("05039", "02013")))
})

# testing that the correct warning is produced, but no error is produced, when the given `yrs` value works for some of the given variables but not others
testthat::test_that("yrs value works for some but not all vars", {
  testthat::expect_warning(retrieve_aggregated(vars = c("median AQI", "unhealthy air days", "GDP per capita"), yrs = 1989, geog = "county"), "The `GDP per capita` variable is not retrievable for any of the given `yrs` values. See the `yrs_available` column of the retrieve_aggregated_vars() dataset for more information.", fixed = TRUE)
  testthat::expect_no_error(retrieve_aggregated(vars = c("median AQI", "unhealthy air days", "GDP per capita"), yrs = 1989, geog = "county"))
})

# testing that the correct warning is produced, but no error is produced, when the given `geog` value works for some of the given variables but not others
testthat::test_that("geog value works for some but not all vars", {
  testthat::expect_warning(retrieve_aggregated(vars = c("unemployment", "voter turnout"), yrs = 2020, geog = "cbsa"), "The `voter turnout` variable is only available for the `county` geography.")
  testthat::expect_no_error(retrieve_aggregated(vars = c("unemployment", "voter turnout"), yrs = 2020, geog = "cbsa"))
})


# testing that all variables work for all `yrs` and `geog` values they should be valid for
#testthat::test_that("testing all combinations of yrs and geog values that are supposed to work for each variable", {
#  
#  aggregated_vars <- retrieve_aggregated_vars()
#  
#  for (i in 1:length(aggregated_vars$variable)) {
#    
#    if (aggregated_vars$variable[i] == "voter turnout") {
#      yrs_covered <- c(2010, 2012, 2014, 2016, 2018, 2020, 2022)
#    } else {
#      
#      yrs_covered_char <- aggregated_vars |>
#        dplyr::filter(variable == aggregated_vars$variable[i]) |>
#        dplyr::select(yrs_available) |>
#        as.character()
#      
#      if (is.na(yrs_covered_char)) {
#        yrs_covered <- NULL
#      } else {
#      
#      if (nchar(yrs_covered_char) == 9) {
#        yrs_covered <- as.numeric(substr(yrs_covered_char, 1, 4)):as.numeric(substr(yrs_covered_char, 6, 9))
#      }
#      
#      if (nchar(yrs_covered_char) == 20) {
#        yrs_covered <- c(as.numeric(substr(yrs_covered_char, 1, 4)):as.numeric(substr(yrs_covered_char, 6, 9)), as.numeric(substr(yrs_covered_char, 12, 15)):as.numeric(substr(yrs_covered_char, 17, 20)))
#      }
#      }
#    }
#    
#    if (!is.null(yrs_covered)) {
#      
#    for (x in 1:length(yrs_covered)) {
#      
#      geogs_covered <- aggregated_vars |>
#        dplyr::filter(variable == aggregated_vars$variable[i]) |>
#        dplyr::select(geogs_available) |>
#        as.character()
#      
#      geogs_covered <- strsplit(geogs_covered, ", ")[[1]]
#      
#      for (z in 1:length(geogs_covered)) {
#        
#        # setting geoselect value to make code run faster
#        
#        if (geogs_covered[z] == "county") {
#          geoselect <- "36001"
#        }
#        
#        if (geogs_covered[z] == "place") {
#          geoselect <- "2507000"
#        }
#        
#        if (geogs_covered[z] == "cbsa") {
#          geoselect <- "38300"
#        }
#        
#        testthat::expect_no_warning(retrieve_aggregated(vars = aggregated_vars$variable[i], yrs = yrs_covered[x], geog = geogs_covered[z], geoselect = geoselect))
#        testthat::expect_no_error(retrieve_aggregated(vars = aggregated_vars$variable[i], yrs = yrs_covered[x], geog = geogs_covered[z], geoselect = geoselect))
#      }
#    }
#  } else {
#    geogs_covered <- aggregated_vars |>
#      dplyr::filter(variable == aggregated_vars$variable[i]) |>
#      dplyr::select(geogs_available) |>
#      as.character()
#    
#    geogs_covered <- strsplit(geogs_covered, ", ")[[1]]
#    
#    for (z in 1:length(geogs_covered)) {
#      
#      # setting geoselect value to make code run faster
#      
#      if (geogs_covered[z] == "county") {
#        geoselect <- "36001"
#      }
#      
#      if (geogs_covered[z] == "place") {
#        geoselect <- "2507000"
#      }
#      
#      if (geogs_covered[z] == "cbsa") {
#        geoselect <- "38300"
#      }
#      
#      testthat::expect_no_warning(retrieve_aggregated(vars = aggregated_vars$variable[i], yrs = NULL, geog = geogs_covered[z], geoselect = geoselect))
#      testthat::expect_no_error(retrieve_aggregated(vars = aggregated_vars$variable[i], yrs = NULL, geog = geogs_covered[z], geoselect = geoselect))
#    }
#  }
#  }
#    
#})
#