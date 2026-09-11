# testing that there will be no warning and no error when the input is valid and has the character type of variable input
testthat::test_that("valid input with character", {
  testthat::expect_no_warning(retrieve_aggregated(vars = c("unemployment", "GDP per capita", "unhealthy air days"), yrs = 2020, geog = "county", geoselect = "01001"))
  testthat::expect_no_error(retrieve_aggregated(vars = c("unemployment", "GDP per capita", "unhealthy air days"), yrs = 2020, geog = "county", geoselect = "01001"))
})

# testing that there will be no warning and no error when the input is valid and has the data frame type of variable input
testthat::test_that("valid input with data frame", {
  test_df <- retrieve_aggregated_vars() |>
    dplyr::filter(category == "housing & family")

  testthat::expect_no_warning(retrieve_aggregated(vars = test_df, yrs = 2022, geog = "place", geoselect = "1714000"))
  testthat::expect_no_error(retrieve_aggregated(vars = test_df, yrs = 2022, geog = "place", geoselect = "1714000"))
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
