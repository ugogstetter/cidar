# testing that there will be no warning and no error when the input is valid and has the character type of features input
testthat::test_that("valid input with character", {
  testthat::expect_no_warning(retrieve_point_polygon(features = c("fatal crashes", "nonprofits", "schools"), yrs = 2020, geog = "county", geoselect = "01001"))
  testthat::expect_no_error(retrieve_point_polygon(features = c("fatal crashes", "nonprofits", "schools"), yrs = 2020, geog = "county", geoselect = "01001"))
})

# testing that there will be no warning and no error when the input is valid and has the list type of features input
testthat::test_that("valid input with list", {
  testthat::expect_no_warning(retrieve_point_polygon(features = list("fatal crashes" = NULL, "libraries" = c("central library", "branch library"), "schools" = "private"), yrs = 2022, geog = "place", geoselect = "1714000"))
  testthat::expect_no_error(retrieve_point_polygon(features = list("fatal crashes" = NULL, "libraries" = c("central library", "branch library"), "schools" = "private"), yrs = 2022, geog = "place", geoselect = "1714000"))
})

# testing that there will be no warning and no error when the input is valid and `yrs` is set to `NULL`
testthat::test_that("valid input with NULL yrs", {
  testthat::expect_no_warning(retrieve_point_polygon(features = c("fatal crashes", "parks"), yrs = NULL, geog = "cbsa", geoselect = "10740"))
  testthat::expect_no_error(retrieve_point_polygon(features = c("fatal crashes", "parks"), yrs = NULL, geog = "cbsa", geoselect = "10740"))
})

# testing that the correct error is produced when features is not of class `character` or `list`
testthat::test_that("features of invalid class", {
  testthat::expect_error(retrieve_point_polygon(features = data.frame("libraries" = c("central library", "branch library"), "schools" = "private"), yrs = 2022, geog = "place", geoselect = "1714000"), "`features` must be of class `character` or `list`.")
})
