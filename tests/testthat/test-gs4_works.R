test_that("gs4_auth_alternative", {
  expect_no_warning(auth_gs4_alternative)
  expect_no_error(auth_gs4_alternative)
})

test_that("gs4_auth_current", {
  expect_no_warning(auth_gs4)
  expect_no_error(auth_gs4)
})
