# tests/testthat/test-atc-categories.R
test_that("ATC categories are properly defined", {
  expect_equal(length(ATC_CATEGORIES), 14)
  expect_true("C" %in% names(ATC_CATEGORIES))
  expect_equal(ATC_CATEGORIES[["N"]]$name, "Nervous system")
})

# tests/testthat/test-prescription-rates.R
test_that("prescription rate functions validate inputs", {
  # Mock a connection object
  mock_conn <- structure(list(), class = "DBIConnection")

  # Test validation failures
  expect_error(get_atc_prescription_count(mock_conn, "", "C"))
  expect_error(get_atc_prescription_count(mock_conn, "schema", "Z"))
})
