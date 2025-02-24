# tests/testthat/test-domain-stats.R
test_that("domain validation works", {
  expect_true(validate_domain("condition"))
  expect_error(validate_domain("invalid_domain"))
})

# Add more tests for your domain functions
