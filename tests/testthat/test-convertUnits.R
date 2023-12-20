#library(testthat)
library(CocktailSelector)

test_that("convertUnits correctly converts imperial to international units", {
  test_data <- test_data <- data.frame(
    col1 = rep(NA, 4),
    col2 = rep(NA, 4),
    col3 = rep(NA, 4),
    col4 = rep(NA, 4),
    col5 = rep(NA, 4),
    col6 = rep(NA, 4),
    units = c("1 inch", "2.5 lb", "3 cm", "0.25 oz")
  )
  expected_data <- data.frame(
    col1 = rep(NA, 4),
    col2 = rep(NA, 4),
    col3 = rep(NA, 4),
    col4 = rep(NA, 4),
    col5 = rep(NA, 4),
    col6 = rep(NA, 4),
    units = c("2.5 cm", "1.1 kg", "3 cm", "10 ml")
  )

  result_data <- convertUnits(test_data, "imperial_to_international")
  expect_equal(result_data, expected_data)
})
