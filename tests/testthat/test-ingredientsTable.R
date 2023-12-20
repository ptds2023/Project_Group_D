
library(testthat)
library(dplyr)
library(tidyr)
library(xtable)

# Sample dataframe for testing
df <- data.frame(
  Name = c("daiquiri", "margarita"),
  Ingredient1 = c("Rum", "Tequila"),
  Quantity1 = c("50ml", "60ml"),
  Ingredient2 = c("Lime Juice", "Triple Sec"),
  Quantity2 = c("20ml", "30ml"),
  stringsAsFactors = FALSE
)

# Test: Invalid Cocktail Name
test_that("ingredientsTable returns NA for invalid cocktail name", {
  result <- ingredientsTable(df, "mojito")
  expected <- data.frame(Ingredient = NA, Quantity = NA)
  expect_equal(xtable::print.xtable(xtable(result), type = "html"),
               xtable::print.xtable(xtable(expected), type = "html"))
})
