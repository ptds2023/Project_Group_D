library(testthat)
library(dplyr)
library(tidyr)
library(kableExtra)

# Sample 'cocktails' dataframe
cocktails <- data.frame(
  Name = c("daiquiri", "margarita"),
  Ingredient1 = c("Rum", "Tequila"),
  Quantity1 = c("50ml", "60ml"),
  Ingredient2 = c("Lime Juice", "Triple Sec"),
  Quantity2 = c("20ml", "30ml"),
  stringsAsFactors = FALSE
)

# Test: Invalid Cocktail Name
test_that("ingredientsTable throws error for invalid cocktail name", {
  expect_error(ingredientsTable(cocktails, "mojito"))
})
