# Sample 'df' dataframe
df <- data.frame(
  Name = c("daiquiri", "margarita"),
  Ingredient1 = c("Rum", "Tequila"),
  Quantity1 = c("50ml", "60ml"),
  Ingredient2 = c("Lime Juice", "Triple Sec"),
  Quantity2 = c("20ml", "30ml")
)

# Test: Invalid Cocktail Name
test_that("ingredientsTable throws error for invalid cocktail name", {
  expect_error(ingredientsTable(df, "mojito"))
})

test_that("ingredientsTable returns expected table for a specific cocktail", {
  result <- ingredientsTable(cocktails, "a true amaretto sour")
  expected <- data.frame(Ingredient = c("amaretto", "lemon"), Quantity = c("1 jigger", "0.5 juice of"))
  expect_equal(result,
               kableExtra::kbl(expected) %>% kableExtra::kable_styling("striped"))
})
