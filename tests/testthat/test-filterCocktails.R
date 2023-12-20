# Import library
library(testthat)

# Sample dataframe for testing
df <- data.frame(
  alcohol = c("Rum", "Vodka", NA, "Whiskey", NA),
  ing1 = c("Lime", NA, "Orange", NA, "Lemon"),
  ing2 = c(NA, "Apple", "Grape", NA, "Mango")
)

# Test 1: Filter by Alcohol
test_that("filterCocktails correctly filters by alcohol", {
  result <- filterCocktails(df, alcohol = "alcohol")
  expected <- df[!is.na(df$alcohol), ]
  expect_equal(result, expected)
})

# Test 2: Filter by First Ingredient (ing1)
test_that("filterCocktails correctly filters by first ingredient", {
  result <- filterCocktails(df, ing1 = "ing1")
  expected <- df[!is.na(df$ing1), ]
  expect_equal(result, expected)
})

# Test 3: Filter by Second Ingredient (ing2)
test_that("filterCocktails correctly filters by second ingredient", {
  result <- filterCocktails(df, ing2 = "ing2")
  expected <- df[!is.na(df$ing2), ]
  expect_equal(result, expected)
})

# Test 4: Combined Filtering
test_that("filterCocktails correctly filters with combined parameters", {
  result <- filterCocktails(df, alcohol = "alcohol", ing1 = "ing1", ing2 = "ing2")
  expected <- df[!is.na(df$alcohol) & !is.na(df$ing1) & !is.na(df$ing2), ]
  expect_equal(result, expected)
})

# Test 5: Handling NULLs and Empty Strings
test_that("filterCocktails handles NULLs and empty strings correctly", {
  result <- filterCocktails(df, alcohol = NULL, ing1 = "", ing2 = NULL)
  expect_equal(result, df)
})
