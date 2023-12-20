#testing that it returns a cocktail name within the dataframe
test_that("give a cocktail that is in the cocktails dataframe", {
  expect_true(surpriseMe()$Name %in% cocktails$Name)
})
