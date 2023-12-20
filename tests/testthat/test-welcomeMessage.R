library(testthat)
library(htmltools)

# Define the expected HTML output
expected_message <- HTML(paste0("Welcome to the Cocktail Explorer App!", '<br/>','<br/>',
                                "Select your favorite alcohol and two ingredients, and let us surprise you with the best matching cocktails.", '<br/>',
                                "Feeling adventurous today? Click on the Surprise Me button, and we'll pick the best drinks for you!", '<br/>', '<br/>',
                                "You can also go to the instructions tab to learn more about the different features of the app."))

# Test
test_that("welcomeMessage returns the correct HTML message", {
  result_message <- welcomeMessage()
  expect_equal(as.character(result_message), as.character(expected_message))
})
