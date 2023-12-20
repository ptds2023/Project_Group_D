library(testthat)
#library(shiny)

# Test if the cocktailApp function launches the app without errors
test_that("cocktailApp launches without errors", {
  expect_error(cocktailApp(), NA)
  # Use stopApp() if needed to ensure the app does not remain open
  shiny::stopApp()
})
