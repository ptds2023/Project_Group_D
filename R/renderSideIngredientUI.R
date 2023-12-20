#' Dropdown rendering
#'
#' This internal function dynamically renders the dropdown menus in the Shiny app UI
#' @param input Shiny input from current session
#' @param output Shiny output from current session
#'
#' @return Dynamically created dropdown menus
renderSideIngredientUI <- function(input, output) {
  output$sideIngredient1Dropdown <- renderUI({
    if (!is.na(input$ingredient1) && input$ingredient1 != "" && input$ingredient1 != "Choose") {
      selectInput("ingredient2", "Side Ingredient 1", choices = c("Choose" = ""), selected = "Choose", multiple = FALSE)
    }
  })

  output$sideIngredient2Dropdown <- renderUI({
    if (isTruthy(input$ingredient2) && input$ingredient2 != "Choose") {
      selectInput("ingredient3", "Side Ingredient 2", choices = c("Choose" = ""), selected = "Choose", multiple = FALSE)
    } else {
      return(NULL)  # Return NULL if the condition is not met
    }
  })
}
