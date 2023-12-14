renderSideIngredientUI <- function(input, output) {
  output$sideIngredient1Dropdown <- renderUI({
    if (!is.na(input$ingredient1) && input$ingredient1 != "" && input$ingredient1 != "Choose") {
      selectInput("ingredient2", "Side Ingredient 1", choices = c("Choose" = ""))
    }
  })

  output$sideIngredient2Dropdown <- renderUI({
    if (isTruthy(input$ingredient2) && input$ingredient2 != "Choose") {
      selectInput("ingredient3", "Side Ingredient 2", choices = c("Choose" = ""))
    } else {
      return(NULL)  # Return NULL if the condition is not met
    }
  })
}
