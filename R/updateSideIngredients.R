#' Update side ingredients content
#'
#' This internal function dynamically updates the content inside the side ingredient dropdowns
#' based on what alcohol and/or side ingredient 1 were selected. It also prevents a user from being able to choose the
#' same side ingredient twice.
#' @param input Shiny input vector from current session
#' @param session Shiny output vector from current session
#' @param cocktails Cocktails dataframe from the package
#'
#' @return Updated dropdown menus
updateSideIngredients <- function(input, session, cocktails) {
  observeEvent(input$ingredient1, {
    if (input$ingredient1 != "" && input$ingredient1 != "Choose") {
      compatible_cocktails <- cocktails[!is.na(cocktails[[input$ingredient1]]), ]
      ingredient_columns <- colnames(cocktails)[7:ncol(cocktails)]
      non_na_ingredients <- ingredient_columns[colSums(!is.na(compatible_cocktails[ingredient_columns])) > 0]
      non_na_ingredients <- setdiff(non_na_ingredients, input$ingredient1)
      updateSelectInput(session, "ingredient2", choices = c("Choose" = "", sort(non_na_ingredients)))
    } else {
      updateSelectInput(session, "ingredient2", choices = c("Choose" = ""))
    }
  })

  observeEvent(input$ingredient2, {
    if (input$ingredient2 != "" && input$ingredient2 != "Choose") {
      compatible_cocktails <- cocktails[!is.na(cocktails[[input$ingredient1]]) & !is.na(cocktails[[input$ingredient2]]), ]
      ingredient_columns <- colnames(cocktails)[7:ncol(cocktails)]
      non_na_ingredients <- ingredient_columns[colSums(!is.na(compatible_cocktails[ingredient_columns])) > 0]
      non_na_ingredients <- setdiff(non_na_ingredients, c(input$ingredient1, input$ingredient2))
      updateSelectInput(session, "ingredient3", choices = c("Choose" = "", sort(non_na_ingredients)))
    } else {
      updateSelectInput(session, "ingredient3", choices = c("Choose" = ""))
    }
  })
}
