#' Update side ingredients content
#'
#' This function dynamically updates the content inside the side ingredient dropdowns
#' based on what alcohol was selected. It also prevents that a user can choose the
#' same side ingredient twice.
#' @param input Shiny input vector
#' @param session Shiny output vector
#' @param cocktails Cocktails dataframe
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
