#' Render Ingredient-Quantity Table
#'
#' The \code{ingredientsTable} function takes as input the cocktails dataframe and the
#' name of a cocktail in that dataframe and returns its ingredients along with quantities.
#' @param df The cocktails dataframe from the package
#' @param selected_cocktail A cocktail name string
#'
#' @return An HTML table with ingredients and corresponding quantities
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' ingredientsTable(cocktails, "daiquiri")
ingredientsTable <- function(df = cocktails, selected_cocktail) {
  # Check if the selected cocktail is not in the df
  if(!(selected_cocktail %in% df$Name)){
    stop("This cocktail is not in the dataframe")
  }
  #creating table
  ing_table <- df %>%
    dplyr::filter(.data$Name == selected_cocktail) %>%
    dplyr::select(-c(.data$Type, .data$Category, .data$Picture, .data$Glass, .data$Recipe)) %>%
    tidyr::pivot_longer(-.data$Name, names_to = "Ingredient", values_to = "Quantity", values_drop_na = TRUE) %>%
    dplyr::select(-.data$Name) %>%
    kableExtra::kbl() %>%
    kableExtra::kable_styling("striped")

  return(ing_table)
}
