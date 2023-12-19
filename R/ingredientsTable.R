#' Render Ingredient-Quantity Table
#'
#' The ingredientsTable function takes as input the cocktails dataframe and the
#' name of a cocktail in that dataframe and returns its ingredients along with quantities.
#' @param selected_cocktail A cocktail name string
#' @param df The cocktails dataframe
#'
#' @return An html table with ingredients and corresponding quantities
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' ingredientsTable(cocktails, "daiquiri")
ingredientsTable <- function(df, selected_cocktail) {
  # Check if df is NULL or empty, or if the selected cocktail is not in the df
  if (is.null(df) || nrow(df) == 0 || !(selected_cocktail %in% df$Name)) {
    return(xtable::xtable(data.frame(Ingredient = NA, Quantity = NA)))
  }
  #creating table
  ing_table <- df %>%
    dplyr::filter(.data$Name == selected_cocktail) %>%
    dplyr::select(-c(.data$Type, .data$Category, .data$Picture, .data$Glass, .data$Recipe)) %>%
    tidyr::pivot_longer(-.data$Name, names_to = "Ingredient", values_to = "Quantity", values_drop_na = TRUE) %>%
    dplyr::select(-.data$Name)

  return(xtable::xtable(ing_table))
}
