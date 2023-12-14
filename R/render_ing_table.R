#' Render Ingredient-Quantity Table
#'
#' @param selected_cocktail A cocktail name string
#'
#' @return An html table with ingredients and corresponding quantities
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' render_ing_table("daiquiri")
render_ing_table <- function(selected_cocktail){
  ing_table <- cocktails %>%
    dplyr::filter(.data$Name == selected_cocktail) %>%
    dplyr::select(-c(.data$Type, .data$Category, .data$Picture, .data$Glass, .data$Recipe)) %>%
    tidyr::pivot_longer(-.data$Name, names_to = "Ingredient", values_to = "Quantity", values_drop_na = TRUE) %>%
    dplyr::select(-.data$Name)
  return(xtable::xtable(ing_table))
}
