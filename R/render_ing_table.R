render_ing_table <- function(selected_cocktail){
  ing_table <- selected_cocktail %>%
    dplyr::select(-c(Type, Category, Picture, Glass, Recipe)) %>%
    tidyr::pivot_longer(-Name, names_to = "Ingredient", values_to = "Quantity", values_drop_na = TRUE) %>%
    dplyr::select(-Name)
  return(xtable::xtable(ing_table))
}
