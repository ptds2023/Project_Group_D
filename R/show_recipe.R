# show_recipe <- function(selected){
#   df <- cocktails %>%
#     filter(Name == selected)
#
#   type <- df$Type
#   glass <- df$Glass
#   recipe <- df$Recipe
#
#   ing_table <- df %>%
#     select(-c(Type, Category, Picture, Glass, Recipe)) %>%
#     pivot_longer(-Name, names_to = "ingredient", values_to = "quantity") %>%
#     drop_na() %>%
#     select(-Name)
#   return(ing_table)
# }
