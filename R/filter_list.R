# filter_list <- function(alcohol = NULL, ing1 =NULL, ing2 = NULL){
#   colnames(cocktails) <- colnames(cocktails) %>% str_trim()
#   #req(input$alcohol, input$ing1, input$ing2)
#   if(!all(c(alcohol, ing1, ing2) %in% colnames(cocktails))){
#     stop(cat("Invalid Input"))
#   }
#
#   if(is.null(alcohol)){
#     cat("The base alcohol selection is required")
#   }else{
#     if(is.null(ing1) && is.null(ing2)){
#       df <- cocktails %>%
#         dplyr::filter(!is.na(.[[alcohol]]))
#     }else if(is.null(ing1) && !is.null(ing2)){
#       df <- cocktails %>%
#         dplyr::filter(!is.na(.[[alcohol]]), !is.na(.[[ing2]]))
#     }else if(!is.null(ing1) && is.null(ing2)){
#       df <- cocktails %>%
#         dplyr::filter(!is.na(.[[alcohol]]), !is.na(.[[ing1]]))
#     }else{
#       df <- cocktails %>%
#         dplyr::filter(!is.na(.[[alcohol]]), !is.na(.[[ing1]]), !is.na(.[[ing2]]))
#     }
#   }
#   return(df %>% select(Name))# %>% kableExtra::kbl())
# }
