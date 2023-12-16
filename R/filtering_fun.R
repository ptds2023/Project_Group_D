#' Filtering for cocktails list
#'
#' The `filtering_fun` function is used to create the list of
#' drinks based on inputs from the dropdowns.
#' It filters the datasets based on on alcohol and two side ingredients
#' an returns a filtered dataframe with drink correspinding to those critieria.
#'
#' @param df A dataframe
#' @param alcohol A string
#' @param ing1 A string
#' @param ing2 A string
#'
#' @return a filtered drinks dataframe based on inputs
#' @export
#'
#' @examples
#' filtering_fun(cocktails, "gin")
filtering_fun <- function(df, alcohol = NULL, ing1 = NULL, ing2 = NULL){
  result <- df
    if (!is.null(alcohol) && alcohol!="") {
      result <- result[!is.na(result[[alcohol]]), ]
    }
    if (!is.null(ing1) && ing1!="") {
      result <- result[!is.na(result[[ing1]]), ]
    }
    if (!is.null(ing2) && ing2!="") {
      result <- result[!is.na(result[[ing2]]), ]
    }
  return(result)
}
