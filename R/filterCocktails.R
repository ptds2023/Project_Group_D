#' Filtering cocktails list
#'
#' The \code{filterCocktails} function is used to create the list of
#' drinks based on inputs from the dropdowns.
#' It filters the datasets based on one alcohol and two side ingredients
#' and returns a filtered dataframe with drinks corresponding to those criteria.
#'
#' @param df The cocktails dataframe included in the package
#' @param alcohol A string which is in the alcohol_Vec vector
#' @param ing1 A string which is in the side_ing_vec vector
#' @param ing2 A string which is in the side_ing_vec vector
#'
#' @return A filtered drinks dataframe based on inputs
#' @export
#'
#' @examples
#' \dontrun{filterCocktails(cocktails, "vodka", "cranberry juice")}
filterCocktails <- function(df, alcohol = NULL, ing1 = NULL, ing2 = NULL){
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
