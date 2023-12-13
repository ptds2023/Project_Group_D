#' Filtering for cocktails list
#'
#' @param df A dataframe
#' @param alcohol A string
#' @param ing1 A string
#' @param ing2 A string
#'
#' @return a filtered dataframe based on inputs
#' @export
#'
#' @examples
#' filtering_fun("vodka")
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
