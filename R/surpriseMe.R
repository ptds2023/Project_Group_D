#' Surprise Me
#'
#' The function can be used to randomly select a cocktail from the dataframe.
#' The output can the be used in the \code{ingredientsTable} function in order
#' to get the ingredients and quantities needed for the cocktail.
#' @return A randomly selected cocktail within the \code{cocktails} dataframe
#' @export
#'
#' @examples
#' #using it on its own
#' surpriseMe()
#' #using it with ingredientsTable
#' ingredientsTable(selected_cocktail = surpriseMe()$Name)
surpriseMe <- function(){
  return(cocktails[sample(nrow(cocktails), 1), ])
}
