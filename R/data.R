#' Drinks dataset with ingredients
#'
#' A dataset which contains drink names, images and ingredients information.
#'
#' @format \code{cocktails}
#' A data frame with 534 rows and 310 columns:
#' \describe{
#'   \item{Name}{Drink name}
#'   \item{Type}{Type of drink (i.e., alcoholic, non-alcoholic, etc.)}
#'   \item{Category}{General category of the drink (cocktail, beer, shot)}
#'   \item{Picture}{URL to the picture of the drink}
#'   \item{Glass}{Type of glass recommended for the drink}
#'   \item{Recipe}{Text description of the recipe to make the drink}
#'   \item{304 ingredients}{All other columns which contain the amoung of each ingredients if present in the drink, and NAs if not}
#' }
#' @source <https://www.kaggle.com/datasets/ai-first/cocktail-ingredients>
"cocktails"
