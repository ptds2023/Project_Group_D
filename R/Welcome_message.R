#' Welcome message
#'
#' @return A welcome message in a popup in shiny
welcome_message <- function() {
  message <- "Welcome to the Cocktail Explorer App! \n\n Select your favorite alcohol and two ingredients, and let us surprise you with the best matching cocktails.\n\nFeeling adventurous today? Click on the Surprise Me button, and we'll pick the best drinks for you!"

  return(message)
}
