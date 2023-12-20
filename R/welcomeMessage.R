#' Welcome message
#'
#' This internal function is called when the app is loaded and shows the welcome message in a popup.
#' @return A welcome message in a popup in shiny
welcomeMessage <- function() {
  message <- HTML(paste0("Welcome to the Cocktail Explorer App!", '<br/>','<br/>',
                   "Select your favorite alcohol and two ingredients, and let us surprise you with the best matching cocktails.", '<br/>',
                   "Feeling adventurous today? Click on the Surprise Me button, and we'll pick the best drinks for you!", '<br/>', '<br/>',
                   "You can also go to the instructions tab to learn more about the different features of the app.")
                  )
  return(message)
}
