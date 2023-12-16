#' Suprise me function
#'
#' This function detects when the suprise me button is clicked and assigns a random
#' cocktail to selected_cocktail()
#' @param input Shiny input vector
#' @param output Shiny output vector
#' @param session Shiny session
#' @param cocktails cocktails dataframe
#' @param selected_cocktail Selected row of specific cocktail
#'
#' @return Shows Cocktail Details page with random cocktail
addSurpriseMeObserver <- function(input, output, session, cocktails, selected_cocktail) {
  observeEvent(input$surpriseMeButton, {
    if (nrow(cocktails) > 0) {
      # Randomly select a cocktail
      random_cocktail <- cocktails[sample(nrow(cocktails), 1), ]
      selected_cocktail(random_cocktail)

      # Update the UI to show the cocktail details
      updateNavbarPage(session, "navbar", selected = "Cocktail Details")
    }
  })
}
