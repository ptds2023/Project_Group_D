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
