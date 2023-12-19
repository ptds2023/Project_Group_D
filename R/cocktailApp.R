#' Shiny app function
#'
#' The `cocktailApp` function launches the shiny app directly from the package.
#' @return Opens shiny app
#' @import shiny
#' @import htmltools
#' @import shinythemes
#' @importFrom magrittr %>%
#' @export

cocktailApp <- function(){
  pastel_green_color <- "#78c1ad"
  # Define UI
  ui <- fluidPage(
    theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
    tags$head(
      tags$style(HTML(paste0("
            .navbar .navbar-nav .nav-link {
                color: ", pastel_green_color, " !important;
            }
            .navbar .navbar-nav .nav-link:hover,
            .navbar .navbar-nav .nav-link:focus {
                color: darken(", pastel_green_color, ", 10%) !important;
            }
            #clearButton, #surpriseMeButton {
                background-color: ", pastel_green_color, " !important;
                border-color: ", pastel_green_color, " !important;
                color: #fff; /* Adjust text color if needed */
                margin-top: 10px;
            }
            #clearButton:hover, #surpriseMeButton:hover {
                background-color: darken(", pastel_green_color, ", 10%) !important;
                border-color: darken(", pastel_green_color, ", 10%) !important;
            }
        ")))
    ),
    titlePanel("Cocktail Explorer"),
    navbarPage("Cocktails", id = "navbar",
               tabPanel("Cocktail List",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("ingredient1", "Alcohol", choices = c("Choose" = "", alcohol_vec), selected = "Choose", multiple = FALSE),
                            uiOutput("sideIngredient1Dropdown"),
                            uiOutput("sideIngredient2Dropdown"),
                            actionButton("clearButton", "Clear Ingredients"),
                            actionButton("surpriseMeButton", "Surprise Me", icon = icon("random")),
                            radioButtons("radio_units", "Measurement Units",
                                         c("International" = "int",
                                           "Imperial" = "imp")
                            )
                          ),
                          mainPanel(
                            uiOutput("cocktailList")
                          )
                        )
               ),
               tabPanel("Cocktail Details",
                        mainPanel(
                          uiOutput("cocktailDetails")
                        )
               ),
               tabPanel("Information",
                        mainPanel(
                          h3("How to Use the Cocktail Explorer"),
                          p("Welcome to the Cocktail Explorer! Here's how you can use this app:"),
                          tags$ul(
                            tags$li("Select an alcohol type from the 'Alcohol' dropdown menu."),
                            tags$li("Based on your selection, the 'Side Ingredient 1' dropdown will be updated with compatible ingredients."),
                            tags$li("After selecting a side ingredient, the 'Side Ingredient 2' dropdown will be updated."),
                            tags$li("The app will display a list of cocktails based on your selections."),
                            tags$li("Click on any cocktail to view its details, including recipe and ingredients."),
                            tags$li("Use the 'Clear Ingredients' button to reset your selections at any time.")
                          ),
                          p("Enjoy exploring and discovering new cocktails!")
                        )
               )
    )
  )


  #server
  server <- function(input, output, session) {

    # Show welcome message when the app starts
    observe({
      showModal(modalDialog(
        title = "Welcome!",
        textOutput("welcomeText"),
        easyClose = TRUE,
        footer = NULL
      ))
    })

    # Render the welcome text
    output$welcomeText <- renderText({
      welcomeMessage()
    })


    filtered_data <- reactive({
      filterCocktails(cocktails, input$ingredient1, input$ingredient2, input$ingredient3)
    })

    # First output: List of cocktails
    output$cocktailList <- renderUI({
      sub_data <- filtered_data()
      if (nrow(sub_data) == 0) {
        HTML("<p>No cocktail found</p>")
      } else {
        cocktailList <- lapply(1:nrow(sub_data), function(i) {
          wellPanel(
            fluidRow(
              column(6, div(style = "display: flex; align-items: center; height: 200px;",
                            actionLink(inputId = paste("cocktail_click", i, sep = "_"), label = sub_data$Name[i])
              )
              ),
              column(6, img(src = sub_data$Picture[i], height = "200px"))
            )
          )
        })
        do.call(tagList, cocktailList)
      }
    })



    selected_cocktail <- reactiveVal(NULL)
    show_details <- reactiveVal(FALSE)

    # Reset selected cocktail and show details flag when the first ingredient changes
    # observeEvent(input$ingredient1, {
    #   selected_cocktail(NULL)
    #   show_details(FALSE)
    # }, ignoreInit = TRUE)

    # # Reset cocktail details when the 'Cocktail List' tab is clicked
    # observeEvent(input$navbar, {
    #   if (input$navbar == "Cocktail List") {
    #     selected_cocktail(NULL)
    #     show_details(FALSE)
    #   }
    # })

    observe({
      lapply(1:nrow(filtered_data()), function(i) {
        observeEvent(input[[paste("cocktail_click", i, sep = "_")]], {
          selected_cocktail(filtered_data()[i, ])
          updateUnitsBasedOnRadioSelection()
          show_details(TRUE)
          updateNavbarPage(session, "navbar", selected = "Cocktail Details")
        })
      })
    })

    # updated_selected <- function(i){
    #   selected_cocktail(filtered_data()[i, ])
    #   updateUnitsBasedOnRadioSelection()
    #   show_details(TRUE)
    #   updateNavbarPage(session, "navbar", selected = "Cocktail Details")
    # }

    # Handle "Surprise Me" button
    observeEvent(input$surpriseMeButton, {
      if (nrow(cocktails) > 0) {
        random_cocktail <- cocktails[sample(nrow(cocktails), 1), ]
        selected_cocktail(random_cocktail)
        updateUnitsBasedOnRadioSelection()
        show_details(TRUE)
        updateNavbarPage(session, "navbar", selected = "Cocktail Details")
      }
    })

    # Second output: Cocktail details
    output$cocktailDetails <- renderUI({
      # Check if the details should be shown and if a cocktail has been selected
      if (!show_details() || is.null(selected_cocktail()) || nrow(selected_cocktail()) == 0) {
        return(h4("Please select a cocktail from the Cocktail List tab to see the details."))
      }

      # Proceed with displaying the selected cocktail details
      cocktail <- selected_cocktail()
      if (is.null(cocktail) || nrow(cocktail) == 0) return(NULL)

      # Creating multiple elements to display
      title <- h2(cocktail$Name)
      image <- tags$img(src = cocktail$Picture, height = "200px")
      category <- h3(paste("Category:", cocktail$Category))
      glass <- h4("Glass: ", cocktail$Glass)
      ingredients_table_html <- tableOutput("ingredientsTable")
      recipe <- h4("Recipe: ", cocktail$Recipe)

      # Arrange elements in layout
      tagList(title, image, category, glass, ingredients_table_html, recipe)
    })

    # Render ingredient-quantity table
    output$ingredientsTable <- renderTable({
      df <- selected_cocktail()
      cocktail_name <- df$Name
      ingredientsTable(df, cocktail_name)
    })

    # Call the function to update side ingredients
    updateSideIngredients(input, session, cocktails)

    # Call the function to render dynamic UI
    renderSideIngredientUI(input, output)

    # Clear button logic
    observeEvent(input$clearButton, {
      updateSelectInput(session, "ingredient1", selected = "")
      updateSelectInput(session, "ingredient2", selected = "")
      updateSelectInput(session, "ingredient3", selected = "")
    })

    observeEvent(input$navbar, {
      if (input$navbar != "Cocktail Details") {
        selected_cocktail(NULL)
        show_details(FALSE)
      }
    })

    # Utility function to update units based on radio selection
    updateUnitsBasedOnRadioSelection <- function() {
      if (input$radio_units == "int") {
        selected_cocktail(convertUnits(selected_cocktail(), "imperial_to_international"))
      } else if (input$radio_units == "imp") {
        selected_cocktail(convertUnits(selected_cocktail(), "international_to_imperial"))
      }
    }
  }

  ### end of server


  # Run the application
  shinyApp(ui = ui, server = server)
}
