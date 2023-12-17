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
  # Define UI
  ui <- fluidPage(
    theme = shinythemes::shinytheme("superhero"),
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
      welcome_message()
    })

    filtered_data <- reactive({
      filtering_fun(cocktails, input$ingredient1, input$ingredient2, input$ingredient3)
    })
    #first output
    output$cocktailList <- renderUI({
      # Get the filtered data
      sub_data <- filtered_data()

      #checking if at least one drink matches the description
      if(nrow(sub_data) == 0){
        HTML("<p>No cocktail found</p>")
      }else{
        # Create a list of clickable cocktail names with pictures, arranged side by side
        cocktailList <- lapply(1:nrow(sub_data), function(i) {
          wellPanel(
            fluidRow(
              column(6, div(style = "display: flex; align-items: center; height: 200px;",
                            actionLink(inputId = paste("cocktail_click", i, sep = "_"), label = sub_data$Name[i]))),
              column(6, img(src = sub_data$Picture[i], height = "200px"))
            )
          )
        })
        do.call(tagList, cocktailList)
      }
    })

    selected_cocktail <- reactiveVal(NULL)
    show_details <- reactiveVal(FALSE)

    observe({
      for (i in 1:nrow(filtered_data())) {
        local({
          local_i <- i
          observeEvent(input[[paste("cocktail_click", local_i, sep = "_")]], {
            selected_cocktail(filtered_data()[local_i, ])
            if(input$radio_units == "int"){
              selected_cocktail(convertUnits(selected_cocktail(), "imperial_to_international"))
            }else if(input$radio_units == "imp"){
              selected_cocktail(convertUnits(selected_cocktail(), "international_to_imperial"))
            }
            show_details(TRUE)
            updateNavbarPage(session, "navbar", selected = "Cocktail Details")
          })
        })
      }
    })
    # showdetails <- reactiveVal(NULL)
    # #Function to reset cocktail details content
    # observeEvent(input$navbar, {
    #   if (input$navbar == "Cocktail List") {
    #     showdetails(NULL)
    #   }
    # })
    # observeEvent(input$navbar, {
    #   if (input$navbar == "Cocktail Details") {
    #     showdetails(10L)
    #   }
    # })
    #second output
    output$cocktailDetails <- renderUI({
      # if (is.null(showdetails())) {
      #   p("Click on a cocktail in the Cocktails List tab to see the details")
      # } else {
        #selecting cocktail which the user clicked on
      if (!show_details()) return(NULL)
      cocktail <- selected_cocktail()
      if (is.null(cocktail) || nrow(cocktail) == 0) return(NULL)
      #creating multiple elements to display
      title <- h2(cocktail$Name)
      image <- tags$img(src = cocktail$Picture, height = "200px")
      category <- h3(paste("Category:", cocktail$Category))
      glass <- h4("Glass: ", cocktail$Glass)
      ingredients_table_html <- tableOutput("ingredientsTable")
      recipe <- h4("Recipe: ", cocktail$Recipe)

      #arrange elements in layout
      tagList(title, image, category, glass, ingredients_table_html, recipe)
      # }
    })
    #rendering ingredient-quantity table
    output$ingredientsTable <- renderTable({
      #setting cocktail variable
      df <- selected_cocktail()
      cocktail_name <- df$Name
      #calling package function to render table
      render_ing_table(df, cocktail_name)
    })


    # Call the function to update side ingredients
    updateSideIngredients(input, session, cocktails)

    # Call the function to render dynamic UI
    renderSideIngredientUI(input, output)


    #addSurpriseMeObserver(input, output, session, cocktails, selected_cocktail)
    observeEvent(input$surpriseMeButton, {
      if (nrow(cocktails) > 0) {
        # Randomly select a cocktail
        random_cocktail <- cocktails[sample(nrow(cocktails), 1), ]
        selected_cocktail(random_cocktail)
        if(input$radio_units == "int"){
          selected_cocktail(convertUnits(selected_cocktail(), "imperial_to_international"))
        }else if(input$radio_units == "imp"){
          selected_cocktail(convertUnits(selected_cocktail(), "international_to_imperial"))
        }
        show_details(TRUE)

        # Update the UI to show the cocktail details
        updateNavbarPage(session, "navbar", selected = "Cocktail Details")
      }
    })
    # Clear button logic
    observeEvent(input$clearButton, {
      updateSelectInput(session, "ingredient1", selected = "")
      updateSelectInput(session, "ingredient2", selected = "")
      updateSelectInput(session, "ingredient3", selected = "")
    })

    observeEvent(input$navbar, {
      if (input$navbar != "Cocktail Details") {
        show_details(FALSE)
      }
    })
  ### end of server
  }


  # Run the application
  shinyApp(ui = ui, server = server)
}
