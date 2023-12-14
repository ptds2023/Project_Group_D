#' Shiny app function
#'
#' @return Opens shiny app
#' @import shiny
#' @import htmltools
#' @importFrom magrittr %>%
#' @export
cocktailApp <- function(){
  # Define UI
  ui <- fluidPage(
    titlePanel("Cocktail Explorer"),

    navbarPage("Cocktails", id = "navbar",
               tabPanel("Cocktail List",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("ingredient1", "Alcohol", choices = c("Choose" = "", alcohol_vec), selected = "Choose", multiple = FALSE),
                            uiOutput("sideIngredient1Dropdown"),
                            uiOutput("sideIngredient2Dropdown"),
                            actionButton("clearButton", "Clear Ingredients")
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

    #first output
    output$cocktailList <- renderUI({
      # Get the filtered data
      sub_data <- filtered_data()

      if(nrow(sub_data) == 0){
        HTML("<p>No cocktail found</p>")
      }else{
        # Create a list of clickable cocktail names with pictures
        cocktailList <- lapply(1:nrow(sub_data), function(i) {
          wellPanel(
            actionLink(inputId = paste("cocktail_click", i, sep = "_"), label = sub_data$Name[i]),
            img(src = sub_data$Picture[i], height = "200px")
          )
        })
        do.call(tagList, cocktailList)
      }
    })

    observe({
      for (i in 1:nrow(filtered_data())) {
        local({
          local_i <- i
          observeEvent(input[[paste("cocktail_click", local_i, sep = "_")]], {
            selected_cocktail(filtered_data()[local_i, ])
            updateNavbarPage(session, "navbar", selected = "Cocktail Details")
          })
        })
      }
    })

    #second output
    output$cocktailDetails <- renderUI({
      # cocktail <- selected_cocktail()
      # if (is.null(cocktail) || nrow(cocktail) == 0) return(NULL)
      #
      # # Extract ingredient names
      # ingredient_names <- colnames(cocktail)[7:ncol(cocktail)]
      # selected_ingredients <- ingredient_names[!is.na(cocktail[1, 7:ncol(cocktail)])]
      #
      # # Display detailed information
      # renderTable(
      #   xtable::xtable(as.data.frame(selected_ingredients))
      # )
      # tagList(
      #   h2(cocktail$Name[1]),
      #   img(src = cocktail$Picture[1], height = "200px"),
      #   h3("Category: ", cocktail$Category[1]),
      #   h4("Ingredients: ", paste(selected_ingredients, collapse = ", ")),
      #   h4("Type: ", cocktail$Type[1]),
      #   h4("Recipe: ", cocktail$Recipe[1])
      # )

      output$cocktailDetails <- renderUI({
        #selecting cocktail which the user clicked on
        cocktail <- selected_cocktail()
        if (is.null(cocktail) || nrow(cocktail) == 0) return(NULL)

        #creating multiple elements to display
        title <- h2(cocktail$Name)
        image <- tags$img(src = cocktail$Picture, height = "200px")
        category <- h3(paste("Category:", cocktail$Category))
        recipe <- h4("Recipe: ", cocktail$Recipe)
        ingredients_table_html <- tableOutput("ingredientsTable")

        #arrange elements in layout
        tagList(title, image, category, ingredients_table_html, recipe)
      })

      output$ingredientsTable <- renderTable({
        #setting cocktail variable
        cocktail <- selected_cocktail()
        cocktail_name <- cocktail$Name
        #calling package function to render table
        render_ing_table(cocktail_name)
      })
    })

    selected_cocktail <- reactiveVal(NULL)

    # Call the function to update side ingredients
    updateSideIngredients(input, session, cocktails)

    # Call the function to render dynamic UI
    renderSideIngredientUI(input, output)

    filtered_data <- reactive({
      filtering_fun(cocktails, input$ingredient1, input$ingredient2, input$ingredient3)
    })



    # Clear button logic
    observeEvent(input$clearButton, {
      updateSelectInput(session, "ingredient1", selected = "")
      updateSelectInput(session, "ingredient2", selected = "")
      updateSelectInput(session, "ingredient3", selected = "")
    })


  }


  # Run the application
  shinyApp(ui = ui, server = server)
}
