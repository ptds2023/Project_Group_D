#' Shiny app function
#'
#' @return Opens shiny app
#' @export
#'
#' @examples
#' cocktailApp()
cocktailApp <- function(){
  library(shiny)

  # Load your dataset
  cleaned_ingredient_names <- unique_ing
  # Define UI
  ui <- fluidPage(
    titlePanel("Cocktail Explorer"),

    navbarPage("Cocktails", id = "navbar",
               tabPanel("Cocktail List",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("ingredient1", "Alcohol", choices = c("Choose" = "", alcohol_vec), selected = "Choose", multiple = FALSE),
                            selectInput("ingredient2", "Side Ingredient 1", choices = c("Choose" = "", side_ing_vec), selected = "Choose", multiple = FALSE),
                            selectInput("ingredient3", "Side Ingredient 2", choices = c("Choose" = "", side_ing_vec), selected = "Choose", multiple = FALSE),
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
               )
    )
  )

  #server
  server <- function(input, output, session) {
    selected_cocktail <- reactiveVal(NULL)

    # filtered_data <- reactive({
    #   # Start with the full dataset
    #   result <- cocktails
    #
    #   # Apply filters based on selected ingredients
    #   colname1 <- input$ingredient1
    #   colname2 <- input$ingredient2
    #   colname3 <- input$ingredient3
    #
    #   if (!is.null(colname1) && colname1 != "") {
    #     result <- result[!is.na(result[[colname1]]), ]
    #   }
    #   if (!is.null(colname2) && colname2 != "") {
    #     result <- result[!is.na(result[[colname2]]), ]
    #   }
    #   if (!is.null(colname3) && colname3 != "") {
    #     result <- result[!is.na(result[[colname3]]), ]
    #   }
    #
    #   result
    # })

    filtered_data <- reactive({
      filtering_fun(cocktails, input$ingredient1, input$ingredient2, input$ingredient3)
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
      cocktail <- selected_cocktail()
      if (is.null(cocktail) || nrow(cocktail) == 0) return(NULL)

      # Extract ingredient names
      ingredient_names <- colnames(cocktail)[7:ncol(cocktail)]
      selected_ingredients <- ingredient_names[cocktail[1, 7:ncol(cocktail)] == TRUE]

      # Display detailed information
      tagList(
        h2(cocktail$Name[1]),
        img(src = cocktail$Picture[1], height = "200px"),
        h3("Category: ", cocktail$Category[1]),
        h4("Ingredients: ", paste(selected_ingredients, collapse = ", ")),
        h4("Type: ", cocktail$Type[1]),
        h4("Recipe: ", cocktail$Recipe[1])
      )
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
