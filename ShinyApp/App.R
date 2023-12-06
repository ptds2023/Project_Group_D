library(shiny)
library(dplyr)

# Load your dataset
data <- read.csv("ShinyApp/first.csv")

# Convert NA to FALSE and any other value to TRUE for ingredients
data[,-(1:6)] <- !is.na(data[,-(1:6)])

# Clean up ingredient names by removing dots
cleaned_ingredient_names <- gsub("\\.", " ", colnames(data)[7:ncol(data)])

# Define UI
ui <- fluidPage(
  titlePanel("Cocktail Explorer"),

  navbarPage("Cocktails", id = "navbar",
             tabPanel("Cocktail List",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("ingredient1", "Ingredient 1", choices = c("Choose" = "", cleaned_ingredient_names), selected = "", multiple = FALSE),
                          selectInput("ingredient2", "Ingredient 2", choices = c("Choose" = "", cleaned_ingredient_names), selected = "", multiple = FALSE),
                          selectInput("ingredient3", "Ingredient 3", choices = c("Choose" = "", cleaned_ingredient_names), selected = "", multiple = FALSE),
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

# Define server logic
server <- function(input, output, session) {
  selected_cocktail <- reactiveVal(NULL)

  filtered_data <- reactive({
    # Start with the full dataset
    result <- data

    # Helper function to get the original column name from cleaned name
    get_original_colname <- function(cleaned_name) {
      if(cleaned_name != "Choose" && cleaned_name != "") {
        return(colnames(data)[7:ncol(data)][cleaned_ingredient_names == cleaned_name])
      }
      return(NULL)
    }

    # Apply filters based on selected ingredients
    colname1 <- get_original_colname(input$ingredient1)
    colname2 <- get_original_colname(input$ingredient2)
    colname3 <- get_original_colname(input$ingredient3)

    if (!is.null(colname1) && colname1 != "") {
      result <- result[result[[colname1]] == TRUE, ]
    }
    if (!is.null(colname2) && colname2 != "") {
      result <- result[result[[colname2]] == TRUE, ]
    }
    if (!is.null(colname3) && colname3 != "") {
      result <- result[result[[colname3]] == TRUE, ]
    }

    result
  })

  output$cocktailList <- renderUI({
    # Get the filtered data
    sub_data <- filtered_data()

    # Create a list of clickable cocktail names with pictures
    cocktailList <- lapply(1:nrow(sub_data), function(i) {
      wellPanel(
        actionLink(inputId = paste("cocktail_click", i, sep = "_"), label = sub_data$Name[i]),
        img(src = sub_data$Picture[i], height = "200px")
      )
    })

    do.call(tagList, cocktailList)
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


