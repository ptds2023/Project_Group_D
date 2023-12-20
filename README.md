---
editor_options: 
  markdown: 
    wrap: 72
---

# Do you fancy a drink ?

<img src="man/figures/cocktail.png" width="500"/>

## Find Your Perfect Mix!

Welcome to the CocktailSelector, an R package and Shiny app designed to
help cocktail lovers and bartenders discover new and exciting drink
recipes. Our intuitive tool simplifies the process of finding drinks
based on your preferred alcohol and ingredients.

## The Objective of Cocktail Selector

### Explicit Functions:

-   `cocktailApp`: Launches the Shiny app, providing an interactive
    platform for exploring cocktail recipes.

-   `convertUnits`: A versatile function to convert measurement units
    (volume, weight, length) between imperial and international systems.

### Implicit Functions:

-   `filterCocktails`: Filters the cocktail list based on chosen alcohol
    and ingredients, offering personalized drink suggestions.

-   `ingredientsTable`: Generates a table displaying ingredients and
    quantities for selected cocktails.

-   `welcomeMessage`: Displays an engaging welcome message to users when
    the app starts, enhancing the overall user experience.

### Why Cocktail Selector?

Choosing the right drink from countless options can be overwhelming. The
Cocktail Selector is here to make that choice easier. Whether you're
hosting a party, exploring the world of mixology, or simply feeling
adventurous, our package provides a user-friendly interface to explore
and discover up to 600 drink recipes that match your tastes.

### Installation

You can install the development version of CocktailSelector from Github
with:

```{r}
#install.packages("devtools")
devtools::install_github("ptds2023/Project_Group_D")
```

### Dependencies

The package depends on the **Shiny**, **htmltools**, and **magrittr**
packages, which are automatically imported and loaded when installing
CocktailSelector.

### Example

This is how you may load the package:

```{r}
# Loading package
library(CocktailSelector)
```

In the following sections, you may find the function that changes the
unit from imperial to international system, or from international to
imperial. The output is the dataset with the transformed measurements
and corresponding chosen units.

```{r}
# Example dataframe with cocktail ingredients and their measurements
cocktail_data <- data.frame(
    col1 = rep(NA, 4),
    col2 = rep(NA, 4),
    col3 = rep(NA, 4),
    col4 = rep(NA, 4),
    col5 = rep(NA, 4),
    col6 = rep(NA, 4),
    units = c("1 inch", "2.5 lb", "3 cm", "0.25 oz")
  )

# Converting from imperial (ounces) to international (milliliters) units
converted_data_international <- convertUnits(cocktail_data, "imperial_to_international")

# Displaying the converted data
print(converted_data_international)

```

|       | **col1** | **col2** | **col3** | **col4** | **col5** | **col6** | **units** |
|:------|:---------|:---------|:---------|:---------|:---------|:---------|:----------|
| **1** | NA       | NA       | NA       | NA       | NA       | NA       | 2.5 cm    |
| **2** | NA       | NA       | NA       | NA       | NA       | NA       | 1.1 kg    |
| **3** | NA       | NA       | NA       | NA       | NA       | NA       | 3 cm      |
| **4** | NA       | NA       | NA       | NA       | NA       | NA       | 10 ml     |

This example demonstrates the conversion of ingredient quantities from
ounces to milliliters, a common requirement in cocktail
preparation.Adjust the dataframe to match your specific needs and
explore the flexibility of the CocktailSelector package.
