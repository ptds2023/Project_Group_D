
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CocktailSelector

<!-- badges: start -->

[![R-CMD-check](https://github.com/ptds2023/Project_Group_D/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/ptds2023/Project_Group_D/actions/workflows/R-CMD-check.yml)
<!-- badges: end -->

# Do you fancy a drink ?

<img src="man/figures/cocktail.png" width="500"/>

## Find Your Perfect Mix!

Welcome to the CocktailSelector, an R package and Shiny app designed to
help cocktail lovers and bartenders discover new and exciting drink
recipes. Our intuitive tool simplifies the process of finding drinks
based on your preferred alcohol and ingredients.

## The Objective of Cocktail Selector

### Why Cocktail Selector?

Choosing the right drink from countless options can be overwhelming. The
Cocktail Selector is here to make that choice easier. Whether you’re
hosting a party, exploring the world of mixology, or simply feeling
adventurous, our package provides a user-friendly interface to explore
and discover up to 600 drink recipes that match your tastes.

### External Functions:

- `cocktailApp`: Launches the Shiny app, providing an interactive
  platform for exploring cocktail recipes.
- `convertUnits`: A versatile function to convert measurement units
  (volume, weight, length) between imperial and international systems.
- `filterCocktails`: Filters the cocktail list based on chosen alcohol
  and ingredients, offering personalized drink suggestions.
- `ingredientsTable`: Generates a table displaying ingredients and
  quantities for selected cocktails.
- `surpriseMe`: Displays a random cocktail from the list of options,
  indicating the ingresients, measurements and recipe.

### Internal Functions:

- `renderSideIngredientUI`: This internal function uses shiny code to
  dynamically render the 3 dropdowns to select ingredients
- `updateSideIngredient`: Another internal function which updates the
  available ingredients in the dropdowns based on the ingredients
  already selected.
- `welcomeMessage`: Displays an engaging welcome message to users when
  the app starts, enhancing the overall user experience.

## Installation

You can install the development version of CocktailSelector from Github
with:

``` r
#install.packages("devtools")
devtools::install_github("ptds2023/Project_Group_D")
```

## Dependencies

The package depends on the `dplyr`, `tidyr`, `kableExtra`, `shiny`,
`magrittr`, `htmltools`, `stringr`, `bslib` packages, which are
automatically imported and loaded when installing `CocktailSelector`.

It is also suggested to load the `testthat (>= 3.0.0)`, `knitr`,
`rmarkdown` packages.

## Examples

Below we show a couple of use cases for standalone functions, and also
how to launch the shiny app directly from the package to have a better
interactive user experience.

You can refer to the vignettes in the articles tab of the website to
learn more about how to use the app in detail and how to use the
functions in a standalone manner.

### Standalone functions

This is how you may load the package:

``` r
# Loading package
library(CocktailSelector)
```

In the following sections, you may find the function that changes the
unit from imperial to international system, or from international to
imperial. The output is the dataset with the transformed measurements
and corresponding chosen units.

``` r
library(dplyr)
# Example dataframe with cocktail ingredients and their measurements
cocktail_data <- cocktails[1:4, 1:6] %>% 
  mutate(units = c("1 inch", "2.5 lb", "3 cm", "0.25 oz"))

# Converting from imperial (ounces) to international (milliliters) units
converted_data_international <- convertUnits(cocktail_data, "imperial_to_international")

# Displaying the converted data (only columns 1 and 7 to show name and units converted)
print(converted_data_international[, c(1, 7)])
#>                                   Name  units
#> 1 '57 chevy with a white license plate 2.5 cm
#> 2                   900 1-900-fuk-meup 1.1 kg
#> 3                     110 in the shade   3 cm
#> 4               151 florida bushwacker  10 ml
```

This example demonstrates the conversion of ingredient quantities from
imperial to international.Adjust the dataframe to match your specific
needs and explore the flexibility of the CocktailSelector package.

Another example would be to use the `filterCocktails` function to see
which cocktails contain gin and cranberry juice.

``` r
#using filterCocktails function
df <- filterCocktails(cocktails, "vodka", "cranberry juice")
#showing head of resulting dataframe
head(df %>% select(Name, vodka, `cranberry juice`))
#>                     Name   vodka cranberry juice
#> 16   a gilligan's island    1 oz            3 oz
#> 38   absolutely fabulous  1 shot         2 shots
#> 168        bruised heart  0.5 oz          0.5 oz
#> 201 cosmopolitan martini    1 oz        1 splash
#> 398         pink penocha 1750 ml           1 gal
#> 401        popped cherry    2 oz            4 oz
```

### Launch Shiny App

In order to use the package a bit more interactively and find your new
favourite cocktails even faster, you can use a shiny app we created
directly from the package by using the code below:

``` r
#launch shiny app
cocktailApp()
```
