% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/updateSideIngredients.R
\name{updateSideIngredients}
\alias{updateSideIngredients}
\title{Update side ingredients content}
\usage{
updateSideIngredients(input, session, cocktails)
}
\arguments{
\item{input}{Shiny input vector from current session}

\item{session}{Shiny output vector from current session}

\item{cocktails}{Cocktails dataframe from the package}
}
\value{
Updated dropdown menus
}
\description{
This internal function dynamically updates the content inside the side ingredient dropdowns
based on what alcohol and/or side ingredient 1 were selected. It also prevents a user from being able to choose the
same side ingredient twice.
}
