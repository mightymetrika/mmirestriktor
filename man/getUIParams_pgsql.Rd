% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/replext_helpers_pgsql.R
\name{getUIParams_pgsql}
\alias{getUIParams_pgsql}
\title{Generate UI Parameters for Selected Simulation Type (PostgreSQL Integration)}
\usage{
getUIParams_pgsql(cellBlock)
}
\arguments{
\item{cellBlock}{A string identifier for the selected simulation type.
Currently supports 'replext_t1_c1' for Order Constrained ANOVA and
'replext_t2_c1' for Inequality Constrained Linear Regression simulations.}
}
\value{
A list of Shiny UI elements appropriate for setting parameters for the
selected simulation type.
}
\description{
This internal helper function dynamically generates UI elements for setting
parameters based on the chosen simulation type in the Replext Shiny application.
The *_pgsql version of the function was built for integration with PostgreSQL
}
\details{
The function uses a switch statement to determine which set of UI
elements to generate based on the \code{cellBlock} parameter. Each set of UI
elements corresponds to the input parameters required for either the ANOVA
or regression simulations. These UI elements include numeric inputs, text
inputs, and checkbox inputs for various simulation parameters such as number
of iterations, number of groups or variables, effect sizes, constraints, etc.
}
\keyword{internal}
