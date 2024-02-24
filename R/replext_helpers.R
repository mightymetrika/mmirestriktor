#' Retrieve Cell Blocks for Replext Simulation
#'
#' This internal helper function provides a list of simulation types available in
#' the Replext Shiny application. It maps the user-friendly names of the
#' simulations to their corresponding function names.
#'
#' @return A named list where each name corresponds to a user-friendly description
#'   of a simulation type and each value is the function name that executes the
#'   simulation.
#'
#' @details The function currently supports 'Order Constrained ANOVA' and
#'   'Inequality Constrained Linear Regression' simulations, corresponding to
#'   `replext_t1_c1` and `replext_t2_c1` functions, respectively. This function is
#'   used to populate the selection options in the Shiny application's UI.
#'
#' @keywords internal
getCellBlocks <- function(){
  list("Order Constrained ANOVA" = "replext_t1_c1",
       "Inequality Constrained Linear Regression" = "replext_t2_c1")
}

#' Generate UI Parameters for Selected Simulation Type
#'
#' This internal helper function dynamically generates UI elements for setting
#' parameters based on the chosen simulation type in the Replext Shiny application.
#'
#' @param cellBlock A string identifier for the selected simulation type.
#'   Currently supports 'replext_t1_c1' for Order Constrained ANOVA and
#'   'replext_t2_c1' for Inequality Constrained Linear Regression simulations.
#'
#' @return A list of Shiny UI elements appropriate for setting parameters for the
#'   selected simulation type.
#'
#' @details The function uses a switch statement to determine which set of UI
#'   elements to generate based on the `cellBlock` parameter. Each set of UI
#'   elements corresponds to the input parameters required for either the ANOVA
#'   or regression simulations. These UI elements include numeric inputs, text
#'   inputs, and checkbox inputs for various simulation parameters such as number
#'   of iterations, number of groups or variables, effect sizes, constraints, etc.
#'
#' @keywords internal
getUIParams <- function(cellBlock) {
  switch(cellBlock,
         "replext_t1_c1" = list(shiny::numericInput("S", "Number of simulation iterations:", 10),
                                shiny::numericInput("k", "Number of groups:", 3),
                                shiny::textInput("fs", "Vector of population mean differences:", "0.10"),
                                shiny::numericInput("n_start", "Starting sample size per group:", 6),
                                shiny::textInput("constrs", "Vector of correctly specified order constraints:", "0,1,2"),
                                shiny::numericInput("alpha", "Alpha level:", 0.05),
                                shiny::textInput("pow", "Statistical power:", 0.80),
                                shiny::numericInput("nmax", "Maximum sample size per group:", 1000)),
         "replext_t2_c1" = list(shiny::numericInput("S", "Number of simulation iterations:", 10),
                                shiny::numericInput("p", "Number of variables:", 3),
                                shiny::textInput("f2s", "Vector of effect sizes:", "0.02"),
                                shiny::numericInput("n_start", "Starting sample size per group:", 6),
                                shiny::textInput("constrs", "Vector of correctly specified inequality:", "0,1,2,3"),
                                shiny::numericInput("rho", "Correlation among independent variables:", 0.0),
                                shiny::textInput("beta", "Vector of regression coefficients:", "0.1"),
                                shiny::numericInput("alpha", "Alpha level:", 0.05),
                                shiny::textInput("pow", "Statistical power:", 0.80),
                                shiny::checkboxInput("standardize", "Standardize variables", value = TRUE),
                                shiny::numericInput("nmax", "Maximum sample size per group:", 1000))

  )
}

# getUIParams <- function(cellBlock) {
#   switch(cellBlock,
#          "replext_t1_c1" = list(shiny::numericInput("S", "Number of simulation iterations:", 20000),
#                                   shiny::numericInput("k", "Number of groups:", 3),
#                                   shiny::textInput("fs", "Vector of population mean differences:", "0.10,0.15,0.20,0.25,0.30,0.35,0.40"),
#                                   shiny::numericInput("n_start", "Starting sample size per group:", 6),
#                                   shiny::textInput("constrs", "Vector of correctly specified order constraints:", "0,1,2"),
#                                   shiny::numericInput("alpha", "Alpha level:", 0.05),
#                                   shiny::textInput("pow", "Statistical power:", 0.80),
#                                   shiny::numericInput("nmax", "Maximum sample size per group:", 1000)),
#          "replext_t2_c1" = list(shiny::numericInput("S", "Number of simulation iterations:", 20000),
#                                 shiny::numericInput("p", "Number of variables:", 3),
#                                 shiny::textInput("f2s", "Vector of effect sizes:", "0.02,0.05,0.08,0.10,0.15,0.20,0.25,0.35"),
#                                 shiny::numericInput("n_start", "Starting sample size per group:", 6),
#                                 shiny::textInput("constrs", "Vector of correctly specified inequality:", "0,1,2,3"),
#                                 shiny::numericInput("rho", "Correlation among independent variables:", 0.0),
#                                 shiny::textInput("beta", "Vector of regression coefficients:", "0.1"),
#                                 shiny::numericInput("alpha", "Alpha level:", 0.05),
#                                 shiny::textInput("pow", "Statistical power:", 0.80),
#                                 shiny::checkboxInput("standardize", "Standardize variables", value = TRUE),
#                                 shiny::numericInput("nmax", "Maximum sample size per group:", 1000))
#
#   )
# }

#' Append Input Parameters to Simulation Results
#'
#' This internal helper function appends the user-specified input parameters to
#' each row of the simulation results data frame. This function is used to provide
#' a complete record of the parameters used for each simulation run.
#'
#' @param df A data frame containing the simulation results.
#' @param input A list containing the user input parameters from the Shiny application.
#'
#' @return A data frame combining the original simulation results with a row-wise
#'   repetition of the input parameters for each simulation run.
#'
#' @details The function first generates a unique identifier for each simulation
#'   run. It then constructs a data frame of the input parameters based on the
#'   selected simulation type (either 'replext_t1_' or 'replext_t2_'). The input
#'   parameters are repeated to match the number of rows in the results data frame
#'   and then appended to it. This augmented data frame provides a comprehensive
#'   view of the results and the parameters used to generate them.
#'
#' @keywords internal
appendInputParams <- function(df, input) {
  # Generate a unique code for the simulation run
  run_code <- paste(sample(letters, 10, replace = TRUE), collapse = "")

  print(paste0("aIP: ", input$constrs))
  print(paste0("aIP: ", text_to_vector(input$constrs)))

  # Create a data frame of input parameters
  if (grepl("^replext_t1_", input$cellBlock)) {
    params_df <- data.frame(
      S = input$S, k = input$k, fs = text_to_vector(input$fs),
      n_start = input$n_start, constrs = text_to_vector(input$constrs),
      alpha = input$alpha, pow = input$pow, nmax = input$nmax, Seed = input$seed,
      RunCode = run_code, stringsAsFactors = FALSE
    )
  } else if (grepl("^replext_t2_", input$cellBlock)) {
    params_df <- data.frame(
      S = input$S, p = input$p, f2s = text_to_vector(input$f2s),
      n_start = input$n_start, constrs = text_to_vector(input$constrs),
      rho = input$rho, beta = text_to_vector(input$beta), alpha = input$alpha,
      pow = input$pow, standardize = input$standardize, nmax = input$nmax,
      Seed = input$seed, RunCode = run_code, stringsAsFactors = FALSE
    )
  } else {
    stop("Must select a supported cell block")
  }

  # Repeat the parameters data frame to match the number of rows in df
  params_df <- params_df[rep(1, nrow(df)), ]

  # Combine with the simulation results
  cbind(df, params_df)
}

#' Execute Simulation Based on User Input
#'
#' This internal helper function runs the appropriate simulation function based
#' on the user-selected cell block in the Replext Shiny application. It handles
#' the dynamic execution of either ANOVA or regression simulations based on the
#' parameters specified by the user.
#'
#' @param input A list containing the user input parameters from the Shiny application.
#'
#' @return The result of the simulation, which can be either from `replext_t1_c1`
#'   (Order Constrained ANOVA) or `replext_t2_c1` (Inequality Constrained Linear
#'   Regression), depending on the selected cell block.
#'
#' @details The function first checks and sets the random number seed if provided.
#'   It then determines which simulation function to call (`replext_t1_c1` or
#'   `replext_t2_c1`) based on the prefix of the `cellBlock` input parameter.
#'   The function passes user-specified parameters to the chosen simulation
#'   function and returns the simulation results.
#'
#' @keywords internal
runSimulation <- function(input) {

  # Set the seed if provided
  if (!is.na(input$seed) && input$seed > 0) {
    set.seed(input$seed)
  }

  # Dynamically call the appropriate function based on the cell block prefix
  if (grepl("^replext_t1_", input$cellBlock)) {
    return(replext_t1_c1(S = input$S, k = input$k, fs = text_to_vector(input$fs),
                         n_start = input$n_start, constrs = text_to_vector(input$constrs),
                         alpha = input$alpha, pow = input$pow, nmax = input$nmax
    ))
  } else if (grepl("^replext_t2_", input$cellBlock)) {
    return(replext_t2_c1(S = input$S, p = input$p, f2s = text_to_vector(input$f2s),
                         n_start = input$n_start, constrs = text_to_vector(input$constrs),
                         rho = input$rho, beta = text_to_vector(input$beta),
                         alpha = input$alpha, pow = input$pow,
                         standardize = input$standardize, nmax = input$nmax
    ))
  } else {
    stop("Must select a supported cell block")
  }

}

#' Convert Comma-Separated String to Numeric Vector
#'
#' This internal function takes a string of comma-separated values and
#' converts it into a numeric vector. It is typically used to process
#' user inputs from the Shiny app's UI where multiple values can be
#' entered as a single string.
#'
#' @param text_input A string containing comma-separated values,
#'        typically a user input from the Shiny app's UI.
#'
#' @return A numeric vector converted from the comma-separated string.
#'         If the input is an empty string, returns an empty numeric vector.
#'
#' @keywords internal
text_to_vector <- function(text_input) {
  as.numeric(unlist(strsplit(text_input, ",")))
}
